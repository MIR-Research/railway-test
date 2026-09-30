library(shiny)
library(dplyr)
library(DT)

# UI function for error metrics module
errorMetricsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    radioButtons(
      inputId = ns("sort_method"),
      label = "Sort by",
      choices = c("Stratification" = "strat", "Best R²" = "best"),
      selected = "strat",
      inline = TRUE
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'strat'", ns("sort_method")),
      selectInput(
        inputId = ns("strat_select"),
        label = "Choose Stratification Type",
        choices = c("Order", "Global", "Texture", "Depth", "MLRA", "LULC"),
        selected = "Order"
      )
    ),
    DTOutput(ns("error_table"))
  )
}

# ---- Loading the metric CSVs (shared by all sessions) -----------------------
# These live outside errorMetricsServer so every user session shares one
# cached copy instead of each session re-reading every CSV from the bucket.

# Bucket prefixes (these are object-key prefixes in the bucket,
# NOT local paths). They mirror the mapping used in model_selection_main.R.
error_metric_prefixes <- list(
  "Order"   = "models/Orders",
  "Global"  = "models/Global",
  "Texture" = "models/Texture_classes",
  "Depth"   = "models/Depths",
  "MLRA"    = "models/MLRA",
  "LULC"    = "models/LULC"
)

uncertainty_prefixes <- list(
  "Order"   = "Uncertainty_vals/Orders",
  "Global"  = "Uncertainty_vals/Global",
  "Texture" = "Uncertainty_vals/Texture_classes",
  "Depth"   = "Uncertainty_vals/Depths",
  "MLRA"    = "Uncertainty_vals/MLRA",
  "LULC"    = "Uncertainty_vals/LULC"
)

error_metric_ml_models <- c("Cubist", "PLS", "RF", "SVM", "CNN")

rename_strat_column <- function(df, category) {
  if (category == "Global") {
    df$strat <- "Global"
  } else if (category %in% names(df)) {
    names(df)[names(df) == category] <- "strat"
  } else {
    df$strat <- NA_character_
  }

  df$strat <- as.character(df$strat)
  df
}

# Reads the error-metric CSVs from the bucket.
#
# For every stratification category and ML model we build the bucket
# prefix (e.g. "models/Orders/RF"), list the objects under it, keep the
# .csv keys, and read each one straight out of the bucket. This replaces
# the old local-filesystem approach (dir.exists / list.files / read.csv)
# that only worked when the model folders were present on disk.
collect_error_csvs <- function(mapping, include_uncertainty = FALSE) {
  all_dfs <- list()

  for (category in names(mapping)) {
    base_dir <- mapping[[category]]

    for (ml_model in error_metric_ml_models) {
      # Bucket-key prefix for this category + model, using the same
      # helper model_selection_main.R uses to build object keys.
      prefix <- bucket_key(base_dir, ml_model)

      # List everything under the prefix. If the prefix doesn't exist
      # (or listing fails), treat it as empty and move on.
      keys <- tryCatch(
        list_model_keys(prefix),
        error = function(e) character(0)
      )
      if (length(keys) == 0) next

      # Keep only CSV objects.
      csv_files <- keys[grepl("\\.csv$", basename(keys), ignore.case = TRUE)]
      if (length(csv_files) == 0) next

      # Split uncertainty vs. regular error files by filename.
      if (include_uncertainty) {
        # Previously loaded both the summary files and the run-level files
        # (one row per training run). The run-level rows were then dropped in
        # compileAllUncertaintySD(), so we now skip downloading them. To load
        # them again (e.g. to show per-run values), swap these two lines back.
        # csv_files <- csv_files[grepl("uncertainty", basename(csv_files), ignore.case = TRUE)]
        csv_files <- csv_files[grepl("uncertainty", basename(csv_files), ignore.case = TRUE) &
                               grepl("summary", basename(csv_files), ignore.case = TRUE)]
      } else {
        csv_files <- csv_files[!grepl("uncertainty", basename(csv_files), ignore.case = TRUE)]
      }

      if (length(csv_files) == 0) next

      for (f in csv_files) {
        # Read the CSV directly from the bucket. read_bucket_delim is the
        # same reader main.R uses for the Full_DFs / spectral_data objects.
        df <- tryCatch(
          as.data.frame(read_bucket_delim(f, delim = ",")),
          error = function(e) NULL
        )
        if (is.null(df) || nrow(df) == 0) next

        df$ModelType  <- category
        df$ML_Model   <- ml_model
        df$SourceFile <- basename(f)
        df <- rename_strat_column(df, category)

        if (include_uncertainty) {
          df$FileKind <- if (grepl("summary", basename(f), ignore.case = TRUE)) {
            "Summary Matrix"
          } else {
            "Run-level Values"
          }
        }

        all_dfs[[length(all_dfs) + 1]] <- df
      }
    }
  }

  if (length(all_dfs) == 0) data.frame() else dplyr::bind_rows(all_dfs)
}

# Cache the combined CSVs in memory for all sessions. Entries expire after
# ERROR_METRICS_CACHE_HOURS (default 12) so newly uploaded metrics appear
# without a redeploy. An empty result raises an error instead of returning,
# because memoise doesn't cache errors: if the bucket was unreachable, the
# next person to open the modal retries rather than getting a cached blank.
error_metrics_cache_hours <- as.numeric(Sys.getenv("ERROR_METRICS_CACHE_HOURS", "12"))

collect_error_csvs_cached <- memoise::memoise(function(mapping, include_uncertainty) {
  df <- collect_error_csvs(mapping, include_uncertainty)
  if (nrow(df) == 0) stop("no metric CSVs could be read from the bucket")
  df
}, cache = cachem::cache_mem(max_age = error_metrics_cache_hours * 3600))

collect_error_csvs_shared <- function(mapping, include_uncertainty = FALSE) {
  tryCatch(
    collect_error_csvs_cached(mapping, include_uncertainty),
    error = function(e) {
      message("Error metrics load failed | ", conditionMessage(e))
      data.frame()
    }
  )
}

# Server function for error metrics module
errorMetricsServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {

    format_metric <- function(value, sd) {
      ifelse(
        is.na(value),
        NA_character_,
        ifelse(
          is.na(sd),
          sprintf("%.3f", value),
          sprintf("%.3f ± %.3f", value, sd)
        )
      )
    }
    
    compileAllErrors <- reactive({
      df <- collect_error_csvs_shared(error_metric_prefixes, include_uncertainty = FALSE)
      if (nrow(df) == 0) return(df)
      
      req_cols <- c("filename", "R2.val", "RMSE.val", "RPIQ.val", "RPD.val")
      for (col in req_cols) {
        if (!(col %in% names(df))) df[[col]] <- NA
      }
      
      metric_cols <- c("R2.val", "RMSE.val", "RPIQ.val", "RPD.val")
      for (col in metric_cols) {
        df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
      }
      
      keep_cols <- c("filename", "ML_Model", "ModelType", "strat",
                     "R2.val", "RMSE.val", "RPIQ.val", "RPD.val")
      
      df[, keep_cols, drop = FALSE]
    })
    
    compileAllUncertaintySD <- reactive({
      df <- collect_error_csvs_shared(uncertainty_prefixes, include_uncertainty = TRUE)
      if (nrow(df) == 0) return(df)
      
      df <- df[df$FileKind == "Summary Matrix", , drop = FALSE]
      if (!("stat" %in% names(df))) return(data.frame())
      
      df <- df[tolower(trimws(as.character(df$stat))) == "sd", , drop = FALSE]
      if (nrow(df) == 0) return(df)
      
      req_cols <- c("filename", "ML_Model", "ModelType", "strat",
                    "R2.val", "RMSE.val", "RPIQ.val", "RPD.val")
      for (col in req_cols) {
        if (!(col %in% names(df))) df[[col]] <- NA
      }
      
      keep_cols <- c("filename", "ML_Model", "ModelType", "strat",
                     "R2.val", "RMSE.val", "RPIQ.val", "RPD.val")
      df <- df[, keep_cols, drop = FALSE]
      
      metric_cols <- c("R2.val", "RMSE.val", "RPIQ.val", "RPD.val")
      for (col in metric_cols) {
        df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
      }
      
      names(df)[names(df) == "R2.val"]   <- "R2.sd"
      names(df)[names(df) == "RMSE.val"] <- "RMSE.sd"
      names(df)[names(df) == "RPIQ.val"] <- "RPIQ.sd"
      names(df)[names(df) == "RPD.val"]  <- "RPD.sd"
      
      df
    })
    
    combinedData <- reactive({
      err_df <- compileAllErrors()
      if (nrow(err_df) == 0) return(err_df)
      
      sd_df <- compileAllUncertaintySD()
      if (nrow(sd_df) == 0) {
        err_df$R2.sd   <- NA_real_
        err_df$RMSE.sd <- NA_real_
        err_df$RPIQ.sd <- NA_real_
        err_df$RPD.sd  <- NA_real_
        return(err_df)
      }
      
      left_join(
        err_df,
        sd_df,
        by = c("filename", "ML_Model", "ModelType", "strat")
      )
    })
    
    filteredData <- reactive({
      df <- combinedData()
      if (nrow(df) == 0) return(df)
      
      prop <- shared$selectedProperty
      if (!is.null(prop) && prop != "") {
        df <- df[grepl(prop, df$filename, ignore.case = TRUE), ]
      }
      
      df
    })
    
    displayedData <- reactive({
      df <- filteredData()
      if (nrow(df) == 0) return(df)
      
      if (is.null(input$sort_method)) input$sort_method <- "best"
      
      if (input$sort_method == "best") {
        df <- arrange(df, desc(R2.val))
      } else if (input$sort_method == "strat") {
        if (!is.null(input$strat_select) && input$strat_select != "") {
          if (input$strat_select == "Global") {
            df <- df[df$ModelType == "Global", ]
          } else {
            df <- df[df$ModelType == input$strat_select, ]
          }
        }
        
        df <- arrange(df, ML_Model)
      }
      
      out <- data.frame(
        Filename   = df$filename,
        ML_Model   = df$ML_Model,
        ModelType  = df$ModelType,
        Strat      = df$strat,
        `R² ± SD`  = format_metric(df$R2.val, df$R2.sd),
        `RMSE ± SD`= format_metric(df$RMSE.val, df$RMSE.sd),
        `RPIQ ± SD`= format_metric(df$RPIQ.val, df$RPIQ.sd),
        `RPD ± SD` = format_metric(df$RPD.val, df$RPD.sd),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      if (input$sort_method == "strat") {
        out <- out[, colnames(out) != "Filename", drop = FALSE]
      }
      
      out
    })
    
    output$error_table <- renderDT({
      df <- displayedData()
      if (nrow(df) == 0) {
        return(datatable(
          data.frame(Message = "No data found"),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }
      
      datatable(
        df,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    }, server = FALSE)
  })
}