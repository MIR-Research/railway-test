# data_aggregation_main.R
library(shiny)
library(shinyWidgets)
library(opusreader2)  
library(prospectr)    

options(max.print = 10000)

dataAggregationUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      verticalLayout(
        card(
          card_header("Data Preprocessing"),
          card_body(
            h2("Instructions:"),
            tags$ol(
              tags$li("Compile your .0 or .csv files with spectral data into a folder"),
              tags$li("Choose which type of files you intend on using"),
              tags$li("Compress the folder containing these files into .zip format"),
              tags$li("Upload the .zip file, and download preprocessed data"),
              tags$li("To process data for use in pretrained models, click 'Default' button in the custom preprocessing option"),
              tags$li("If you want more control over preprocessing steps, click on the 'Custom' button in the  custom Preprocessing Options box that will allow you to change the preprocessing parameters"),
              tags$li(
                "For more info, see ",
                tags$a("User Guide",
                       href    = "#",
                       style = "color: #0000EE",
                       onclick = sprintf(
                         "Shiny.setInputValue('%s', Math.random()); return false;", 
                         ns("goto_user_guide")
                       )
                )
              )
            ) 
          )
        ),
        card(
          card_header = "File Upload",
          card_body(
            layout_column_wrap(
              gap = "50px",
              radioGroupButtons(
                inputId = ns("upload_type"),
                label = "Select Data Upload Type:",
                choices = c("Raw Data (ZIP)" = "raw", "Aggregated Data (CSV)" = "partial"),
                selected = "raw"
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'raw'", ns("upload_type")),
                radioButtons(
                  inputId = ns("file_type"),
                  label = "Select File Type:",
                  choices = c("Opus (.0)" = "opus", "CSV" = "csv"),
                  selected = "opus"
                )
              )
            ),
            # For raw ZIP uploads:
            conditionalPanel(
              condition = sprintf("input['%s'] == 'raw'", ns("upload_type")),
              layout_column_wrap(
                fileInput(
                  ns("data_zip"),
                  "Upload a Zip File Containing CSV and/or .0 Files",
                  multiple = FALSE,
                  accept = ".zip"
                ),
                checkboxInput(
                  ns("use_outlier"),
                  label = "Enable outlier detection (96% similarity cutoff)",
                  value = FALSE
                ))
            ),
            # For partially preprocessed CSV uploads:
            conditionalPanel(
              condition = sprintf("input['%s'] == 'partial'", ns("upload_type")),
              layout_column_wrap(
                fileInput(
                  ns("partial_data"),
                  "Upload Preprocessed CSV File",
                  multiple = FALSE,
                  accept = ".csv"
                ),
                checkboxInput(
                  ns("use_outlier"),
                  label = "Enable outlier detection (96% similarity cutoff)",
                  value = FALSE
                ))
              
            ),
            
            textOutput(ns("processing_status")),
            verbatimTextOutput(ns("error_message"))
          )
        ),
        card(
          card_header("Custom Preprocessing Options"),
          card_body(
            layout_column_wrap(
              checkboxInput(
                ns("aggregated"),
                label = "Aggregate Data",
                value = TRUE
              ),
              radioButtons(
                inputId = ns("partial"),
                choices = c("Default", "Custom"),
                selected = "Default",
                label = ""
              )),
            
            checkboxInput(
              ns("filtered"),
              label = "Filter Out Negative Values (Recommended)",
              value = TRUE
            ),
            checkboxInput(
              ns("averaged"),
              "Average Replicates",
              value = TRUE
            ),
            layout_column_wrap(
              checkboxInput(
                ns("sg"),
                label = "Savitzky-Golay Smoothing",
                value = TRUE
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("sg")),
                selectInput(
                  inputId = ns("m"),
                  label = "Derivative Order (m)",
                  choices = c(0, 1, 2),
                  selected = 0
                ),
                selectInput(
                  inputId = ns("w"),
                  label = "Window Size (w)",
                  choices = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19),
                  selected = 13
                ),
                selectInput(
                  inputId = ns("p"),
                  label = "Polynomial Order (p)",
                  choices = c(1, 2, 3, 4, 5),
                  selected = 2
                ))
            ),
            layout_column_wrap(
              checkboxInput(
                ns("resampled"),
                label = "Resample Data",
                value = TRUE
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("resampled")),
                sliderInput(
                  inputId = ns("resample_val"),
                  label = "Resampling Interval (cm⁻¹)",
                  min = 1,
                  max = 100,
                  value = 10,
                  step = 1
                )
              )),
            
            checkboxInput(
              ns("snv"),
              label = "Standard Normal Variate/Baseline Correction",
              value = TRUE
            ),
            actionButton(ns("start"), "Start Preprocessing"),
            downloadButton(ns("step_data"), "Download")
          )
        )
        
      ),
      conditionalPanel(
        condition = sprintf("output['%s'] !== ''", ns("processing_status")),
        card(
          full_screen = TRUE,
          card_header = "Raw Spectra",
          card_body(
            plotOutput(ns("mir_plot")),
            downloadButton(ns("download_mir_jpg"), "Download Raw JPG")
          )
        ),
        br(),
        card(
          full_screen = TRUE,
          card_header = "MIR + Savitzky-Golay Plot",
          card_body(
            plotOutput(ns("sg_plot")),
            downloadButton(ns("download_sg_jpg"), "Download SG JPG")
          )
        ),
        br(),
        card(
          full_screen = TRUE,
          card_header = "Resample Plot",
          card_body(
            plotOutput(ns("resample_plot")),
            downloadButton(ns("download_resample_jpg"), "Download Resample JPG")
          )
        ),
        br(),
        card(
          full_screen = TRUE,
          card_header = "Standard Normal Variate Plot",
          card_body(
            plotOutput(ns("snv_plot")),
            downloadButton(ns("download_snv_jpg"), "Download SNV JPG")
          )
        )
      )
    )
  )
}


dataAggregationServer <- function(id, shared, session) {
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    status <- reactiveVal("")
    error_message <- reactiveVal("")
    
    log_debug <- function(...) {
      msg <- paste0("[", Sys.time(), "] ", ...)
      message(msg)          # prints to console
    }
    
    
    # Reactively store intermediate data
    rv <- reactiveValues(
      main_data = NULL,
      mir_data = NULL,
      mir_sg = NULL,
      mir_res = NULL,
      mir_snv = NULL,
      custom_res = NULL,
      spectra0 = NULL,
      start_seq_csv = NULL,
      stop_seq_csv = NULL,
      data_in_filtered = NULL,
      mir_res_partial = NULL,
      mir_snv_partial = NULL,
      start_seq_opus = NULL,
      stop_seq_opus = NULL,
      removed_samples = character(0),
      removed_replicates = character(0),
      orig_start_seq_opus = NULL,
      orig_stop_seq_opus = NULL,
      snipped_start_seq_opus = NULL,
      snipped_stop_seq_opus = NULL
    )
    
    observeEvent(input$goto_user_guide, {
      # just flip a shared flag
      shared$goto_user_guide <- Sys.time()
    })
    
    # Allow adjustment of the "Default" vs "Custom" settings
    observe({
      if (input$partial == "Default") {
        shinyjs::disable("aggregated")
        shinyjs::disable("averaged")
        shinyjs::disable("sg")
        shinyjs::disable("resampled")
        shinyjs::disable("filtered")
        shinyjs::disable("m")
        shinyjs::disable("w")
        shinyjs::disable("p")
        shinyjs::disable("resample_val")
        shinyjs::disable("snv")
      } else {
        shinyjs::disable("aggregated")
        shinyjs::enable("averaged")
        shinyjs::enable("sg")
        shinyjs::enable("resampled")
        shinyjs::enable("filtered")
        shinyjs::enable("m")
        shinyjs::enable("w")
        shinyjs::enable("p")
        shinyjs::enable("resample_val")
        shinyjs::enable("snv")
      }
    })
    
    # ---- Helper -------------------------------------------------------------
    # Takes a vector of filenames and returns a data.frame with 2 columns:
    #   * sample_id
    #   * replicate_id  (NA if no replicate information)
    split_sample_replicate <- function(fname_vec) {
      
      # 1️ try the **numeric-suffix** rule  (25783XS03  →  sample = 25783XS, rep = 03)
      numeric_suffix <- grepl("[0-9]+\\.?[0-9]*$", fname_vec)
      sample_id_1    <- sub("([0-9]+\\.?[0-9]*)$", "",  fname_vec)
      replicate_id_1 <- sub(".*?([0-9]+\\.?[0-9]*)$", "\\1", fname_vec)
      
      # 2️ where that failed, look for .0 .1 .2 … extensions  (sample.0  →  sample / rep = 0)
      #     NB: we keep the dot so that "mysample.0.csv" still matches.
      missing <- !numeric_suffix
      has_dot_rep <- grepl("\\.[0-4](\\.[A-Za-z0-9]+)?$", fname_vec)   # .0  .1  .2 … possibly followed by .csv
      use_ext <- missing & has_dot_rep
      
      sample_id_2    <- sub("\\.[0-4](\\.[A-Za-z0-9]+)?$", "", fname_vec[use_ext])
      replicate_id_2 <- sub("^.*\\.", "", fname_vec[use_ext])           # gives "0", "1", …
      
      # 3 fall-back: treat each file as its own sample, replicate = NA
      sample_id_final    <- sample_id_1
      replicate_id_final <- replicate_id_1
      sample_id_final[use_ext]    <- sample_id_2
      replicate_id_final[use_ext] <- replicate_id_2
      replicate_id_final[replicate_id_final == fname_vec] <- NA   # no pattern at all
      
      data.frame(sample_id    = sample_id_final,
                 replicate_id = replicate_id_final,
                 stringsAsFactors = FALSE)
    }
    
    ## ────────── 1.  NEW helper (place it near the top of dataAggregationServer) ──────────
    build_status_msg <- function(done_txt   = "Processing complete!",
                                 removed_s  = rv$removed_samples,
                                 removed_r  = rv$removed_replicates,
                                 use_ol     = isTRUE(input$use_outlier)) {
      
      if (!use_ol)                                  # outlier switch off
        return(done_txt)
      
      if (length(removed_s) == 0 && length(removed_r) == 0)
        return(paste(done_txt, "• No outliers detected."))
      
      parts <- character(0)
      if (length(removed_s) > 0)
        parts <- c(parts,
                   paste0("Removed samples: ",
                          paste(removed_s, collapse = ", ")))
      if (length(removed_r) > 0)
        parts <- c(parts,
                   paste0("Removed replicates: ",
                          paste(removed_r, collapse = ", ")))
      
      paste(done_txt, "•", paste(parts, collapse = "  •  "))
    }
    
    
    #-------------------------------
    # process_directory()
    # For "csv" => read the CSV logic
    # For "opus" => use approach yakun gave
    # This function is robust to errors, pattern types for replicates, and more
    #-------------------------------
    process_directory <- function(dir_path, file_type) {
      status(paste("Processing directory:", basename(dir_path)))
      
      if (file_type == "csv") {
        ## 1.  Find all *.csv in the current sub‑directory
        file_list <- list.files(dir_path, pattern = "(?i)\\.csv$", full.names = TRUE)
        if (length(file_list) == 0) return(NULL)
        
        ## 2.  Read each file ----------------------------------------------------
        data_list <- lapply(file_list, function(file) {
          tryCatch({
            ## ---- read *both* columns: wavenumber + absorbance ----
            ## -- inside the lapply(file_list, …) loop -------------------------------
            vals <- read.csv(file, header = FALSE, sep = ",",
                             col.names = c("wn", "abs"))
            
            if (nrow(vals) < 2 || vals$wn[1] == vals$wn[nrow(vals)]) {
              warning(basename(file), ": skipped (only one wavenumber)")
              return(NULL)                                   # <‑‑ skip this file
            }
            
            # record start / stop only once, and only after the above test
            if (is.null(rv$start_seq_csv)) rv$start_seq_csv <- vals$wn[1]
            if (is.null(rv$stop_seq_csv))  rv$stop_seq_csv  <- vals$wn[nrow(vals)]
            
            t(vals$abs)          # keep transpose just as before
            # Individual scans must be transposed prior to prior to Rbind
            # transpose: one row per file
          }, error = function(e) {
            error_message(
              paste("Error processing file", basename(file), ":", e$message)
            )
            NULL
          })
        })
        
        data_list <- Filter(Negate(is.null), data_list)
        if (length(data_list) == 0) return(NULL)
        
        ## 3.  Combine all rows into one data‑frame ------------------------------
        data_combined <- as.data.frame(do.call(rbind, data_list))
        rv$spectra0   <- data_combined
        colnames(data_combined) <- paste0("V", seq_len(ncol(data_combined)))
        
        ## 4.  Build scan‑path names and return ----------------------------------
        spn <- sub("\\.csv$", "", basename(file_list[seq_len(nrow(data_combined))]))
        cbind(scan_path_name = spn, data_combined)
      } else {
        # OPUS approach 
        file_list <- list.files(
          dir_path,
          pattern = "(?i)\\.[0-9]$",   # .0  .1  .2  .3 ...
          full.names = TRUE
        )
        if (length(file_list) == 0) return(NULL)
        
        # 1) read all .0 OPUS files in this directory at once
        data_list <- tryCatch({
          read_opus(dsn = dir_path)
        }, error = function(e) {
          error_message(paste("Error in read_opus for dir", basename(dir_path), ":", e$message))
          return(NULL)
        })
        if (is.null(data_list) || length(data_list) == 0) return(NULL)
        
        # Get first dataset to define the columns
        dataset0 <- data_list[[1]]
        
        if (!is.null(dataset0$ab$data) &&
            nrow(as.data.frame(dataset0$ab$data)) > 0 &&
            ncol(as.data.frame(dataset0$ab$data)) > 0) {
          spectra0 <- as.data.frame(dataset0$ab$data)
        } else if (!is.null(dataset0$ab_no_atm_comp$data) &&
                   nrow(as.data.frame(dataset0$ab_no_atm_comp$data)) > 0 &&
                   ncol(as.data.frame(dataset0$ab_no_atm_comp$data)) > 0) {
          spectra0 <- as.data.frame(dataset0$ab_no_atm_comp$data)
        } else {
          error_message("No absorbance data found in first OPUS dataset")
          return(NULL)
        }
        
        # Get wavelength values from column names
        wavelengths <- suppressWarnings(as.numeric(colnames(spectra0)))
        if (all(is.na(wavelengths))) {
          error_message("No numeric wavelength columns found in first OPUS dataset")
          return(NULL)
        }
        
        # Store ORIGINAL full OPUS range for metadata
        orig_hi <- max(wavelengths, na.rm = TRUE)
        orig_lo <- min(wavelengths, na.rm = TRUE)
        
        if (is.null(rv$orig_start_seq_opus)) {
          rv$orig_start_seq_opus <- orig_hi
        } else {
          rv$orig_start_seq_opus <- max(rv$orig_start_seq_opus, orig_hi, na.rm = TRUE)
        }
        
        if (is.null(rv$orig_stop_seq_opus)) {
          rv$orig_stop_seq_opus <- orig_lo
        } else {
          rv$orig_stop_seq_opus <- min(rv$orig_stop_seq_opus, orig_lo, na.rm = TRUE)
        }
        
        # Keep FULL wavelength range for now; clip later in fil_df()
        rv$start_seq_opus <- orig_hi
        rv$stop_seq_opus  <- orig_lo
        
        # Initialize full matrix
        data_out <- matrix(NA_real_, nrow = length(data_list), ncol = length(wavelengths))
        
        # Assign wavelength column names
        colnames(data_out) <- wavelengths
        data_out <- as.data.frame(data_out, check.names = FALSE)
        
        scan_path_name <- character(length(data_list))
        
        for (i in seq_along(data_list)) {
          dataset <- data_list[[i]]
          
          if (!is.null(dataset$ab$data) &&
              nrow(as.data.frame(dataset$ab$data)) > 0 &&
              ncol(as.data.frame(dataset$ab$data)) > 0) {
            spectra_df <- as.data.frame(dataset$ab$data)
          } else if (!is.null(dataset$ab_no_atm_comp$data) &&
                     nrow(as.data.frame(dataset$ab_no_atm_comp$data)) > 0 &&
                     ncol(as.data.frame(dataset$ab_no_atm_comp$data)) > 0) {
            spectra_df <- as.data.frame(dataset$ab_no_atm_comp$data)
          } else {
            error_message(paste("No absorbance data found in file", i))
            next
          }
          
          # Use first row if multiple rows are present, matching one-row-per-dataset logic
          if (nrow(spectra_df) < 1) {
            next
          }
          
          spectra_values <- as.numeric(spectra_df[1, , drop = TRUE])
          data_out[i, ] <- spectra_values
          
          scan_path_name[i] <- if (!is.null(dataset$basic_metadata$dsn_filename)) {
            dataset$basic_metadata$dsn_filename
          } else {
            paste0("dataset_", i)
          }
        }
        
        colnames(data_out) <- paste0("V", seq_len(ncol(data_out)))
        dat <- cbind(scan_path_name = scan_path_name, data_out)
      }
    }
    
    
    raw_data <- reactive({
      req(input$upload_type)
      
      if (input$upload_type == "raw") {
        status("")
        req(input$data_zip)
        log_debug("– handling raw ZIP:", input$data_zip$name)
        
        rv$main_data <- NULL
        rv$mir_data  <- NULL
        rv$mir_sg    <- NULL
        rv$mir_res   <- NULL
        rv$mir_snv   <- NULL
        rv$data_in_filtered <- NULL
        rv$orig_start_seq_opus   <- NULL
        rv$orig_stop_seq_opus    <- NULL
        rv$snipped_start_seq_opus <- NULL
        rv$snipped_stop_seq_opus  <- NULL
        
        # Unique temp dir
        temp_dir <- file.path(tempdir(), paste0("upload_", format(Sys.time(), "%Y%m%d%H%M%S")))
        dir.create(temp_dir)
        
        withProgress(message = "Reading archive and building table…", value = 0, {
          incProgress(0.05, detail = "Unzipping…")
          tryCatch({
            unzip(input$data_zip$datapath, exdir = temp_dir)
            
            # Gather root + subdirs
            all_dirs <- unique(c(temp_dir, list.dirs(temp_dir, recursive = TRUE)))
            if (length(all_dirs) == 0) {
              status("No valid directories found in zip file")
              return()
            }
            
            all_data <- list()
            n <- length(all_dirs)
            # Reserve 90% of the progress bar for per-dir parsing
            for (i in seq_along(all_dirs)) {
              incProgress(0.90 / n, detail = sprintf("Parsing folder %d/%d: %s",
                                                     i, n, basename(all_dirs[[i]])))
              result <- process_directory(all_dirs[[i]], input$file_type)
              if (!is.null(result)) {
                all_data[[length(all_data) + 1]] <- result
              }
            }
            
            incProgress(0.03, detail = "Finalizing…")
            
            if (length(all_data) > 0) {
              rv$main_data <- do.call(rbind, all_data)
              shared$main_data <- rv$main_data
            } else {
              rv$main_data <- NULL
              shared$main_data <- NULL
              status("No valid files found")
            }
          }, error = function(e) {
            error_message(paste("Error processing files:", e$message))
          }, finally = {
            unlink(temp_dir, recursive = TRUE)
            incProgress(0.02, detail = "Cleanup complete")
          })
        })
        
        df <- rv$main_data
        log_debug("– raw_data returning", nrow(df), "rows ×", ncol(df), "cols")
        return(df)
        
      } else {
        status("")
        req(input$partial_data)
        log_debug("– reading aggregated CSV:", input$partial_data$name)
        
        # quick progress for CSV path
        withProgress(message = "Loading aggregated CSV…", value = 0, {
          incProgress(0.4, detail = "Reading file…")
          df <- read.csv(input$partial_data$datapath, check.names = FALSE)
          incProgress(0.4, detail = "Validating columns…")
          
          wns <- as.numeric(colnames(df)[-1])
          if (any(is.na(wns))) stop("CSV column names must all be numeric wavenumbers")
          rv$start_seq_csv <- min(wns)
          rv$stop_seq_csv  <- max(wns)
          
          incProgress(0.2, detail = "Done")
          df
        })
      }
    })
    
    
    # update df's and debug statements
    observeEvent(input$start, {
      if (!is.null(input$data_zip)) {
        log_debug("⚙️ fileInput$data_zip changed:", input$data_zip$name)
        df <- raw_data()
        log_debug("→ raw_data() returned", nrow(df), "rows ×", ncol(df), "cols")
      } else{
        return()
      }
    })
    
    observeEvent(input$start, {
      if (!is.null(input$partial_data)) {
        log_debug("⚙️ fileInput$partial_data changed:", input$partial_data$name)
        df <- raw_data()
        log_debug("→ raw_data() returned", nrow(df), "rows ×", ncol(df), "cols")
      } else{
        return()
      }
    })
    
    
    # 2. cfg(): centralize “Default” vs “Custom” settings
    cfg <- reactive({
      # build a baseline list depending on Default vs Custom
      if (input$partial == "Default") {
        cfg0 <- list(
          filtered     = TRUE,
          averaged     = TRUE,
          sg           = TRUE,
          m            = 0,
          w            = 13,
          p            = 2,
          resampled    = TRUE,
          resample_val = 10,
          snv          = TRUE  
        )
      } else {
        cfg0 <- list(
          filtered     = input$filtered,
          averaged     = input$averaged,
          sg           = input$sg,
          m            = as.integer(input$m),
          w            = as.integer(input$w),
          p            = as.integer(input$p),
          resampled    = input$resampled,
          resample_val = as.numeric(input$resample_val),
          snv          = isTRUE(input$snv)
        )
      }
      
      # now tack on the outlier‐detection flag
      cfg0$use_outlier <- isTRUE(input$use_outlier)
      
      # and return
      cfg0
    })
    
    
    # 3. agg_df(): just the “one‐row‐per‐file” table
    agg_df <- eventReactive(input$start, {
      df <- raw_data()
      req(df)
      df
    })
    
    #4 fil_df(): filter out negative values
    
    fil_df <- eventReactive(input$start, {
      df <- agg_df()
      c  <- cfg()
      
      if (c$use_outlier) {
        
        csv_start <- as.numeric(colnames(df)[2])
        csv_stop  <- as.numeric(colnames(df)[ncol(df)])
        
        out <- filter_spectral_outliers(
          df,
          start_seq = if (input$upload_type == "raw") {
            if (input$file_type == "opus") rv$start_seq_opus else rv$start_seq_csv
          } else {
            csv_start
          },
          stop_seq  = if (input$upload_type == "raw") {
            if (input$file_type == "opus") rv$stop_seq_opus else rv$stop_seq_csv
          } else {
            csv_stop
          },
          threshold = 96,
          frac_cut  = 0.5
        )
        
        rv$removed_samples    <- out$removed_samples
        rv$removed_replicates <- out$removed_replicates
        df <- out$cleaned_df
      } else {
        rv$removed_samples    <- character(0)
        rv$removed_replicates <- character(0)
      }
      
      data_cols <- 2:ncol(df)
      
      # For OPUS, figure out which columns will survive the later snip
      keep_wave_cols <- data_cols
      wn_full <- NULL
      
      if (input$upload_type == "raw" && input$file_type == "opus" && length(data_cols) > 0) {
        wn_full <- seq(
          from = rv$start_seq_opus,
          to   = rv$stop_seq_opus,
          length.out = length(data_cols)
        )
        
        keep_mask <- !is.na(wn_full) & wn_full >= 599 & wn_full <= 4000.99
        if (any(keep_mask)) {
          keep_wave_cols <- data_cols[keep_mask]
        }
      }
      
      # Negative filtering checks only the wavelengths that will remain after snipping
      if (c$filtered && length(keep_wave_cols) > 0) {
        df <- df[
          apply(df[, keep_wave_cols, drop = FALSE], 1,
                function(x) all(x >= 0, na.rm = TRUE)),
          ,
          drop = FALSE
        ]
      }
      
      # Now actually snip the OPUS wavelengths
      if (input$upload_type == "raw" && input$file_type == "opus" && length(data_cols) > 0) {
        if (!is.null(wn_full)) {
          keep_mask <- !is.na(wn_full) & wn_full >= 599 & wn_full <= 4000.99
          
          if (any(keep_mask)) {
            df <- cbind(
              df[, 1, drop = FALSE],
              df[, data_cols[keep_mask], drop = FALSE]
            )
            
            rv$snipped_start_seq_opus <- max(wn_full[keep_mask], na.rm = TRUE)
            rv$snipped_stop_seq_opus  <- min(wn_full[keep_mask], na.rm = TRUE)
            
            # update downstream range to the snipped range
            rv$start_seq_opus <- rv$snipped_start_seq_opus
            rv$stop_seq_opus  <- rv$snipped_stop_seq_opus
          } else {
            log_debug("OPUS post-filter clipping found 0 columns in [599, 4000.99].")
          }
        }
      }
      
      df
    })
    
    # 5. avg_df(): average replicates if requested
    avg_df <- eventReactive(input$start, {
      df <- fil_df()
      c  <- cfg()
      if (!c$averaged) return(df)
      
      id_col <- names(df)[1]                  # usually "scan_path_name"
      ids    <- df[[id_col]]
      
      ## ── 1. parse replicate / sample the same way everywhere ──────────────
      has_dot   <- grepl("\\.[0-9]+$", ids)
      ext_digit <- ifelse(has_dot, sub("^.*\\.([0-9]+)$", "\\1", ids), NA_character_)
      mixed_ext <- !all(is.na(ext_digit)) &&      # at least one .digit present …
        length(unique(na.omit(ext_digit))) > 1   # …and they differ
      
      df2 <- df %>% dplyr::mutate(
        replicate_id = dplyr::case_when(
          mixed_ext           ~ ext_digit,                                   # .0 / .1 / .2 / .3
          has_dot             ~ sub("^.*?([0-9]{2})\\.[0-9]+$", "\\1", ids), # only .0  → use XS04 etc.
          TRUE                ~ sub("^.*?([0-9]+)(\\.[^.]+)?$", "\\1", ids)  # CSV fallback
        ),
        sample_id = dplyr::case_when(
          mixed_ext           ~ sub("\\.[0-9]+$", "", ids),                  # strip .digit
          has_dot             ~ sub("([0-9]{2})\\.[0-9]+$", "", ids),        # strip last 2 + .0
          TRUE                ~ sub("([0-9]+)(\\.[^.]+)?$", "", ids)
        )
      )
      
      ## ── 2. average every numeric column by sample_id ─────────────────────
      df_avg <- df2 %>%
        dplyr::group_by(sample_id) %>%
        dplyr::summarise(
          dplyr::across(where(is.numeric), mean, na.rm = TRUE),
          .groups = "drop"
        )
      
      ## ── 3. restore the original first‑column name for downstream code ────
      dplyr::rename(df_avg, !!id_col := sample_id)
    })
    
    
    
    # 6. mir_mat(): build the spectral matrix
    mir_mat <- eventReactive(input$start, {
      df <- avg_df()
      req(df)
      
      if (input$upload_type == "partial") {
        
        # Identify spectral columns by numeric type
        spec_idx <- which(sapply(df, is.numeric))
        if (length(spec_idx) == 0) {
          stop("No numeric columns found in CSV for spectral data")
        }
        # Build spectral matrix
        mat <- as.matrix(df[, spec_idx, drop = FALSE])
        
        # Strip 'wn_' prefix and parse to numeric wavenumbers
        raw_names <- names(df)[spec_idx]
        stripped  <- sub("^wn_", "", raw_names)
        wn        <- as.numeric(stripped)
        if (any(is.na(wn))) {
          stop("After stripping 'wn_', some column names are not valid numbers.")
        }
        
        # Order columns by ascending wavenumber
        ord <- order(wn)
        wn  <- wn[ord]
        mat <- mat[, ord, drop = FALSE]
        
        # Update reactive start/stop values
        rv$stop_seq_csv  <- min(wn)
        rv$start_seq_csv <- max(wn)
        
        
        
        
        # Assign numeric wavenumbers as column names and return
        colnames(mat) <- wn
        return(mat)
      }
      
      # —— Raw ZIP path (unchanged) ——
      start_seq <- if (input$file_type == "opus") rv$start_seq_opus else rv$start_seq_csv
      stop_seq  <- if (input$file_type == "opus") rv$stop_seq_opus else rv$stop_seq_csv
      
      
      MIR <- as.data.frame(df[, 2:ncol(df), drop = FALSE])
      col_seq <- seq(from = start_seq, to = stop_seq, length.out = ncol(MIR))
      colnames(MIR) <- col_seq
      as.matrix(MIR)
    })
    
    
    # 7. sg_mat(): optionally Savitzky‐Golay
    sg_mat <- eventReactive(input$start, {
      mat <- mir_mat()
      c   <- cfg()
      
      if (c$sg) {
        # Ensure we have enough columns for the SG window
        if (ncol(mat) < c$w) {
          log_debug(sprintf("SG skipped: need ≥ %d columns, have %d.", c$w, ncol(mat)))
          return(mat)
        }
        return(savitzkyGolay(mat, m = c$m, w = c$w, p = c$p))
      } else {
        return(mat)
      }
      
    })
    
    
    # 8. res_mat(): optionally resample to a custom interval
    res_mat <- eventReactive(input$start, {
      mat <- sg_mat()
      c   <- cfg()
      if (c$resampled) {
        wav    <- as.numeric(colnames(mat))
        new.wav<- seq(4000, 600, by = -c$resample_val)
        out    <- resample(mat, wav, new.wav)
        colnames(out) <- new.wav
        out
      } else {
        mat
      }
    })
    
    # 9. snv_mat(): always run standard‐normal‐variate
    snv_mat <- eventReactive(input$start, {
      withProgress(message = "Preprocessing spectra…", value = 0, {
        incProgress(0.05, detail = "Building base matrix…")
        # triggers mir_mat() → avg_df()/fil_df()/raw_data() as needed
        invisible(mir_mat())
        
        incProgress(0.30, detail = "Savitzky–Golay…")
        mat_sg  <- sg_mat()
        
        incProgress(0.30, detail = "Resampling grid…")
        mat_res <- res_mat()
        
        incProgress(0.30, detail = "SNV / baseline correction…")
        c <- cfg()
        if (c$snv) {
          out <- standardNormalVariate(mat_res)
          colnames(out) <- colnames(mat_res)
          incProgress(0.05, detail = "Finishing…")
          return(out)
        } else {
          incProgress(0.05, detail = "Finishing…")
          return(mat_res)
        }
      })
    })
    
    
    observeEvent(input$upload_type, {
      updateCheckboxInput(session, "use_outlier", value = FALSE)
    })
    
    
    ## wipe the banner whenever *anything* that forces a re-run changes ----
    observeEvent({
      input$start
    },{
      status("\u00A0")              # one NBSP keeps the string non-empty
      rv$removed_samples    <- character(0)
      rv$removed_replicates <- character(0)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    
    # when the final matrix is computed, light up the plots
    observeEvent(snv_mat(), {
      status(build_status_msg())                    # <-- single, tidy message
      log_debug("snv_mat ready — status banner updated")
    })
    
    #-----------
    # Plots
    #-----------
    output$mir_plot <- renderPlot({
      m <- mir_mat(); req(m)
      
      op <- par(no.readonly = TRUE)          # save current settings
      on.exit(par(op))                       # restore when the plot is done
      
      par(cex.main = 1.8,   
          cex.lab  = 1.4,
          cex.axis = 1.2,
          mar      = c(5,5,4,2)) # a bit more room for big labels
      
      matplot(
        main = "Raw Data",
        x    = as.numeric(colnames(m)),
        y    = t(m),
        type = "l",
        xlim = rev(range(as.numeric(colnames(m)))),
        xlab = "Wavenumber (cm⁻¹)",
        ylab = "Absorbance"
      )
    })
    
    output$sg_plot <- renderPlot({
      m <- sg_mat(); req(m)
      
      op <- par(no.readonly = TRUE)          # save current settings
      on.exit(par(op))                       # restore when the plot is done
      
      par(cex.main = 1.8,        
          cex.lab  = 1.4,
          cex.axis = 1.2,
          mar      = c(5,5,4,2)) # a bit more room for big labels
      
      matplot(
        main = "Savitzky Golay",
        x    = as.numeric(colnames(m)),
        y    = t(m),
        type = "l",
        xlim = rev(range(as.numeric(colnames(m)))),
        xlab = "Wavenumber (cm⁻¹)",
        ylab = "Absorbance"
      )
    })
    
    output$resample_plot <- renderPlot({
      m <- res_mat(); req(m)
      
      op <- par(no.readonly = TRUE)          # save current settings
      on.exit(par(op))                       # restore when the plot is done
      
      par(cex.main = 1.8,       
          cex.lab  = 1.4,
          cex.axis = 1.2,
          mar      = c(5,5,4,2)) # a bit more room for big labels
      
      matplot(
        main = "Resampled",
        x    = as.numeric(colnames(m)),
        y    = t(m),
        type = "l",
        xlim = rev(range(as.numeric(colnames(m)))),
        xlab = "Wavenumber (cm⁻¹)",
        ylab = "Absorbance"
      )
    })
    
    output$snv_plot <- renderPlot({
      m <- snv_mat(); req(m)
      
      op <- par(no.readonly = TRUE)          # save current settings
      on.exit(par(op))                       # restore when the plot is done
      
      par(cex.main = 1.8,     
          cex.lab  = 1.4,
          cex.axis = 1.2,
          mar      = c(5,5,4,2)) # a bit more room for big labels
      
      matplot(
        main = "Standard Normal Variate",
        x    = as.numeric(colnames(m)),
        y    = t(m),
        type = "l",
        xlim = rev(range(as.numeric(colnames(m)))),
        xlab = "Wavenumber (cm⁻¹)",
        ylab = "Absorbance"
      )
    })
    
    # inside dataAggregationServer(), right after you define the outputs
    outputOptions(output, "mir_plot",      suspendWhenHidden = FALSE)
    outputOptions(output, "sg_plot",       suspendWhenHidden = FALSE)
    outputOptions(output, "resample_plot", suspendWhenHidden = FALSE)
    outputOptions(output, "snv_plot",      suspendWhenHidden = FALSE)
    
    
    #-----------
    # Downloads
    #-----------
    output$main_data <- downloadHandler(
      filename = function() {
        paste("preprocessed_data_", input$file_type, "_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(shared$main_data)
        write.csv(shared$main_data, file, row.names = FALSE)
      }
    )
    
    output$download_mir_jpg <- downloadHandler(
      filename = function() {
        paste0("raw_spectra_", Sys.Date(), ".jpg")
      },
      content = function(file) {
        jpeg(file, width = 1920, height = 1080, units = "px", quality = 95)
        m <- mir_mat()        # same reactive you use in renderPlot
        
        op <- par(no.readonly = TRUE)          # save current settings
        on.exit(par(op))                       # restore when the plot is done
        
        par(cex.main = 1.8,    
            cex.lab  = 1.4,
            cex.axis = 1.2,
            mar      = c(5,5,4,2)) # a bit more room for big labels
        
        matplot(
          main = "Raw Data",
          x    = as.numeric(colnames(m)),
          y    = t(m),
          type = "l",
          xlim = rev(range(as.numeric(colnames(m)))),
          xlab = "Wavenumber (cm⁻¹)",
          ylab = "Absorbance"
        )
        dev.off()
      }
    )
    
    output$download_sg_jpg <- downloadHandler(
      filename = function() paste0("savitzky_golay_", Sys.Date(), ".jpg"),
      content = function(file) {
        jpeg(file, width = 1920, height = 1080, units = "px", quality = 95)
        m <- sg_mat()
        
        op <- par(no.readonly = TRUE)          # save current settings
        on.exit(par(op))                       # restore when the plot is done
        
        par(cex.main = 1.8,      
            cex.lab  = 1.4,
            cex.axis = 1.2,
            mar      = c(5,5,4,2)) # a bit more room for big labels
        
        matplot(
          main = "Savitzky Golay",
          x    = as.numeric(colnames(m)),
          y    = t(m),
          type = "l",
          xlim = rev(range(as.numeric(colnames(m)))),
          xlab = "Wavenumber (cm⁻¹)",
          ylab = "Absorbance"
        )
        dev.off()
      }
    )
    
    output$download_resample_jpg <- downloadHandler(
      filename = function() paste0("resampled_", Sys.Date(), ".jpg"),
      content = function(file) {
        jpeg(file, width = 1920, height = 1080, units = "px", quality = 95)
        m <- res_mat()
        
        op <- par(no.readonly = TRUE)          # save current settings
        on.exit(par(op))                       # restore when the plot is done
        
        par(cex.main = 1.8,    
            cex.lab  = 1.4,
            cex.axis = 1.2,
            mar      = c(5,5,4,2)) # a bit more room for big labels
        
        matplot(
          main = "Resampled",
          x    = as.numeric(colnames(m)),
          y    = t(m),
          type = "l",
          xlim = rev(range(as.numeric(colnames(m)))),
          xlab = "Wavenumber (cm⁻¹)",
          ylab = "Absorbance"
        )
        dev.off()
      }
    )
    
    output$download_snv_jpg <- downloadHandler(
      filename = function() paste0("snv_", Sys.Date(), ".jpg"),
      content = function(file) {
        jpeg(file, width = 1920, height = 1080, units = "px", quality = 95)
        m <- snv_mat()
        
        op <- par(no.readonly = TRUE)          # save current settings
        on.exit(par(op))                       # restore when the plot is done
        
        par(cex.main = 1.8,     
            cex.lab  = 1.4,
            cex.axis = 1.2,
            mar      = c(5,5,4,2)) # a bit more room for big labels
        
        matplot(
          main = "Standard Normal Variate",
          x    = as.numeric(colnames(m)),
          y    = t(m),
          type = "l",
          xlim = rev(range(as.numeric(colnames(m)))),
          xlab = "Wavenumber (cm⁻¹)",
          ylab = "Absorbance"
        )
        dev.off()
      }
    )
    
    # ——— NEW download handler: CSV + metadata bundled in a .zip ———
    output$step_data <- downloadHandler(
      filename = function() paste0("preprocessed_", Sys.Date(), ".zip"),
      content = function(file) {
        withProgress(message = "Packaging download…", value = 0, {
          incProgress(0.2, detail = "Assembling matrix…")
          mat  <- snv_mat()
          samp <- avg_df()$scan_path_name
          df_out <- data.frame(
            scan_path_name = samp,
            as.data.frame(mat, check.names = FALSE),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          
          incProgress(0.2, detail = "Writing CSV…")
          tmpdir <- tempdir()
          data_file <- file.path(tmpdir, "processed_data.csv")
          write.csv(df_out, data_file, row.names = FALSE)
          
          incProgress(0.3, detail = "Writing metadata…")
          c <- cfg()
          
          meta <- paste(
            "Project: MIR Pre-processing",
            "\nVersion: 1.0",
            "\nDate:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            "\n\nInput:",
            sprintf("\n      Upload type: %s", input$upload_type),
            sprintf("\n      File type  : %s", ifelse(input$upload_type == "raw", input$file_type, "CSV")),
            sprintf("\n      Samples    : %d", nrow(df_out)),
            sprintf("\n      Wavenumbers: %s to %s cm⁻¹",
                    colnames(df_out)[ncol(df_out)], colnames(df_out)[2]),
            "\n\nPre-processing pipeline (in order):",
            sprintf("\n      • Positive filter      : %s", c$filtered),
            sprintf("\n      • Outlier detection    : %s", c$use_outlier),
            sprintf("\n      • Replicate averaging  : %s", c$averaged),
            sprintf("\n      • Savitzky-Golay       : %s", c$sg),
            if (c$sg) sprintf("  (m = %d, w = %d, p = %d)", c$m, c$w, c$p),
            sprintf("\n      • Resample interval    : %s", ifelse(c$resampled, paste(c$resample_val, "cm⁻¹"), "no")),
            sprintf("\n      • SNV / baseline corr. : %s", c$snv),
            "\n\nSoftware:",
            "\n      R ", getRversion(), " (prospectr, shiny, shinyWidgets)"
          )
          
          if (input$upload_type == "raw" && input$file_type == "opus") {
            orig_hi <- isolate(rv$orig_start_seq_opus)
            orig_lo <- isolate(rv$orig_stop_seq_opus)
            snip_hi <- isolate(rv$snipped_start_seq_opus)
            snip_lo <- isolate(rv$snipped_stop_seq_opus)
            
            if (is.null(snip_hi) || is.null(snip_lo)) {
              snip_hi <- orig_hi
              snip_lo <- orig_lo
            }
            
            trim_msg <- if (!is.null(orig_hi) && !is.null(orig_lo) &&
                            !is.null(snip_hi) && !is.null(snip_lo) &&
                            (orig_hi != snip_hi || orig_lo != snip_lo)) {
              "Yes"
            } else {
              "No"
            }
            
            wn_note <- paste(
              "\n\nWavenumber trimming:",
              sprintf("\n      Original OPUS range : %s to %s cm⁻¹", orig_hi, orig_lo),
              sprintf("\n      Final output range  : %s to %s cm⁻¹", snip_hi, snip_lo),
              sprintf("\n      Snipping applied    : %s", trim_msg)
            )
            
            meta <- paste0(meta, wn_note)
          }
          
          if (c$use_outlier) {
            rs <- isolate(rv$removed_samples)
            rr <- isolate(rv$removed_replicates)
            
            if (is.null(rs)) rs <- character(0)
            if (is.null(rr)) rr <- character(0)
            
            n_samp <- length(rs)
            n_rep  <- length(rr)
            
            removed_samples_txt <- if (n_samp > 0) {
              paste0("      - ", rs, collapse = "\n")
            } else {
              "      None"
            }
            
            removed_reps_txt <- if (n_rep > 0) {
              paste0("      - ", rr, collapse = "\n")
            } else {
              "      None"
            }
            
            meta <- paste0(
              meta,
              "\n\nOutlier detection results:",
              sprintf("\n      • Samples removed   : %d", n_samp),
              sprintf("\n      • Replicates removed: %d", n_rep),
              "\n\n      Removed sample IDs:\n",
              removed_samples_txt,
              "\n\n      Removed replicate IDs:\n",
              removed_reps_txt
            )
          }
          meta_file <- file.path(tmpdir, "metadata.txt")
          writeLines(meta, meta_file)
          
          incProgress(0.25, detail = "Zipping…")
          old_wd <- getwd()
          setwd(tmpdir); on.exit(setwd(old_wd), add = TRUE)
          zip(zipfile = file, files = c("processed_data.csv", "metadata.txt"))
          incProgress(0.05, detail = "Done")
        })
      },
      contentType = "application/zip"
    )
    
    
    
    output$processing_status <- renderText({
      status()
    })
    
    output$error_message <- renderText({
      error_message()
    })
  })
}
