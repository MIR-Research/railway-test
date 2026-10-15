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
          card_header("Preprocessing Options"),
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
            # Calibration transfer (spectral space transformation, SST): maps
            # Alpha II spectra into the Vertex 70 / KSSL spectral space. Only
            # available with the preprocessing the transfer model was built
            # on; see sst_allowed() in the server.
            checkboxInput(
              ns("sst"),
              # label = "Calibration Transfer (Alpha II → Vertex 70 / KSSL)",
              label = tagList(
                "Calibration Transfer - Spectral Space Transformation",
                br(),
                "Alpha II → Vertex 70/HTS-XT (KSSL Instrument)"
              ),
              value = FALSE
            ),
            uiOutput(ns("sst_note")),
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
        uiOutput(ns("sg_card")),
        uiOutput(ns("resample_card")),
        uiOutput(ns("snv_card")),
        # uiOutput(ns("sst_card")),
        sst_card_ui(ns)
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
      snipped_stop_seq_opus = NULL,
      orig_start_seq_csv = NULL,
      orig_stop_seq_csv = NULL,
      snipped_start_seq_csv = NULL,
      snipped_stop_seq_csv = NULL
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
            
            # preserve original range for metadata
            if (is.null(rv$orig_start_seq_csv)) rv$orig_start_seq_csv <- vals$wn[1]
            if (is.null(rv$orig_stop_seq_csv))  rv$orig_stop_seq_csv  <- vals$wn[nrow(vals)]
            
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
        # reset CSV range tracking for this upload
        rv$start_seq_csv          <- NULL
        rv$stop_seq_csv           <- NULL
        rv$orig_start_seq_csv     <- NULL
        rv$orig_stop_seq_csv      <- NULL
        rv$snipped_start_seq_csv  <- NULL
        rv$snipped_stop_seq_csv   <- NULL
        
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
          
          # preserve original range for metadata
          rv$orig_start_seq_csv <- max(wns)
          rv$orig_stop_seq_csv  <- min(wns)
          rv$snipped_start_seq_csv <- NULL
          rv$snipped_stop_seq_csv  <- NULL
          
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

      # calibration transfer, only when the settings match the transfer model
      cfg0$sst <- isTRUE(input$sst) && sst_compatible(cfg0)

      # and return
      cfg0
    })

    # ---- Calibration transfer (SST) -----------------------------------------
    # The transfer model was built on spectra preprocessed exactly this way:
    # snip 4000-600 -> SG (m = 0, w = 13, p = 2) -> resample to 10 cm^-1 -> SNV
    # (the "Default" settings). With any other settings its output would be
    # meaningless, so the checkbox is disabled unless the settings match.
    sst_settings_match <- function(c) {
      isTRUE(c$sg) && c$m == 0 && c$w == 13 && c$p == 2 &&
        isTRUE(c$resampled) && c$resample_val == 10 &&
        isTRUE(c$snv)
    }

    # TEMPORARY: the settings check is switched off so already-preprocessed
    # files can be uploaded (with SG / resampling / SNV unticked) and run
    # through the calibration transfer as-is. Set back to TRUE to require
    # the Default settings again. metadata.txt records whether they matched.
    SST_REQUIRE_DEFAULT_SETTINGS <- FALSE

    sst_compatible <- function(c) {
      !SST_REQUIRE_DEFAULT_SETTINGS || sst_settings_match(c)
    }

    observe({
      if (sst_compatible(cfg())) {
        shinyjs::enable("sst")
      } else {
        shinyjs::disable("sst")
        if (isTRUE(input$sst)) updateCheckboxInput(session, "sst", value = FALSE)
      }
    })

    output$sst_note <- renderUI({
      if (sst_compatible(cfg())) return(NULL)
      tags$small(
        class = "text-muted",
        "Calibration transfer requires the Default preprocessing settings ",
        "(Savitzky-Golay m = 0, w = 13, p = 2; resample to 10 cm⁻¹; SNV)."
      )
    })

    # Describe the calibration transfer tool when the user ticks the box
    observeEvent(input$sst, {
      if (!isTRUE(input$sst)) return()
      showModal(modalDialog(
        title = "Calibration transfer accuracy",
        p("Spectra must be preprocessed exactly as described (snip 4000–600 cm⁻¹ → ",
          "SG smoothing, w = 13, p = 2 → 10 cm⁻¹ resampling → SNV)."),
        p("SST reduced the spectral difference between the two instruments from ",
          "RMSE = 0.280 to 0.212, a 24% reduction, and increased spectral agreement ",
          "(CCC) between them from 0.82 to 0.88."),
        p("The SST transform was developed from 100 soils scanned on one Bruker Alpha II ",
          "and one Bruker Vertex 70/HTS-XT. SST has not been tested on other Alpha II ",
          "units; because each instrument has its own optical characteristics, performance ",
          "on another unit may be different."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }, ignoreInit = TRUE)

    # The transfer model lives in the bucket and is loaded through the shared
    # model cache (bucket_helper.R), so it is downloaded once per app process.
    sst_model_key <- Sys.getenv("SST_MODEL_KEY", "transfer_models/SST_final_UW100.rds")

    # Applies the transfer model to the preprocessed spectra (same steps as
    # apply_SST_UW100.R). Returns list(before, after, model) or NULL when the
    # transfer wasn't requested; stops with a message if it fails.
    sst_result <- eventReactive(input$start, {
      c <- last_cfg()
      if (!isTRUE(c$sst)) return(NULL)
      mat <- snv_mat()

      sst <- tryCatch(read_bucket_rds_cached(sst_model_key), error = function(e) e)
      if (inherits(sst, "error")) {
        msg <- paste("Calibration transfer failed: could not load the transfer model (",
                     sst_model_key, "):", conditionMessage(sst))
        showNotification(msg, type = "error")
        stop(msg)
      }

      # Check the wavenumbers match before transforming
      missing_wn <- setdiff(sst$wavenumbers, colnames(mat))
      if (length(missing_wn) > 0) {
        msg <- paste("Calibration transfer failed: missing wavenumber columns, e.g.",
                     paste(head(missing_wn, 3), collapse = ", "))
        showNotification(msg, type = "error")
        stop(msg)
      }

      before <- mat[, sst$wavenumbers, drop = FALSE]   # selects + orders the columns
      after  <- sst$apply(before)
      colnames(after) <- sst$wavenumbers
      list(before = before, after = after, model = sst)
    })

    # The spectra that go into the download: transformed if calibration
    # transfer was applied, otherwise the regular preprocessing output.
    final_mat <- eventReactive(input$start, {
      if (isTRUE(last_cfg()$sst)) sst_result()$after else snv_mat()
    })
    
    # snapshot of cfg() at the moment "Start" is clicked, so the plot cards
    # below show/hide in sync with what was actually run, not live checkbox state
    last_cfg <- eventReactive(input$start, {
      cfg()
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
      
      # figure out which columns survive the wavenumber snip (599-4000.99 cm⁻¹)
      keep_wave_cols <- data_cols
      wn_full <- NULL
      
      if (input$upload_type == "raw" && input$file_type == "opus" && length(data_cols) > 0) {
        wn_full <- seq(
          from = rv$start_seq_opus,
          to   = rv$stop_seq_opus,
          length.out = length(data_cols)
        )
      } else if (input$upload_type == "raw" && input$file_type == "csv" && length(data_cols) > 0) {
        wn_full <- seq(
          from = rv$start_seq_csv,
          to   = rv$stop_seq_csv,
          length.out = length(data_cols)
        )
      } else if (input$upload_type == "partial" && length(data_cols) > 0) {
        wn_full <- as.numeric(colnames(df)[data_cols])
      }
      
      if (!is.null(wn_full)) {
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
      
      # Now actually snip down to the target wavenumber range
      if (!is.null(wn_full)) {
        keep_mask <- !is.na(wn_full) & wn_full >= 599 & wn_full <= 4000.99
        
        if (any(keep_mask)) {
          df <- cbind(
            df[, 1, drop = FALSE],
            df[, data_cols[keep_mask], drop = FALSE]
          )
          
          if (input$upload_type == "raw" && input$file_type == "opus") {
            rv$snipped_start_seq_opus <- max(wn_full[keep_mask], na.rm = TRUE)
            rv$snipped_stop_seq_opus  <- min(wn_full[keep_mask], na.rm = TRUE)
            
            # update downstream range to the snipped range
            rv$start_seq_opus <- rv$snipped_start_seq_opus
            rv$stop_seq_opus  <- rv$snipped_stop_seq_opus
          } else if (input$upload_type == "raw" && input$file_type == "csv") {
            rv$snipped_start_seq_csv <- max(wn_full[keep_mask], na.rm = TRUE)
            rv$snipped_stop_seq_csv  <- min(wn_full[keep_mask], na.rm = TRUE)
            
            # update downstream range to the snipped range
            rv$start_seq_csv <- rv$snipped_start_seq_csv
            rv$stop_seq_csv  <- rv$snipped_stop_seq_csv
          } else if (input$upload_type == "partial") {
            rv$snipped_start_seq_csv <- max(wn_full[keep_mask], na.rm = TRUE)
            rv$snipped_stop_seq_csv  <- min(wn_full[keep_mask], na.rm = TRUE)
          }
        } else {
          log_debug("Post-filter clipping found 0 columns in [599, 4000.99].")
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
        
        # Identify spectral columns by numeric type. The first column is the
        # sample ID (as everywhere else in this pipeline), so it's excluded
        # even when the IDs are numbers (e.g. "SampleID" = 28851, ...).
        spec_idx <- setdiff(which(sapply(df, is.numeric)), 1L)
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
      done_txt <- "Processing complete!"
      if (isTRUE(last_cfg()$sst)) {
        # Run the calibration transfer now so any failure shows immediately
        ok <- tryCatch({ sst_result(); TRUE }, error = function(e) FALSE)
        done_txt <- if (ok) "Processing complete! Calibration transfer applied."
                    else "Preprocessing complete, but calibration transfer FAILED (see Messages)."
      }
      status(build_status_msg(done_txt))            # <-- single, tidy message
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
        xlab = expression(paste("Wavenumber (cm"^-1, ")")),
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
        xlab = expression(paste("Wavenumber (cm"^-1, ")")),
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
        xlab = expression(paste("Wavenumber (cm"^-1, ")")),
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
        xlab = expression(paste("Wavenumber (cm"^-1, ")")),
        ylab = "Absorbance"
      )
    })
    
    # Mean +/- SD of the spectra before and after calibration transfer
    # (adapted from the old Spectral Transformation page's mean +/- SD plot).
    draw_sst_plot <- function(res) {
      B <- res$before; A <- res$after
      wav <- as.numeric(colnames(B))
      mean_B <- colMeans(B); sd_B <- apply(B, 2, sd)
      mean_A <- colMeans(A); sd_A <- apply(A, 2, sd)
      sd_B[is.na(sd_B)] <- 0; sd_A[is.na(sd_A)] <- 0   # a single sample has no SD

      op <- par(no.readonly = TRUE)
      on.exit(par(op))
      par(cex.main = 1.8, cex.lab = 1.4, cex.axis = 1.2, mar = c(5, 5, 4, 2))

      y_range <- range(c(mean_B + sd_B, mean_B - sd_B, mean_A + sd_A, mean_A - sd_A), na.rm = TRUE)
      y_range[2] <- y_range[2] + 0.25 * diff(y_range)   # room for the legend above the data
      draw_band <- function(m, s, col) {
        polygon(c(wav, rev(wav)), c(m + s, rev(m - s)),
                col = adjustcolor(col, alpha.f = 0.15), border = NA)
      }
      plot(wav, mean_B, type = "n",
           xlim = rev(range(wav)), ylim = y_range,
           main = "Calibration Transfer: Mean ± SD",
           xlab = expression(paste("Wavenumber (cm"^-1, ")")),
           ylab = "Absorbance")
      draw_band(mean_B, sd_B, "red")
      draw_band(mean_A, sd_A, "blue")
      lines(wav, mean_B, col = "red",  lwd = 2, lty = 2)
      lines(wav, mean_A, col = "blue", lwd = 2)
      legend("topright",
             legend = c("Before transfer (Alpha II)", "After transfer (Vertex 70 / KSSL space)"),
             col = c("red", "blue"), lty = c(2, 1), lwd = 2,
             fill = adjustcolor(c("red", "blue"), alpha.f = 0.15), border = NA)
    }

    # The user's spectra before and after calibration transfer against the
    # KSSL reference: the mean +/- SD of a property's KSSL calibration
    # spectra, drawn like the Static Models spectral plot (grey band, black
    # mean line). See kssl_reference_summary() at the bottom of this file.
    draw_sst_kssl_plot <- function(res, ref) {
      B <- res$before; A <- res$after
      wav <- as.numeric(colnames(B))
      mean_B <- colMeans(B); mean_A <- colMeans(A)
      ref_mean <- ref$mean[colnames(B)]; ref_sd <- ref$sd[colnames(B)]
      if (anyNA(ref_mean)) stop("KSSL reference wavenumbers don't match the transferred spectra")

      op <- par(no.readonly = TRUE)
      on.exit(par(op))
      par(cex.main = 1.8, cex.lab = 1.4, cex.axis = 1.2, mar = c(5, 5, 4, 2))

      y_range <- range(c(ref_mean + ref_sd, ref_mean - ref_sd, mean_B, mean_A), na.rm = TRUE)
      y_range[2] <- y_range[2] + 0.25 * diff(y_range)   # room for the legend above the data
      plot(wav, ref_mean, type = "n",
           xlim = rev(range(wav)), ylim = y_range,
           main = paste0("Calibration Transfer vs KSSL Reference (", ref$label, ")"),
           xlab = expression(paste("Wavenumber (cm"^-1, ")")),
           ylab = "Absorbance")
      polygon(c(wav, rev(wav)), c(ref_mean + ref_sd, rev(ref_mean - ref_sd)),
              col = scales::alpha("gray", 0.5), border = NA)
      lines(wav, ref_mean, col = "black", lwd = 2)
      lines(wav, mean_B, col = "red",  lwd = 2, lty = 2)
      lines(wav, mean_A, col = "blue", lwd = 2)
      legend("topright",
             legend = c(sprintf("KSSL mean ± SD (%s, n = %s)", ref$label, format(ref$n, big.mark = ",")),
                        "Before transfer (Alpha II)", "After transfer (Vertex 70 / KSSL space)"),
             col = c("black", "red", "blue"), lty = c(1, 2, 1), lwd = 2,
             fill = c(scales::alpha("gray", 0.5), NA, NA), border = NA,
             bg = "white", box.lwd = 0.5, box.col = "black")
    }

    # NULL (with an error message) if the KSSL reference can't be loaded
    sst_kssl_reference <- function() {
      tryCatch(
        withProgress(message = "Loading KSSL reference spectra...", value = 0.5,
                     kssl_reference_summary(SST_KSSL_REFERENCE_PROPERTY)),
        error = function(e) {
          showNotification(paste("Could not load the KSSL reference spectra:", conditionMessage(e)),
                           type = "error")
          NULL
        }
      )
    }

    # Each view now has its own tab, plot and download (see sst_card below),
    # so this switch is no longer needed.
    # # Draws whichever view is selected with the buttons above the plot
    # draw_selected_sst_plot <- function(res) {
    #   if (identical(input$sst_plot_view, "kssl")) {
    #     ref <- sst_kssl_reference(); req(ref)
    #     draw_sst_kssl_plot(res, ref)
    #   } else {
    #     draw_sst_plot(res)
    #   }
    # }

    output$sst_plot <- renderPlot({
      res <- sst_result(); req(res)
      draw_sst_plot(res)
    })

    output$sst_kssl_plot <- renderPlot({
      res <- sst_result(); req(res)
      ref <- sst_kssl_reference(); req(ref)
      draw_sst_kssl_plot(res, ref)
    })

    output$download_sst_jpg <- downloadHandler(
      filename = function() paste0("calibration_transfer_", Sys.Date(), ".jpg"),
      content = function(file) {
        jpeg(file, width = 1920, height = 1080, units = "px", quality = 95)
        on.exit(dev.off())
        draw_sst_plot(sst_result())
      }
    )

    output$download_sst_kssl_jpg <- downloadHandler(
      filename = function() paste0("calibration_transfer_vs_KSSL_", Sys.Date(), ".jpg"),
      content = function(file) {
        ref <- sst_kssl_reference(); req(ref)
        jpeg(file, width = 1920, height = 1080, units = "px", quality = 95)
        on.exit(dev.off())
        draw_sst_kssl_plot(sst_result(), ref)
      }
    )

    # The calibration transfer card is now in the page UI (see
    # sst_card_ui() at the bottom of this file) and shown with a
    # conditionalPanel when this flag is TRUE. Built here with renderUI, its
    # tabs got Bootstrap 3 markup: the old view buttons did not respond to
    # clicks and the tab labels did not pick up the site styling.
    output$sst_done <- reactive({
      req(input$start)
      isTRUE(last_cfg()$sst)
    })
    outputOptions(output, "sst_done", suspendWhenHidden = FALSE)

    # output$sst_card <- renderUI({
    #   req(input$start)
    #   c <- last_cfg()
    #   if (!isTRUE(c$sst)) return(NULL)
    #   # tagList(
    #   #   br(),
    #   #   card(
    #   #     full_screen = TRUE,
    #   #     card_header = "Calibration Transfer Plot",
    #   #     card_body(
    #   #       # Switch between the before/after plot and the KSSL comparison
    #   #       shinyWidgets::radioGroupButtons(
    #   #         ns("sst_plot_view"), label = NULL,
    #   #         choices = c("Before vs After" = "transfer", "Compare with KSSL" = "kssl"),
    #   #         selected = "transfer", size = "sm"
    #   #       ),
    #   #       plotOutput(ns("sst_plot")),
    #   #       downloadButton(ns("download_sst_jpg"), "Download Calibration Transfer JPG")
    #   #     )
    #   #   )
    #   # )
    #   # Tabs switch between the before/after plot and the KSSL comparison.
    #   # (The radioGroupButtons above didn't respond to clicks: built inside
    #   # renderUI they got Bootstrap 3 markup this Bootstrap 5 page can't use.)
    #   # The KSSL plot is only drawn, and its reference loaded, when its tab
    #   # is opened.
    #   tagList(
    #     br(),
    #     # Same tab style as the Static Models property tabs
    #     navset_card_tab(
    #       id = ns("sst_plot_view"),
    #       full_screen = TRUE,
    #       nav_panel(
    #         "Before vs After", value = "transfer",
    #         plotOutput(ns("sst_plot")),
    #         downloadButton(ns("download_sst_jpg"), "Download Calibration Transfer JPG")
    #       ),
    #       nav_panel(
    #         "Compare with KSSL", value = "kssl",
    #         plotOutput(ns("sst_kssl_plot")),
    #         downloadButton(ns("download_sst_kssl_jpg"), "Download KSSL Comparison JPG")
    #       )
    #     )
    #   )
    # })

    # inside dataAggregationServer(), right after you define the outputs
    # sst_plot is left out: drawing it before its card is on the page made
    # thematic error ("getCurrentOutputInfo()[["bg"]]: attempt to apply
    # non-function"), since the page hasn't reported the plot's colours yet.
    # outputOptions(output, "sst_plot",      suspendWhenHidden = FALSE)
    outputOptions(output, "mir_plot",      suspendWhenHidden = FALSE)
    outputOptions(output, "sg_plot",       suspendWhenHidden = FALSE)
    outputOptions(output, "resample_plot", suspendWhenHidden = FALSE)
    outputOptions(output, "snv_plot",      suspendWhenHidden = FALSE)
    
    # Only show each optional plot's card if that preprocessing step was run
    output$sg_card <- renderUI({
      req(input$start)
      c <- last_cfg()
      if (!isTRUE(c$sg)) return(NULL)
      tagList(
        br(),
        card(
          full_screen = TRUE,
          card_header = "MIR + Savitzky-Golay Plot",
          card_body(
            plotOutput(ns("sg_plot")),
            downloadButton(ns("download_sg_jpg"), "Download SG JPG")
          )
        )
      )
    })
    
    output$resample_card <- renderUI({
      req(input$start)
      c <- last_cfg()
      if (!isTRUE(c$resampled)) return(NULL)
      tagList(
        br(),
        card(
          full_screen = TRUE,
          card_header = "Resample Plot",
          card_body(
            plotOutput(ns("resample_plot")),
            downloadButton(ns("download_resample_jpg"), "Download Resample JPG")
          )
        )
      )
    })
    
    output$snv_card <- renderUI({
      req(input$start)
      c <- last_cfg()
      if (!isTRUE(c$snv)) return(NULL)
      tagList(
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
    })
    
    
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
          xlab = expression(paste("Wavenumber (cm"^-1, ")")),
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
          xlab = expression(paste("Wavenumber (cm"^-1, ")")),
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
          xlab = expression(paste("Wavenumber (cm"^-1, ")")),
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
          xlab = expression(paste("Wavenumber (cm"^-1, ")")),
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
          mat  <- final_mat()   # transformed if calibration transfer was applied
          samp <- avg_df()[[1]]  # ID column name varies by upload type
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
            sprintf("\n      • Calibration transfer : %s", isTRUE(last_cfg()$sst)),
            if (isTRUE(last_cfg()$sst)) {
              sst <- sst_result()$model
              paste0(
                "\n\nCalibration transfer (spectral space transformation):",
                sprintf("\n      Direction     : %s", sst$direction),
                sprintf("\n      Transfer set  : %s", sst$transfer_set),
                sprintf("\n      Components    : %s", sst$ncomp),
                sprintf("\n      Settings match model's preprocessing : %s",
                        if (sst_settings_match(last_cfg())) "Yes" else
                          "NO (settings check was switched off; input assumed already preprocessed)"),
                sprintf("\n      Preprocessing : %s", sst$preprocessing),
                sprintf("\n      Instrument    : %s", sst$instrument),
                sprintf("\n      Model created : %s", format(sst$created)),
                sprintf("\n      Model file    : %s", sst_model_key)
              )
            },
            "\n\nSoftware:",
            "\n      R ", getRversion(), " (prospectr, shiny, shinyWidgets)"
          )
          
          if ((input$upload_type == "raw" && input$file_type == "opus") ||
              (input$upload_type == "raw" && input$file_type == "csv") ||
              input$upload_type == "partial") {
            
            if (input$upload_type == "partial" ||
                (input$upload_type == "raw" && input$file_type == "csv")) {
              orig_hi   <- isolate(rv$orig_start_seq_csv)
              orig_lo   <- isolate(rv$orig_stop_seq_csv)
              snip_hi   <- isolate(rv$snipped_start_seq_csv)
              snip_lo   <- isolate(rv$snipped_stop_seq_csv)
              range_lbl <- "CSV"
            } else {
              orig_hi   <- isolate(rv$orig_start_seq_opus)
              orig_lo   <- isolate(rv$orig_stop_seq_opus)
              snip_hi   <- isolate(rv$snipped_start_seq_opus)
              snip_lo   <- isolate(rv$snipped_stop_seq_opus)
              range_lbl <- "OPUS"
            }
            
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
              sprintf("\n      Original %-4s range : %s to %s cm⁻¹", range_lbl, orig_hi, orig_lo),
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

# ---- KSSL reference for the calibration transfer comparison plot ------------
# Mean and SD of one property's KSSL calibration spectra (already SG ->
# resample -> SNV preprocessed; the same data the Static Models spectral plot
# uses), loaded through the shared calibration cache (calibration_data.R).
# Memoised so the SD is computed once per app process; errors aren't cached.
SST_KSSL_REFERENCE_PROPERTY <- "TC"
SST_KSSL_REFERENCE_LABEL    <- "Total Carbon"

kssl_reference_summary <- memoise::memoise(function(property) {
  snv <- load_calibration(property)$snv
  if (is.null(snv) || nrow(snv) == 0) stop("no calibration data for ", property)
  list(
    label = SST_KSSL_REFERENCE_LABEL,
    n     = nrow(snv),
    mean  = colMeans(snv, na.rm = TRUE),
    sd    = apply(snv, 2, sd, na.rm = TRUE)
  )
})


# ---- Calibration transfer plot card (Data Preprocessing page) ---------------
# Tabs switch between the before/after plot and the KSSL comparison, in the
# same style as the Static Models property tabs. Hidden until a run with
# calibration transfer finishes (output$sst_done in the server). The KSSL plot
# is only drawn, and its reference loaded, when its tab is opened.
sst_card_ui <- function(ns) {
  conditionalPanel(
    condition = sprintf("output['%s']", ns("sst_done")),
    br(),
    navset_card_tab(
      id = ns("sst_plot_view"),
      full_screen = TRUE,
      nav_panel(
        "Before vs After", value = "transfer",
        plotOutput(ns("sst_plot")),
        downloadButton(ns("download_sst_jpg"), "Download Calibration Transfer JPG")
      ),
      nav_panel(
        "Compare with KSSL", value = "kssl",
        plotOutput(ns("sst_kssl_plot")),
        downloadButton(ns("download_sst_kssl_jpg"), "Download KSSL Comparison JPG")
      )
    )
  )
}
