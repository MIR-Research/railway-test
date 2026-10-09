library(dplyr)
library(randomForest)
library(caret)
library(caTools)
library(shiny)
library(shinyjs)
library(bslib)
library(prospectr)
library(FNN)
library(ggplot2)
library(DT)
# Old version: read from a local pretrained_pca/ folder, which isn't in the
# repo or the Docker image, and kept every loaded file in memory.
# pca_dir <- "pretrained_pca"
#
# get_pretrained_pca <- memoise(function(prop) {
#   path <- file.path(pca_dir, paste0("pca_", prop, ".rds"))
#   if (!file.exists(path)) stop("No pretrained PCA for soil property: ", prop)
#   readRDS(path)
# })

# Reads pretrained_pca/pca_<prop>.rds from the bucket through the shared
# model cache (bucket_helper.R). It shares that cache's MODEL_CACHE_MB limit
# with the models rather than adding a separate one, and the least-recently
# used file is dropped first when the limit is reached.
get_pretrained_pca <- function(prop) {
  object_key <- bucket_key("pretrained_pca", paste0("pca_", prop, ".rds"))
  tryCatch(
    read_bucket_rds_cached(object_key),
    error = function(e) {
      stop("No pretrained PCA for soil property: ", prop, " (", conditionMessage(e), ")")
    }
  )
}
# ---- Training + prediction (runs in a background process) -------------------
#
# Trains the chosen model on the selected calibration neighbours, computes the
# internal calibration metrics and predicts the user's samples. It is sent to
# a background R process (background_tasks.R), which has none of the app's
# other functions, so everything it needs is defined inside it, loaded with
# library() or passed in as an argument. It never calls showNotification():
# it returns list(ok = TRUE, ...) on success or list(ok = FALSE, error = ...)
# with the message to show, and the app displays the result.
#
#   train_data   : data.frame of preprocessed spectra + calc_value
#   user_matrix  : user spectra in the pretrained-PCA column order
#                  (used by the PCA-based models and PLS)
#   user_spectra : all user spectral columns (used by CNN)
#   model_type   : "rf", "svm", "cb", "pls" or "cnn"
knn_train_and_predict <- function(train_data, user_matrix, user_spectra, model_type,
                                  res_var = "calc_value") {
  suppressPackageStartupMessages(library(caret))
  if (model_type == "cnn") suppressPackageStartupMessages(library(keras))
  # Use this process's parallel workers if it has them (parallel_training.R)
  if (exists("ensure_training_cluster", mode = "function")) ensure_training_cluster()

  fail <- function(msg) list(ok = FALSE, error = msg)
  retry_hint <- " try increasing distance or number of neighbors."

  # PCA (99% variance) then caret::train, as used by RF, SVM and Cubist
  train_on_pca <- function(trainData, method, ...) {
    X <- trainData[, !(names(trainData) %in% res_var)]
    Y <- trainData[[res_var]]
    pca <- prcomp(X, center = TRUE, scale. = TRUE)
    cum_var <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
    num_components <- which(cum_var >= 0.99)[1]
    X_pca <- as.data.frame(pca$x[, 1:num_components, drop = FALSE])
    fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
    model <- train(x = X_pca, y = Y,
                   method = method,
                   trControl = fitControl,
                   metric = "RMSE",
                   ...)
    list(model = model, pca = pca, num_components = num_components)
  }

  train_pls <- function(trainData) {
    X <- trainData[, !(names(trainData) %in% res_var)]
    Y <- trainData[[res_var]]
    fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
    pls_model <- train(
      x = X,
      y = Y,
      na.action = na.omit,
      trControl = fitControl,
      method = "pls",
      tuneLength = 30,
      metric = "RMSE"
    )
    list(model = pls_model, pca = NULL, num_components = NA)
  }

  # Same network as before; the %>% chains are written as plain calls.
  train_cnn <- function(trainData) {
    data <- trainData[, !(names(trainData) %in% res_var)]
    Y <- trainData[[res_var]]
    n_features <- ncol(data)
    model <- keras_model_sequential()
    model <- layer_conv_1d(model, filters = 32, kernel_size = 3, activation = 'relu',
                           input_shape = c(n_features, 1))
    model <- layer_batch_normalization(model)
    model <- layer_max_pooling_1d(model, pool_size = 2)
    model <- layer_conv_1d(model, filters = 64, kernel_size = 5, activation = 'relu')
    model <- layer_batch_normalization(model)
    model <- layer_max_pooling_1d(model, pool_size = 2)
    model <- layer_flatten(model)
    model <- layer_dense(model, units = 128, activation = 'relu')
    model <- layer_dropout(model, rate = 0.2)
    model <- layer_dense(model, units = 1, activation = "linear")

    compile(model,
            loss = "mse",
            optimizer = "adam",
            metrics = c("mean_absolute_error"))

    x <- as.matrix(data)
    x_array <- array_reshape(x, c(nrow(x), n_features, 1))

    fit(model,
        x_array, Y,
        epochs = 50,
        batch_size = 32,
        callbacks = list(
          callback_early_stopping(monitor = "loss", patience = 15, min_delta = 0.001,
                                  restore_best_weights = TRUE)
        ),
        verbose = 0)
    list(model = model, pca = NULL, num_components = NA)
  }

  model_labels <- c(rf = "Random Forest", svm = "SVM", cb = "Cubist",
                    pls = "PLS", cnn = "CNN")

  # ---- Train ----
  modelResult <- tryCatch(
    switch(model_type,
           "rf"  = train_on_pca(train_data, "rf"),
           "svm" = train_on_pca(train_data, "svmRadial", tuneLength = 10),
           "cb"  = train_on_pca(train_data, "cubist"),
           "pls" = train_pls(train_data),
           "cnn" = train_cnn(train_data)),
    error = function(e) e
  )
  if (inherits(modelResult, "error")) {
    return(fail(paste("Error training", model_labels[[model_type]], "model:",
                      conditionMessage(modelResult), retry_hint)))
  }

  # ---- Predictions on the training data, for the calibration metrics ----
  obs_cal <- train_data[[res_var]]
  pred_cal <- tryCatch({
    if (!is.null(modelResult$pca)) {
      pca_scores <- as.data.frame(modelResult$pca$x)[, 1:modelResult$num_components, drop = FALSE]
      predict(modelResult$model, newdata = pca_scores)
    } else if (model_type == "cnn") {
      pred_cols <- setdiff(names(train_data), res_var)
      xmat      <- as.matrix(train_data[, pred_cols, drop = FALSE])
      arr_train <- array_reshape(xmat, c(nrow(xmat), ncol(xmat), 1))
      predict(modelResult$model, arr_train)
    } else {
      pred_cols <- setdiff(names(train_data), res_var)
      predict(modelResult$model, newdata = train_data[, pred_cols, drop = FALSE])
    }
  }, error = function(e) e)
  if (inherits(pred_cal, "error")) {
    return(fail(paste("Error computing calibration metrics:", conditionMessage(pred_cal))))
  }
  pred_cal <- as.numeric(pred_cal)

  # ---- Calibration metrics ----
  r_val  <- cor(obs_cal, pred_cal, use = "complete.obs")
  r2     <- r_val^2
  me     <- mean(pred_cal - obs_cal, na.rm = TRUE)
  rmse   <- sqrt(mean((pred_cal - obs_cal)^2, na.rm = TRUE))
  sd_obs <- sd(obs_cal, na.rm = TRUE)
  rpd    <- sd_obs / rmse
  rpiq   <- IQR(obs_cal, na.rm = TRUE) / rmse
  ccc    <- (2 * r_val * sd_obs * sd(pred_cal, na.rm = TRUE)) /
    (sd_obs^2 + var(pred_cal, na.rm = TRUE) + (mean(obs_cal) - mean(pred_cal))^2)

  metrics_df <- data.frame(
    Metric = c("R2", "RMSE", "ME", "RPD", "RPIQ", "CCC"),
    Value  = round(c(r2, rmse, me, rpd, rpiq, ccc), 4),
    stringsAsFactors = FALSE
  )

  # ---- Predict the user's samples ----
  if (!is.null(modelResult$pca)) {
    user_pca <- tryCatch(
      predict(modelResult$pca, newdata = user_matrix)[, 1:modelResult$num_components, drop = FALSE],
      error = function(e) e)
    if (inherits(user_pca, "error")) {
      return(fail(paste("Error projecting user data for prediction:", conditionMessage(user_pca))))
    }
    preds <- tryCatch(round(predict(modelResult$model, newdata = as.data.frame(user_pca)), 2),
                      error = function(e) e)
    if (inherits(preds, "error")) {
      return(fail(paste("Error predicting with PCA-based model:", conditionMessage(preds))))
    }
  } else if (model_type == "pls") {
    preds <- tryCatch(round(predict(modelResult$model, newdata = as.data.frame(user_matrix)), 2),
                      error = function(e) e)
    if (inherits(preds, "error")) {
      return(fail(paste("Error predicting with PLS model:", conditionMessage(preds))))
    }
  } else if (model_type == "cnn") {
    x_array <- tryCatch(
      array_reshape(user_spectra, c(nrow(user_spectra), ncol(user_spectra), 1)),
      error = function(e) e)
    if (inherits(x_array, "error")) {
      return(fail(paste("Error reshaping user data for CNN:", conditionMessage(x_array))))
    }
    preds <- tryCatch(round(predict(modelResult$model, x_array), 2), error = function(e) e)
    if (inherits(preds, "error")) {
      return(fail(paste("Error predicting with CNN model:", conditionMessage(preds))))
    }
  }

  list(ok = TRUE, metrics = metrics_df, preds = as.numeric(preds),
       obs_cal = obs_cal, pred_cal = pred_cal)
}

# Starts knn_train_and_predict() in a background training slot and returns a
# promise, for shiny::ExtendedTask. If background training isn't available
# (see background_tasks.R), it trains here in the main process instead.
start_knn_training <- function(args) {
  if (isTRUE(background_training_available)) {
    mirai::mirai(do.call(f, args), f = knn_train_and_predict, args = args)
  } else {
    promises::promise_resolve(do.call(knn_train_and_predict, args))
  }
}

# knn_models.R

source("modules/extraction_methods.R")

knnUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
  column(
         width = 4,
    verticalLayout(
        card(
          card_header("Instructions"),
          card_body(
            h2("Instructions:"),
            tags$ul(
            tags$li("Upload a CSV file containing spectral data, downloaded from Data Preprocessing"),
            tags$li("Select the soil property you want to predict"),
            tags$li("Train based on distance or # of neighbors. Use distance if you require larger training datasets, or neighbors if you prefer smaller datasets"),
            tags$li("Select the model type you want to use. Choose between Cubist, RF, SVM, PLS, or CNN"),
            tags$li("For more information, see ",
                    tags$a(
                      "User Guide",
                      href = "#",
                      style = "color:#0000EE;",
                      onclick = sprintf(
                        "Shiny.setInputValue('%s', Math.random()); return false;", 
                        ns("goto_user_guide")
                      )
                    ))
            )
          )
        ),
        card(    
          height = "250px",
          card_header("Data Input"),
          card_body(
            fileInput(
              ns("file1"),
              "Upload CSV File",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
            ),
          )
        ),
        card(
          card_header("Train Settings"),
            verticalLayout(
              selectizeInput(
                ns("soilProperty"),
                "Select Soil Property to Predict",
                choices = c(  "Sand        " = "Sand",
                              "Silt         " = "Silt",
                              "Clay         " = "Clay",
                              "Aggregate Stability" = "AS",
                              "Bulk Density" = "BD",
                              "pH        " = "pH",
                              "Electrical Conductivity" = "EC",
                              "Total Carbon" = "TC",
                              "ESOC" = "ESOC",
                              # "Organic Carbon" = "SOC",
                              # "Carbon (pom)" = "C_pom",
                              "Carbon (hpom)" = "hpom",
                              "Carbon (maom)" = "maom",
                              # "Carbon (pom mineral)" = "C_pom_mineral",
                              "Total Nitrogen" = "TN",
                              "Phosphorus (Olsen)" = "P_Olsen",
                              "Phosphorus (Bray)" = "P_Bray",
                              "Phosphorus (Mehlich)" = "P_Mehlich3",
                              "Total Sulfur" = "TS",
                              "CEC         " = "CEC",
                              "Potassium       " = "K",
                              "Carbonate      " = "Carbonate",
                              "Gypsum" = "Gypsum",
                              "Select a Soil Property" = ""),
                selected = ""
              ),
              uiOutput(ns("extraction_methods")),
              verticalLayout(
                radioButtons(ns("knnType"),
                             "Train based on distance or # of neighbors?",
                             choices = c("Distance", "Neighbors"),
                             selected = "Distance",
                             inline = TRUE),
                uiOutput(ns("knnSliderUI"))%>% withSpinner(type = 1, color = "#3734eb", hide.ui = FALSE)
              ), 
              
              selectizeInput(
                ns("modelType"),
                "Select Model Type",
                choices = c("Cubist" = "cb", 
                            "Random Forest" = "rf", 
                            "Support Vector Machine" = "svm", 
                            "Partial Least Squares" = "pls",
                            "CNN" = "cnn"),
                selected = "Cubist"
              ),
              
              # Disables itself and shows the busy label while the model
              # trains in the background (bound to the task in knnServer).
              bslib::input_task_button(
                ns("predict"), "Make Prediction",
                label_busy = "Training in the background...",
                type = "default"
              ),
              downloadButton(ns("downloadData"), "Download Predictions & Metadata")
            )
        ),


        )),
  column(width = 8,
      verticalLayout(
        card(
          card_header("PCA Plot"),
          plotOutput(ns("knn_pcaPlot"))
          
        ),

        card(
          card_header("Internal Calibration Metrics"),
          card_body(
            div(
              style = "overflow-x:auto;",          # enable horizontal scroll on phones
              tableOutput(ns("calib_metrics"))
            )
          )
        ),
        layout_column_wrap(
          ncol = 2,          # ← two equal columns
          gap  = "16px",     # ← optional gutter
          
          # left card ─ predictions table
          card(
            card_header("Predictions"),
            DT::dataTableOutput(ns("pred_table"))
          ),
          
          # right card ─ 1:1 plot
          card(
            card_header("Calibration: Observed vs Predicted"),
            plotOutput(ns("calib_scatter"))
          )
        )
      )
  )
    )
  )
}

knnServer <- function(id, shared, load_spectral_data_memo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

    
    observeEvent(c(shared$page_navbar, input$soilProperty),{
      if (shared$page_navbar == "knn_model") {
     if (input$soilProperty != "") {
      shared$extraction_method <- input$soilProperty
     } else {
       shared$extraction_method <- NULL
     }
      }
    })
    
    output$extraction_methods <- renderUI({
      extraction_methodsUI("extraction_methods")
    })
    
    extraction_methodsServer("extraction_methods", shared)
    
    ## keep any tidbits we want to echo into metadata.txt
    metaRV <- reactiveValues(
      metrics        = NULL,   # data‑frame from calibMetrics()
      n_neighbors    = NA,     # how many rows used to train
      knn_type       = NA,     # "Distance" / "Neighbors"
      knn_value      = NA,     # threshold or k
      property       = NULL,   # soil property the predictions were trained for
      model_type     = NULL    # model type the predictions were trained with
    )
    
    trainPlotData <- reactiveVal(NULL)
    
    observeEvent(input$goto_user_guide, {
      # just flip a shared flag
      shared$goto_user_guide <- Sys.time()
      
    })
    
    #reactive to store calib metrics
    calibMetrics <- reactiveVal(data.frame(
      Metric = character(0),
      Value = numeric(0),
      stringsAsFactors = FALSE
    ))
    
    #Output said error metrics
    output$calib_metrics <- renderTable({
      df <- calibMetrics()
      validate(need(nrow(df) > 0, "No metrics available"))
      
      ## transpose so metrics run left‑to‑right
      wide <- as.data.frame(t(df$Value),       # make the values the single row
                            stringsAsFactors = FALSE)
      colnames(wide) <- df$Metric              # column names = metric labels
      wide                                           # one row, many columns
    }, rownames = FALSE)
    
    
    # Reactive to compute calibration distances for the slider input.
    calibDistances <- reactive({
      req(input$file1, input$soilProperty)
      
      tryCatch({
        obj <- get_pretrained_pca(input$soilProperty)
        pca_res        <- obj$pca
        calib_scores_2d <- obj$calib_scores_2d
        spec_cols      <- obj$spec_cols
        
        # Read user-uploaded data
        user_df <- tryCatch({
          read.csv(input$file1$datapath, check.names = FALSE)
        }, error = function(e) {
          showNotification(paste("Error reading user CSV file:", e$message), type = "error")
          return(NULL)
        })
        if (is.null(user_df)) return(list(min = 0, max = 10, default = 4))
        
        # Same spectral columns as training PCA
        user_mat <- as.matrix(user_df[, spec_cols, drop = FALSE])
        user_mat <- matrix(
          as.numeric(user_mat),
          nrow = nrow(user_mat),
          dimnames = list(NULL, spec_cols)
        )
        
        user_scores <- as.data.frame(
          predict(pca_res, newdata = user_mat)[, 1:2, drop = FALSE]
        )
        
        neighbors <- get.knnx(
          data  = as.matrix(calib_scores_2d),
          query = as.matrix(user_scores),
          k     = nrow(calib_scores_2d)
        )
        
        min_dist    <- round(min(neighbors$nn.dist, na.rm = TRUE), 2)
        max_dist    <- round(max(neighbors$nn.dist, na.rm = TRUE), 2)
        default_val <- round((min_dist + max_dist) / 2, 2)
        
        list(min = min_dist, max = max_dist, default = default_val)
        
      }, error = function(e) {
        showNotification(paste("Error computing calibration distances:", e$message), type = "error")
        list(min = 0, max = 10, default = 4)
      })
    })
    
    
    output$knnSliderUI <- renderUI({
      req(input$knnType)
      if (input$knnType == "Distance") {
        distances <- calibDistances()
        sliderInput(ns("knnSlider"), "Distance Threshold", 
                    min = distances$min, 
                    max = distances$max, 
                    value = distances$default, 
                    step = 0.01)
      } else {
        sliderInput(ns("knnSlider"), "Number of Neighbors", min = 1, max = 10, value = 5)
      }
    })
    
    # Reactive to read user-uploaded CSV data with error handling.
    user_data <- reactive({
      req(input$file1)
      tryCatch({
        read.csv(input$file1$datapath, check.names = FALSE)
      }, error = function(e) {
        showNotification(paste("Error reading user data:", e$message), type = "error")
        return(NULL)
      })
    })
    
    output$downloadData <- downloadHandler(
      filename = function() sprintf("predictions_%s.zip", Sys.Date()),
      
      content = function(file) {
        
        ## ---------------------------------------------
        ## 1.  Write the predictions CSV
        ## ---------------------------------------------
        tmpdir <- tempdir()
        pred_file <- file.path(tmpdir, "predictions.csv")
        write.csv(shared$preds, pred_file, row.names = FALSE)
        
        ## ---------------------------------------------
        ## 2.  Assemble metadata
        ## ---------------------------------------------
        m <- metaRV$metrics %||% data.frame()           # safe NULL‑to‑empty
        metrics_str <- if (nrow(m)) {
          paste(
            "\nInternal calibration metrics:",
            paste(apply(m, 1, function(r)
              sprintf("\n      • %s : %s", r["Metric"], r["Value"])), collapse = "")
          )
        } else ""
        
        knn_str <- sprintf(
          "\nKNN selection : %s = %s (neighbors used: %d)",
          metaRV$knn_type, metaRV$knn_value, metaRV$n_neighbors
        )
        
        meta <- paste(
          "Project : MIR KNN predictions",
          "\nDate    :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          # What was actually trained (the inputs may have changed since)
          "\nSoil property predicted :", metaRV$property %||% input$soilProperty,
          "\nModel type              :", metaRV$model_type %||% input$modelType,
          knn_str,
          metrics_str,
          "\nSoftware : R", getRversion(), "(caret, randomForest, etc.)"
        )
        
        meta_file <- file.path(tmpdir, "metadata.txt")
        writeLines(meta, meta_file)
        
        ## ---------------------------------------------
        ## 3.  Zip and stream
        ## ---------------------------------------------
        old <- setwd(tmpdir); on.exit(setwd(old), add = TRUE)
        zip(zipfile = file, files = c("predictions.csv", "metadata.txt"))
      },
      
      contentType = "application/zip"
    )
    
    
    # The model training functions now live in knn_train_and_predict()
    # (top of this file), which runs in a background process.


    # 1) Reactive: load & preprocess calibration + user data, run PCA, project user
    pcaData <- reactive({
      req(input$file1, input$soilProperty)
      
      # Load pretrained PCA object for this soil property
      obj <- get_pretrained_pca(input$soilProperty)
      pca_res        <- obj$pca
      calib_scores_2d <- obj$calib_scores_2d
      spec_cols      <- obj$spec_cols
      
      # User-uploaded data, already preprocessed (from Data Preprocessing)
      user_df <- read.csv(input$file1$datapath, check.names = FALSE)
      
      # Ensure same spectral columns / ordering as used in training PCA
      user_mat <- as.matrix(user_df[, spec_cols, drop = FALSE])
      user_mat <- matrix(
        as.numeric(user_mat),
        nrow = nrow(user_mat),
        dimnames = list(NULL, spec_cols)
      )
      
      # Project to PCA space (fast)
      user_proj <- predict(pca_res, newdata = user_mat)[, 1:2, drop = FALSE]
      user_scores <- as.data.frame(user_proj)
      
      list(
        pca_res      = pca_res,
        calib_scores = calib_scores_2d,
        user_scores  = user_scores
      )
    })
    
    
    # 2) Reactive: pick neighbor indices based on type + slider
    neighborIdx <- reactive({
      pd <- pcaData()
      validate(need(nrow(pd$calib_scores) > 0, "No calibration scores"))
      
      if (input$knnType == "Neighbors") {
        k  <- input$knnSlider
        nn <- get.knnx(data  = pd$calib_scores,
                       query = pd$user_scores,
                       k     = k)
        unique(as.vector(nn$nn.index))
        
      } else {                           # ---- Distance mode ----
        thresh <- input$knnSlider
        
        nn <- get.knnx(
          data  = pd$calib_scores,
          query = pd$user_scores,
          k     = nrow(pd$calib_scores)
        )
        
        ## hits = [user,row] pairs whose distance ≤ threshold
        idx_mat <- which(nn$nn.dist <= thresh, arr.ind = TRUE)
        
        ## If no cells met the threshold, stop 
        validate(need(nrow(idx_mat) > 0,
                      "No neighbors within that distance"))
        
        ## Second column = column indices (= calibration rows)
        unique(idx_mat[, 2])
      }
    })
    
    # 3) Render the live PCA + neighbor overlay
    output$knn_pcaPlot <- renderPlot({
      pd  <- pcaData()
      idx <- neighborIdx()
      
      dfCal  <- pd$calib_scores %>% mutate(type = "Calibration")
      dfNbr  <- dfCal[idx, ]    %>% mutate(type = "Selected Neighbors")
      dfUser <- pd$user_scores  %>% mutate(type = "User Data")
      
      ggplot() +
        geom_point(data = dfCal,  aes(PC1, PC2, color = type), alpha = .6) +
        geom_point(data = dfNbr,  aes(PC1, PC2, color = type), size = 3) +
        geom_point(data = dfUser, aes(PC1, PC2, color = type), size = 3) +
        scale_color_manual(name = "Legend",
                           values = c(
                             "Calibration"        = "black",
                             "Selected Neighbors" = "gold",
                             "User Data"          = "red"
                           )) +
        guides(color = guide_legend(override.aes = list(size = 5))) +  # ← bigger keys
        theme(
          legend.title = element_text(size = 16),  # ← larger title
          legend.text  = element_text(size = 14)   # ← larger labels
        ) +
        labs(title = "PCA: Calibration / Neighbors / User",
             x = "PC1", y = "PC2")
      
    })

    ##############################################################
    # Predict: prepare here (fast), train in the background, then
    # show the results when the background job finishes.
    ##############################################################

    # The background job (knn_train_and_predict via start_knn_training).
    # Bound to the Predict button, which stays disabled while it runs.
    train_task <- ExtendedTask$new(start_knn_training)
    bslib::bind_task_button(train_task, "predict")

    # Settings the running job was started with, so the results and the
    # metadata download match what was trained even if the user changes
    # the inputs while waiting.
    job_info <- reactiveVal(NULL)

    observeEvent(input$predict, {
      req(input$soilProperty, user_data())
      withProgress(message = "Preparing training data...", value = 0, {
        tryCatch({

          res_var <- "calc_value"

          incProgress(0.1, detail = "Loading calibration data...")
          # Calibration metadata + spectra already preprocessed (Savitzky-Golay
          # -> resample to 10 cm^-1 -> SNV) from the shared calibration bundle
          # (calibration_data.R), instead of reloading and reprocessing the
          # raw file on every click.
          bundle <- load_calibration(input$soilProperty)
          validate(
            need(!is.null(bundle) && nrow(bundle$meta) > 0,
                 "No calibration data found or it is empty.")
          )
          soil    <- bundle$meta
          MIR.snv <- bundle$snv

          calib_df <- as.data.frame(MIR.snv)
          if ("calc_value" %in% colnames(soil)) {
            calib_df$calc_value <- soil[["calc_value"]]
          }

          incProgress(0.2, detail = "Loading pretrained PCA...")
          # Load pretrained PCA for this soil property
          obj     <- get_pretrained_pca(input$soilProperty)
          pca_res <- obj$pca
          spec_cols <- obj$spec_cols
          
          incProgress(0.1, detail = "Projecting user data into PCA space...")
          
          # User matrix must match spec_cols
          user_matrix <- tryCatch({
            as.matrix(user_data()[, spec_cols, drop = FALSE])
          }, error = function(e) {
            showNotification(paste("Error processing user data:", e$message), type = "error")
            return(NULL)
          })
          user_matrix <- matrix(
            as.numeric(user_matrix), 
            nrow = nrow(user_matrix), 
            dimnames = list(NULL, spec_cols)
          )
          
          user_pca_scores <- tryCatch({
            predict(pca_res, newdata = user_matrix)
          }, error = function(e) {
            showNotification(paste("Error projecting user data into PCA space:", e$message), type = "error")
            return(NULL)
          })
          
          calib_pca_scores <- pca_res$x
          
          incProgress(0.1, detail = "Selecting neighbors...")
          # Use KNN to select calibration rows.
          if (input$knnType == "Neighbors") {
            k <- input$knnSlider
            neighbors <- tryCatch({
              get.knnx(data = calib_pca_scores, query = user_pca_scores, k = k)
            }, error = function(e) {
              showNotification(paste("Error finding neighbors:", e$message), type = "error")
              return(NULL)
            })
            # Use all neighbor indices (duplicates allowed if same calibration point is selected for multiple user points)
            all_neighbor_indices <- as.vector(neighbors$nn.index)
            print(paste("Number of neighbors per user point:", k))
          } else {
            neighbors <- tryCatch({
              get.knnx(data = calib_pca_scores, query = user_pca_scores, k = nrow(calib_pca_scores))
            }, error = function(e) {
              showNotification(paste("Error computing distances:", e$message), type = "error")
              return(NULL)
            })
            threshold <- input$knnSlider
            all_neighbor_indices <- tryCatch({
              valid_indices <- c()
              for (user_i in seq_len(nrow(user_pca_scores))) {
                row_dists <- neighbors$nn.dist[user_i, ]
                row_idxs  <- neighbors$nn.index[user_i, ]
                good_cols <- which(row_dists <= threshold)
                if (length(good_cols) > 0) {
                  valid_indices <- c(valid_indices, row_idxs[good_cols])
                }
              }
              neighbor_indices <- unique(valid_indices)
              if (length(neighbor_indices) == 0) {
                stop("No neighbors selected with the given distance threshold.")
              }
              neighbor_indices
            }, error = function(e) {
              showNotification(e$message, type = "error")
              return(NULL)
            })
            print(paste("Number of selected neighbors:", length(all_neighbor_indices)))
          }
          
          incProgress(0.1, detail = "Building training data...")
          # Subset calibration data.
          if ("calc_value" %in% colnames(calib_df)) {
            sub_soil <- soil[all_neighbor_indices, ]
            sub_spec <- MIR.snv[all_neighbor_indices, ]
            complete_rows <- complete.cases(sub_soil[["calc_value"]])
            sub_soil <- sub_soil[complete_rows, ]
            sub_spec <- sub_spec[complete_rows, ]
          } else {
            showNotification("Response variable not found in calibration data", type = "error")
            return()
          }
          
          trainData <- tryCatch({
            cbind(as.data.frame(sub_spec), calc_value = sub_soil[["calc_value"]])
          }, error = function(e) {
            showNotification(paste("Error building training data:", e$message), type = "error")
            return(NULL)
          })
          
          # CNN uses every spectral column of the upload: everything after
          # the first column, which is the sample ID whatever it's named
          # ("scan_path_name" from Data Preprocessing, "SampleID" from
          # Spectral Transformation).
          user_spectra <- NULL
          if (input$modelType == "cnn") {
            user_spectra <- tryCatch({
              as.matrix(user_data()[, names(user_data())[-1], drop = FALSE])
            }, error = function(e) {
              showNotification(paste("Error subsetting user predictors:", e$message), type = "error")
              return(NULL)
            })
            if (is.null(user_spectra)) return()
          }

          incProgress(0.2, detail = "Starting training in the background...")
          job_info(list(
            sample_ids  = user_data()[[1]],
            property    = input$soilProperty,
            model_type  = input$modelType,
            knn_type    = input$knnType,
            knn_value   = input$knnSlider,
            n_neighbors = length(all_neighbor_indices)
          ))
          train_task$invoke(list(
            train_data   = trainData,
            user_matrix  = user_matrix,
            user_spectra = user_spectra,
            model_type   = input$modelType
          ))

        }, error = function(e) {
          showNotification(paste("Error during model training and prediction:", e$message), type = "error")
        })
      })
    })

    # Show the results when the background job finishes.
    observeEvent(train_task$status(), {
      status <- train_task$status()
      if (status == "error") {
        # The background process itself failed (e.g. it was stopped or ran
        # out of memory); errors during training come back as ok = FALSE.
        msg <- tryCatch({ train_task$result(); "unknown error" },
                        error = function(e) conditionMessage(e))
        showNotification(paste("Error during model training and prediction:", msg), type = "error")
        return()
      }
      if (status != "success") return()

      res  <- train_task$result()
      info <- job_info()
      if (!isTRUE(res$ok)) {
        showNotification(res$error, type = "error")
        return()
      }
      showNotification("Model training complete!", type = "message")

      calibMetrics(res$metrics)
      metaRV$metrics     <- res$metrics
      metaRV$n_neighbors <- info$n_neighbors
      metaRV$knn_type    <- info$knn_type
      metaRV$knn_value   <- info$knn_value
      metaRV$property    <- info$property
      metaRV$model_type  <- info$model_type
      trainPlotData(data.frame(obs = res$obs_cal, pred = res$pred_cal))

      pred_df <- tryCatch({
        # Sample IDs come from the first column, whatever it's named
        # (same approach as the Static Models page).
        data.frame("Sample Name" = info$sample_ids, Prediction = res$preds)
      }, error = function(e) {
        showNotification(paste("Error building predictions dataframe:", e$message), type = "error")
        return(NULL)
      })
      if (is.null(pred_df)) return()

      shared$preds <- pred_df
      output$pred_table <- DT::renderDataTable({
        DT::datatable(pred_df)
      })
    })
    
    output$calib_scatter <- renderPlot({
      df <- trainPlotData()
      req(df)
      
      ggplot(df, aes(obs, pred)) +
        geom_point(color = "red", alpha = 0.65, size = 2) +   # ← red points
        geom_abline(slope = 1, intercept = 0,
                    linetype = "dashed", linewidth = 1) +
        coord_equal() +
        labs(x = "Observed", y = "Predicted") +
        theme_minimal(base_size = 14)
    })
    
  })
}
