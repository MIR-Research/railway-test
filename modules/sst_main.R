library(shiny)
library(bslib)
# NOTE: uses MASS::ginv (namespaced) so we don't attach MASS and mask dplyr::select.

# =====================================================================
# Spectral Space Transformation (SST) - Shiny module
#
# Learns a transform from PAIRED spectra measured on two instruments
# (primary = reference space, secondary = to be transformed), applies it
# to the secondary spectra, shows diagnostic plots, and offers a download
# of the transformed spectra with sample IDs.
#
# Requirement: the primary and secondary files must contain the SAME
# samples in the SAME row order, measured at the SAME wavelengths.
# =====================================================================

# ---------------------------------------------------------------------
# Core SST computation (ported from the prototype script)
# ---------------------------------------------------------------------
compute_sst <- function(primary_spec, secondary_spec,
                        mean_center = TRUE, var_threshold = 0.9999) {
  P <- as.matrix(primary_spec)
  S <- as.matrix(secondary_spec)
  
  mean_P <- colMeans(P)
  mean_S <- colMeans(S)
  
  # Step 1: optional mean-centering for the SVD
  if (isTRUE(mean_center)) {
    mP <- sweep(P, 2, mean_P, "-")
    mS <- sweep(S, 2, mean_S, "-")
  } else {
    mP <- P
    mS <- S
  }
  
  # Step 2: concatenate and SVD
  p    <- ncol(mP)
  comb <- cbind(mP, mS)
  sv   <- svd(comb)
  
  # Step 3: choose number of components by cumulative variance
  var_explained <- cumsum(sv$d^2) / sum(sv$d^2)
  ncomp <- which(var_explained >= var_threshold)[1]
  if (is.na(ncomp)) ncomp <- length(sv$d)
  
  # Step 4: split V into instrument halves
  p1 <- sv$v[1:p,           1:ncomp, drop = FALSE]
  p2 <- sv$v[(p + 1):(2 * p), 1:ncomp, drop = FALSE]
  
  # Step 5: transformation matrix (MATLAB-style)
  stdmat <- diag(p) + MASS::ginv(t(p2)) %*% (t(p1) - t(p2))
  
  # Step 6: additive background-correction vector
  stdvect <- mean_P - mean_S %*% stdmat
  
  # Step 7: transform the (original) secondary spectra
  sst <- sweep(S %*% stdmat, 2, as.numeric(stdvect), "+")
  colnames(sst) <- colnames(P)
  
  list(
    sst           = sst,
    stdmat        = stdmat,
    stdvect       = stdvect,
    ncomp         = ncomp,
    var_explained = var_explained,
    primary       = P,
    secondary     = S
  )
}

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
sstUI <- function(id) {
  ns <- NS(id)
  
  card(
    full_screen = TRUE,
    card_header("Spectral Space Transformation (SST)"),
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        fileInput(ns("primary_file"),
                  "Primary instrument CSV (reference space)",
                  accept = ".csv"),
        fileInput(ns("secondary_file"),
                  "Secondary instrument CSV (to transform)",
                  accept = ".csv"),
        numericInput(ns("id_col"),
                     "Sample ID column", value = 1, min = 1, step = 1),
        numericInput(ns("first_spec_col"),
                     "First spectral column", value = 6, min = 1, step = 1),
        numericInput(ns("last_spec_col"),
                     "Last spectral column (0 = to end)", value = 0, min = 0, step = 1),
        checkboxInput(ns("mean_center"),
                      "Mean-center before SVD", value = TRUE),
        numericInput(ns("var_threshold"),
                     "Variance threshold for components",
                     value = 0.9999, min = 0.5, max = 1, step = 0.0001),
        numericInput(ns("preview_row"),
                     "Preview sample (row #)", value = 1, min = 1, step = 1),
        actionButton(ns("run"), "Run SST", class = "btn-primary"),
        downloadButton(ns("download"), "Download transformed spectra")
      ),
      
      textOutput(ns("status")),
      navset_card_tab(
        nav_panel("Mean \u00B1 SD",   plotOutput(ns("mean_sd_plot"), height = "460px")),
        nav_panel("Single sample",    plotOutput(ns("sample_plot"),  height = "460px"))
      )
    )
  )
}

# ---------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------
sstServer <- function(id, shared = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # read a CSV and strip the leading 'X' R adds to numeric column names
    read_instrument <- function(path) {
      df <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
      names(df) <- gsub("^X(\\d)", "\\1", names(df))
      df
    }
    
    primary_df <- reactive({
      req(input$primary_file)
      read_instrument(input$primary_file$datapath)
    })
    
    secondary_df <- reactive({
      req(input$secondary_file)
      read_instrument(input$secondary_file$datapath)
    })
    
    spec_cols <- function(df) {
      first <- input$first_spec_col
      last  <- if (is.null(input$last_spec_col) || input$last_spec_col < 1) {
        ncol(df)
      } else {
        input$last_spec_col
      }
      first:last
    }
    
    # compute only when the user clicks Run
    sst_result <- eventReactive(input$run, {
      p_df <- primary_df()
      s_df <- secondary_df()
      
      pc <- spec_cols(p_df)
      sc <- spec_cols(s_df)
      
      validate(
        need(max(pc) <= ncol(p_df) && max(sc) <= ncol(s_df),
             "Spectral column range is outside the uploaded data."),
        need(length(pc) == length(sc),
             "Primary and secondary have different numbers of spectral columns."),
        need(nrow(p_df) == nrow(s_df),
             "Primary and secondary must have the same number of samples (paired rows).")
      )
      
      P <- as.matrix(p_df[, pc, drop = FALSE])
      S <- as.matrix(s_df[, sc, drop = FALSE])
      
      validate(
        need(all(is.finite(P)) && all(is.finite(S)),
             "Spectral columns contain non-numeric or missing values.")
      )
      
      res <- compute_sst(P, S,
                         mean_center   = isTRUE(input$mean_center),
                         var_threshold = input$var_threshold)
      
      showNotification(
        sprintf("SST: retained %d components (%.4f%% variance)",
                res$ncomp, res$var_explained[res$ncomp] * 100),
        type = "message"
      )
      res
    })
    
    output$status <- renderText({
      res <- sst_result()
      sprintf("Retained %d components (%.4f%% cumulative variance). Transformed %d samples across %d bands.",
              res$ncomp, res$var_explained[res$ncomp] * 100,
              nrow(res$sst), ncol(res$sst))
    })
    
    # ---- Mean +/- SD diagnostic ----
    output$mean_sd_plot <- renderPlot({
      res <- sst_result()
      P <- res$primary; S <- res$secondary; SST <- res$sst
      
      wav <- suppressWarnings(as.numeric(colnames(P)))
      if (any(is.na(wav))) wav <- seq_len(ncol(P))
      
      mean_P <- colMeans(P); mean_S <- colMeans(S); mean_T <- colMeans(SST)
      sd_P <- apply(P, 2, sd); sd_S <- apply(S, 2, sd); sd_T <- apply(SST, 2, sd)
      
      y_range <- range(c(mean_P + sd_P, mean_P - sd_P,
                         mean_S + sd_S, mean_S - sd_S,
                         mean_T + sd_T, mean_T - sd_T), na.rm = TRUE)
      
      draw_band <- function(w, m, s, col) {
        polygon(c(w, rev(w)), c(m + s, rev(m - s)),
                col = adjustcolor(col, alpha.f = 0.15), border = NA)
      }
      
      plot(wav, mean_P, type = "n",
           xlim = rev(range(wav)), ylim = y_range,
           main = "Mean \u00B1 SD: Primary (black) | Secondary raw (red) | Secondary SST (blue)",
           xlab = "Wavenumber", ylab = "Absorbance")
      draw_band(wav, mean_P, sd_P, "black")
      draw_band(wav, mean_S, sd_S, "red")
      draw_band(wav, mean_T, sd_T, "blue")
      lines(wav, mean_P, col = "black", lwd = 2)
      lines(wav, mean_S, col = "red",   lwd = 2, lty = 2)
      lines(wav, mean_T, col = "blue",  lwd = 2)
      legend("topright",
             legend = c("Primary", "Secondary (raw)", "Secondary (SST)"),
             col = c("black", "red", "blue"), lty = c(1, 2, 1), lwd = 2,
             fill = adjustcolor(c("black", "red", "blue"), alpha.f = 0.15),
             border = NA)
    })
    
    # ---- Single-sample overlay ----
    output$sample_plot <- renderPlot({
      res <- sst_result()
      P <- res$primary; S <- res$secondary; SST <- res$sst
      
      i <- input$preview_row
      if (is.null(i) || i < 1 || i > nrow(P)) i <- 1
      
      wav <- suppressWarnings(as.numeric(colnames(P)))
      if (any(is.na(wav))) wav <- seq_len(ncol(P))
      
      yr <- range(c(P[i, ], S[i, ], SST[i, ]), na.rm = TRUE)
      plot(wav, P[i, ], type = "l", col = "black", lwd = 1.5,
           xlim = rev(range(wav)), ylim = yr,
           main = paste("Sample row", i),
           xlab = "Wavenumber", ylab = "Absorbance")
      lines(wav, S[i, ],   col = "red",  lwd = 1, lty = 2)
      lines(wav, SST[i, ], col = "blue", lwd = 1.5)
      legend("topright",
             legend = c("Primary", "Secondary (raw)", "Secondary (SST)"),
             col = c("black", "red", "blue"), lty = c(1, 2, 1), lwd = 1.5)
    })
    
    # ---- Download transformed spectra (sample ID + transformed bands) ----
    output$download <- downloadHandler(
      filename = function() paste0("secondary_SST_", Sys.Date(), ".csv"),
      content = function(file) {
        res  <- sst_result()
        s_df <- secondary_df()
        ids  <- s_df[[input$id_col]]
        out  <- data.frame(SampleID = ids, res$sst, check.names = FALSE)
        write.csv(out, file, row.names = FALSE)
      }
    )
    
    # Return the result reactive so the rest of the app can consume it later
    # (e.g. feed transformed spectra straight into the prediction pipeline).
    return(sst_result)
  })
}

# ---------------------------------------------------------------------
# Standalone test app  --  DELETE this block when integrating into the site
# ---------------------------------------------------------------------
# if (interactive()) {
#   ui <- page_fillable(
#     theme = bs_theme(version = 5, bootswatch = "cosmo"),
#     sstUI("sst")
#   )
#   server <- function(input, output, session) {
#     options(shiny.maxRequestSize = 500 * 1024^2)
#     sstServer("sst")
#   }
#   shinyApp(ui, server)
# }