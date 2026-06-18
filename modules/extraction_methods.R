# extraction_methods.R

extraction_methodsUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("extraction_methods"))
  )
}

extraction_methodsServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns
    
    # Read as-is; don't convert names or factors
    dat <- read.csv("www/Analyte_Methods.csv",
                    stringsAsFactors = FALSE,
                    check.names = FALSE)
    
    # ---------- (1) property code -> long analyte_name (CSV text) ----------
    property_lookup <- c(
      AS         = "Aggregate Stability, 0.5-2mm Aggregates",
      BD         = "Bulk Density, <2mm Fraction, Ovendry",
      Carbonate  = "Carbonate, <2mm Fraction",
      TC         = "Carbon, Total",
      TN         = "Nitrogen, Total",
      pH         = "pH, 1:1 Soil-Water Suspension",
      P_Bray     = "Phosphorus, Bray-1 Extractable",
      P_Olsen    = "Phosphorus, Olsen Extractable",
      Clay       = "Clay",
      EC         = "Electrical Conductivity, Predict, 1:2 (w/w)",
      TS         = "Sulfur, Total",
      P_Mehlich3 = "Phosphorus, Mehlich3 Extractable",
      Gypsum     = "Corrected Gypsum, <2mm",
      CEC        = "CEC, NH4OAc, pH 7.0, 2M KCl displacement",
      K          = "Potassium, NH4OAc Extractable, 2M KCl displacement",
      C_hpom     = "Carbon, hpom",
      Sand       = "Sand, Total",
      Silt       = "Silt, Total",
      ESOC       = "Estimated Organic Carbon, Total C, S prep",
      Carbon_hmin= "Carbon, hmin, S Prep"
    )
    
    # ---------- (2) property code -> Analyte_Code (fallback) ----------
    analyte_code_lookup <- c(
      AS         = "3F1a1a",
      BD         = "3B1c",
      Carbonate  = "4E1a1a1a1",
      TC         = "4H2a1",
      TN         = "4H2a1",
      pH         = "4C1a2a1",
      P_Bray     = "4D3a1",
      P_Olsen    = "4D5a1",
      Clay       = "3A1a1",
      EC         = "4F1a",
      TS         = "4H2a3",
      P_Mehlich3 = "4D6a1",
      Gypsum     = "4E2a1a",
      CEC        = "4B1a1a2a1",
      K          = "4B1a1c1",
      C_hpom     = "6A4a1a1-3a1",
      Sand       = "3A1a1",
      Silt       = "3A1a1",
      ESOC       = NA_character_,           # calculated; not in manual as a single code
      Carbon_hmin= "6A4a1a1-3a3"
    )
    
    # ---------- (3) property code -> PDF file ----------
    pdf_lookup <- setNames(rep("Kellogg_Lab_Manual.pdf",
                               length(property_lookup)),
                           names(property_lookup))
    
    # String normalizer: trim, collapse spaces, lowercase, UTF-8
    norm <- function(x) {
      x <- enc2utf8(x)
      x <- trimws(x)
      x <- gsub("[\u00A0]+", " ", x)       # NBSP -> space
      x <- gsub("\\s+", " ", x)
      tolower(x)
    }
    
    analyte_name <- reactive({
      code <- req(shared$extraction_method)
      property_lookup[[code]]
    })
    
    pdf_file <- reactive({
      code <- req(shared$extraction_method)
      pdf_lookup[[code]]
    })
    
    extraction_methods <- reactive({
      nm <- analyte_name()
      code <- req(shared$extraction_method)
      
      sel_cols <- c("analyte_id", "analyte_name", "analyte_desc",
                    "Analyte_Code", "Detection_limit")
      
      # Primary: match by analyte_name (robust string compare)
      rows <- dat[norm(dat$analyte_name) == norm(nm), sel_cols, drop = FALSE]
      
      # Fallback: match by Analyte_Code mapping if name match failed
      if (nrow(rows) == 0) {
        acode <- analyte_code_lookup[[code]]
        if (!is.null(acode) && !is.na(acode)) {
          rows <- subset(dat, Analyte_Code == acode, select = sel_cols)
        }
      }
      
      if (nrow(rows) == 0) NULL else rows
    })
    
    # Render whenever property changes (including first time)
    observeEvent(shared$extraction_method, ignoreInit = FALSE, {
      # Only show on these pages
      if (!isTRUE(shared$page_navbar %in% c("knn_model", "static_models"))) {
        output$extraction_methods <- renderUI({ NULL })
        return()
      }
      
      methods <- extraction_methods()
      link    <- pdf_file()
      
      output$extraction_methods <- renderUI({
        tagList(
          if (!is.null(methods) && nrow(methods) > 0) {
            tags$ul(
              lapply(seq_len(nrow(methods)), function(i) {
                tags$li(HTML(paste0(
                  methods$analyte_id[i], "<br>",
                  methods$analyte_name[i], "<br>",
                  methods$analyte_desc[i], "<br>",
                  "KSSL Lab manual analyte code: <b>",
                  methods$Analyte_Code[i], "</b><br>",
                  "Detection limit: ", methods$Detection_limit[i]
                )))
              })
            )
          } else {
            tags$p("No extraction methods found.")
          },
          if (!is.null(link)) {
            tags$p(
              tags$a("Download full KSSL lab manual (PDF)",
                     href = link, target = "_blank",
                     style = "color:#0000EE;")
            )
          }
        )
      })
      outputOptions(output, "extraction_methods", suspendWhenHidden = FALSE)
    })
  })
}
