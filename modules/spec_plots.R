# modules/spec_plots.R
library(shiny)
library(prospectr)  
library(scales)

# Compare mean of user data to mean + sd of training data

specPlotsUI <- function(id) {
  ns <- NS(id)
  card(
    card_header("Spectral Plots"),
    card_body(
      plotOutput(ns("spec_plot"))%>% withSpinner(color = "#3734eb", type = 1, hide.ui = FALSE)
    )
  )
}

specPlotsServer <- function(id, shared, fs_cache, load_spectral_data_memo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # The calibration spectra, already preprocessed (Savitzky-Golay ->
    # resample to 10 cm^-1 -> SNV), come from the shared calibration bundle
    # (see calibration_data.R), so this module no longer preprocesses them.
    processed_data <- reactive({
      req(shared$selectedProperty)
      bundle <- load_calibration(shared$selectedProperty)
      if (is.null(bundle)) {
        showNotification(paste("No data for", shared$selectedProperty), type = "error")
        return(NULL)
      }
      bundle$snv
    })
    
    output$spec_plot <- renderPlot({
      data_to_plot <- processed_data()
      req(data_to_plot)
      
      wav <- as.numeric(colnames(data_to_plot))
      
      if (any(is.na(wav))) {
        showNotification("Wavelength data contains NA values.", type = "error")
        return(NULL)
      }
      
      if (nrow(data_to_plot) == 0 || ncol(data_to_plot) == 0) {
        showNotification("Processed data is empty.", type = "error")
        return(NULL)
      }
      
      # Compute mean and SD of the processed (training) data
      mean_spectrum <- colMeans(data_to_plot, na.rm = TRUE)
      sd_spectrum <- apply(data_to_plot, 2, sd, na.rm = TRUE)
      
      # Set up the plot with no data first
      plot(wav, mean_spectrum,
           type = "n",
           main = "Spectral Plots",
           xlim = rev(range(wav)),
           xlab = "Wavelength (cm^-1)",
           ylab = "Absorbance")
      
      # Shade the SD area around the mean
      polygon(x = c(wav, rev(wav)),
              y = c(mean_spectrum + sd_spectrum, rev(mean_spectrum - sd_spectrum)),
              col = scales::alpha("gray", 0.5),
              border = NA)
      
      # Draw the mean spectrum as a thick black line
      lines(wav, mean_spectrum, lwd = 2, col = "black")
      
      # Initialize a vector to store legend labels and colors
      legend_labels <- c("Mean Spectrum")
      legend_colors <- c("black")
      legend_lwd <- c(2)
      
      # Overlay user data if available and dimensions match
      if (!is.null(shared$df)) {
        user_data <- shared$df
        
        # Ensure user_data matches the wavelengths in dimension
        if (ncol(user_data) == length(wav)) {
          # Calculate the mean user spectrum
          user_mean <- colMeans(user_data, na.rm = TRUE)
          
          # Plot the user average in red
          lines(wav, user_mean, col = "red", lwd = 2)
          showNotification("User data displayed", type = "message")
          
          # Update legend vectors to include user data
          legend_labels <- c(legend_labels, "User Data")
          legend_colors <- c(legend_colors, "red")
          legend_lwd <- c(legend_lwd, 2)
        } else {
          showNotification("User data dimensions do not match processed data.", type = "error")
        }
      } else {
        showNotification("User data not available for overlay.", type = "warning")
      }
      
      # Add the legend to the plot
      legend("topright",
             legend = legend_labels,
             col = legend_colors,
             lwd = legend_lwd,
             bg = "white",
             box.lwd = 0.5,
             box.col = "black",
             cex = 0.9)
    })
  })
}


