# Data Processing Module for Static Plots
# Modular Static Plot Templates (ggplot2) - Data Processing

ggplot_data_processing_template <- "# Data Processing for Static Plot
# Available: df (processed data), input (UI inputs)
# Purpose: Handle X-axis transformations and basic data setup

library(ggplot2)
library(stringr)
library(RColorBrewer)

# Handle X-axis transformation
plot_df <- copy(df) # Work on a copy to avoid modifying original data

if ('timestamp' %in% names(plot_df)) {
  min_ts <- min(plot_df$timestamp, na.rm = TRUE)
  
  # Transform X-axis based on input selection
  if (!is.null(input$plot_xlabel)) {
    if (input$plot_xlabel == 'Duration Minutes') {
      plot_df[, x_axis := as.numeric(difftime(timestamp, min_ts, units = 'mins'))]
      x_label <- 'Duration (Minutes)'
    } else if (input$plot_xlabel == 'Duration Hours') {
      plot_df[, x_axis := as.numeric(difftime(timestamp, min_ts, units = 'hours'))]
      x_label <- 'Duration (Hours)'
    } else if (input$plot_xlabel == 'Duration Days') {
      plot_df[, x_axis := as.numeric(difftime(timestamp, min_ts, units = 'days'))]
      x_label <- 'Duration (Days)'
    } else if (input$plot_xlabel == 'Duration Seconds') {
      plot_df[, x_axis := as.numeric(difftime(timestamp, min_ts, units = 'secs'))]
      x_label <- 'Duration (Seconds)'
    } else {
      # Default to timestamp or custom label
      plot_df[, x_axis := timestamp]
      x_label <- input$plot_xlabel
    }
  } else {
    plot_df[, x_axis := timestamp]
    x_label <- 'Timestamp'
  }
} else {
  # No timestamp column, use first numeric column or create index
  if (ncol(plot_df) > 0) {
    plot_df[, x_axis := .I] # Use row index
    x_label <- 'Index'
  }
}

plot_df # Return processed data
"