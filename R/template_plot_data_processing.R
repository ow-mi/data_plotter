# Plot-specific Data Processing Template
# For Plot-specific Data Processing (ui_plotter)

r_code_plot_process_template <- "# R code to process the COMBINED data specifically for THIS plot.
# Available variables:
# - df: The data.table of combined data from all importers.
# - input: Reactive list of UI inputs for THIS plotter (e.g., input$sample_n_plot, input$filter_in_plot).
# - filter_in(), filter_out(), rname(): Helper functions.
# Required output: A data.table for plotting.

# Ensure df is a data.table
setDT(df)

# Sample data if sample_n_plot is set (0 means use all data)
if (!is.null(input$sample_n_plot) && input$sample_n_plot > 0 && input$sample_n_plot < 1) {
  if (nrow(df) * input$sample_n_plot >= 1) { # Ensure sample size is at least 1
    df <- df[sample(.N, floor(input$sample_n_plot * .N))]
  }
}

# Filter series for this plot
if (!is.null(input$filter_in_files_plot) && nzchar(input$filter_in_files_plot)) {
  df <- filter_in(df, 'file_name_source', input$filter_in_files_plot)
}

if (!is.null(input$filter_out_files_plot) && nzchar(input$filter_out_files_plot)) {
  df <- filter_out(df, 'file_name_source', input$filter_out_files_plot)
}

# Filter series for this plot
if (!is.null(input$filter_in_plot) && nzchar(input$filter_in_plot)) {
  df <- filter_in(df, 'series', input$filter_in_plot)
}
if (!is.null(input$filter_out_plot) && nzchar(input$filter_out_plot)) {
  df <- filter_out(df, 'series', input$filter_out_plot)
}

# Rename series for this plot
if (!is.null(input$rename_plot) && nzchar(input$rename_plot)) {
  df <- rname(df, 'series', input$rename_plot)
}

# Source filtering - filter by selected importer sources
if ('source_importer_id' %in% names(df)) {
    # Get all unique sources in the data
    unique_sources <- unique(df$source_importer_id)
    
    # Check which sources are selected via checkboxes
    selected_sources <- c()
    for (source_id in unique_sources) {
        checkbox_id <- paste0('source_', source_id)
        if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
            selected_sources <- c(selected_sources, source_id)
        }
    }
    
    # Filter data by selected sources
    if (length(selected_sources) > 0) {
        df <- df[source_importer_id %in% selected_sources]
    }
    # If no sources selected, keep all data (no filtering)
}

# Time-based filtering using separate start/end date and time inputs
if (input$filter_time_enabled) {
  if ('timestamp' %in% names(df)) {
      # Build start datetime
      if (!is.null(input$filter_time_start_date) && !is.null(input$filter_time_start_time)) {
          start_datetime <- as.POSIXct(paste(
              as.character(input$filter_time_start_date), 
              format(input$filter_time_start_time, '%H:%M:%S')
          ))
        
          if (!is.na(start_datetime)) {
              df <- df[timestamp >= start_datetime]
          }
      }
    
      # Build end datetime
      if (!is.null(input$filter_time_end_date) && !is.null(input$filter_time_end_time)) {
          end_datetime <- as.POSIXct(paste(
              as.character(input$filter_time_end_date), 
              format(input$filter_time_end_time, '%H:%M:%S')
          ))
        
          if (!is.na(end_datetime)) {
              df <- df[timestamp <= end_datetime]
          }
      }
  }
}

# Extract 'dut' if not already present or if needed differently for this plot
# This might be redundant if done in main processing, but allows plot-specific override.
if (!'dut' %in% names(df) && 'file_name_source' %in% names(df)) {
    df[, dut := stringr::str_extract(file_name_source, '2212[^_]*')]
}

df # Return the data processed for this specific plot
"