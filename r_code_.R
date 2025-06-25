# r_code_.R: Contains default R code strings for Ace editors

# --- For Data Import Pre-processing (ui_data_importer) ---
r_code_data_pre_process <- "# R code to read and initially process a single file.
# Available variables:
# - file_path: Full path to the uploaded file.
# - file_name: Original name of the file.
# - n_every: Value from 'Read every Nth row' input (0 or 1 means all).
# - skip_rows: Value from 'Skip N rows at start' input.
# - showNotification(message, type): Function to display messages.
# Required output: A data.table or data.frame. Return NULL on error.

# Example:
# showNotification(paste('Processing:', file_name), type='message')

# Determine file extension
ext <- tools::file_ext(tolower(file_name)) # Use tolower for consistency

# Define read functions in a list for clarity
read_functions <- list(
  'csv' = function(fp, sr) data.table::fread(fp, skip = sr),
  'csv.gz' = function(fp, sr) data.table::fread(cmd = paste('gunzip -c', shQuote(fp)), skip = sr), # More robust for .csv.gz
  'gz' = function(fp, sr) data.table::fread(cmd = paste('gunzip -c', shQuote(fp)), skip = sr), # Assumes text content after gunzip
  'xlsx' = function(fp, sr) data.table::as.data.table(readxl::read_excel(fp, skip = sr)),
  'fst' = function(fp, sr) data.table::setDT(fst::read_fst(fp)), # FST doesn't typically have skip
  'parquet' = function(fp, sr) data.table::setDT(nanoparquet::read_parquet(fp)) # Parquet doesn't typically have skip
)

df <- NULL # Initialize df

if (ext %in% names(read_functions)) {
  df <- tryCatch({
    read_functions[[ext]](file_path, skip_rows)
  }, error = function(e) {
    showNotification(paste('Error reading', file_name, ':', e$message), type = 'error')
    return(NULL)
  })
} else if (ext == 'tdms') {
  showNotification('TDMS files are not directly supported. Please convert to a supported format (e.g., CSV, Parquet) first.', type = 'error', duration=10)
  return(NULL)
} else {
  showNotification(paste0('Unsupported file type: .', ext, '. Please use CSV, XLSX, FST, or Parquet.'), type = 'warning', duration=10)
  return(NULL)
}

if (is.null(df) || !is.data.frame(df)) {
  # showNotification(paste('Failed to read or convert', file_name, 'to a data frame.'), type = 'error')
  return(NULL)
}

# Ensure it's a data.table
data.table::setDT(df)

# Check row count
if (nrow(df) == 0) {
  showNotification(paste('File', file_name, 'is empty or resulted in zero rows after reading.'), type = 'warning')
  return(NULL) # Or return df if empty data.tables are acceptable downstream
}

# Downsample if n_every is greater than 1 (0 or 1 means read all relevant rows already)
# The 'n_every' input is now 'Read every Nth row (0=all)'
# So if n_every > 1, we sample. If n_every is 0 or 1, we take all.
if (exists('n_every') && is.numeric(n_every) && n_every > 1 && nrow(df) > n_every) {
  df <- df[seq(1, .N, by = as.integer(n_every))]
  showNotification(paste('Downsampled', file_name, 'to every', n_every, 'th row.'), type='message')
}

# Add file_name column for tracking provenance
df[, file_name_source := file_name] # Use a distinct name to avoid conflict if 'file_name' is a data column

df # Return the processed data.table
"

# --- For Data Import Post-processing (ui_data_importer) ---
r_code_data_processing <- "# R code to transform the data from a single file AFTER pre-processing.
# Available variables:
# - df: The data.table from the pre-processing step for the current file.
# - input_filter_in_1, input_filter_out_1, input_rename_1, input_date_format: Values from UI controls.
# - showNotification(message, type): Function.
# - filter_in(), filter_out(), rname(): Helper functions.
# Required output: A data.table.

# Ensure df is a data.table
setDT(df)

# Rename the first column to 'timestamp' if it's not already named so.
# This is a common operation but make it conditional.
if (ncol(df) > 0 && names(df)[1] != 'timestamp') {
  setnames(df, old = names(df)[1], new = 'timestamp', skip_absent=TRUE)
}

# Identify potential measurement variables (assuming 'timestamp' and 'file_name_source' are ID vars)
id_vars <- c('timestamp', 'file_name_source')
measure_vars <- setdiff(names(df), id_vars)

if (length(measure_vars) == 0) {
  showNotification('No measurement variables found after identifying id_vars. Check column names.', type='warning')
  return(df) # Or NULL if this is an error state
}

# Convert all potential measure_vars to numeric. Handle errors gracefully.
for (m_var in measure_vars) {
  if (m_var %in% names(df)) {
    # Try conversion, keep original on failure or make NA
    original_class <- class(df[[m_var]])[1]
    df[, (m_var) := suppressWarnings(as.numeric(get(m_var)))]
    if (all(is.na(df[[m_var]])) && original_class != 'numeric') {
        # showNotification(paste('Column', m_var, 'could not be fully converted to numeric. Check data.'), type='warning')
    }
  }
}

# Melt data to long format
df_long <- melt(df, 
                id.vars = id_vars, 
                measure.vars = measure_vars, 
                variable.name = 'series', 
                value.name = 'value',
                na.rm = TRUE) # Remove rows where 'value' became NA after melt (e.g. from failed as.numeric)

if (nrow(df_long) == 0) {
    showNotification('Data became empty after melting. Check numeric conversions and series.', type='warning')
    return(data.table()) # Return empty DT
}


# Parse timestamp using the format from UI. Be robust.
# input_date_format is available. Default 'ymd HMS'
if ('timestamp' %in% names(df_long)) {
  current_timestamps <- df_long[['timestamp']]
  if (!is.POSIXct(current_timestamps)) { # Only parse if not already POSIXct
    parsed_ts <- tryCatch({
      lubridate::parse_date_time(as.character(current_timestamps), orders = input_date_format, quiet = TRUE)
    }, error = function(e) NULL)
    
    if (!is.null(parsed_ts) && !all(is.na(parsed_ts))) {
      df_long[, timestamp := parsed_ts]
    } else {
      # Attempt with fasttime as a fallback for common formats if lubridate fails broadly
      parsed_ts_ft <- tryCatch({ fasttime::fastPOSIXct(as.character(current_timestamps)) }, error = function(e) NULL)
      if(!is.null(parsed_ts_ft) && !all(is.na(parsed_ts_ft))) {
         df_long[, timestamp := parsed_ts_ft]
         showNotification('Used fallback timestamp parser for some values.', type='message')
      } else {
         showNotification(paste('Failed to parse timestamps with format:', input_date_format, '. Check format and data.'), type='warning')
      }
    }
  }
}


# Apply filters (these helpers are available in the eval environment)
if (exists('input_filter_in_1') && nzchar(input_filter_in_1)) {
  df_long <- filter_in(df_long, 'series', input_filter_in_1)
}
if (exists('input_filter_out_1') && nzchar(input_filter_out_1)) {
  df_long <- filter_out(df_long, 'series', input_filter_out_1)
}
if (exists('input_rename_1') && nzchar(input_rename_1)) {
  df_long <- rname(df_long, 'series', input_rename_1)
}

# Extract 'dut' (Device Under Test) identifier from file_name_source as an example
# This pattern '2212\\d*' might be specific to your filenames.
if ('file_name_source' %in% names(df_long)) {
    df_long[, dut := stringr::str_extract(file_name_source, '2212[^_]*')] # Example: 2212 followed by digits until an underscore
}

# showNotification(paste('Post-processing complete for data from:', df_long[1, file_name_source]), type='message')
df_long # Return the final processed data.table for this file
"

# --- For Plot-specific Data Processing (ui_plotter) ---
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

# Extract 'dut' if not already present or if needed differently for this plot
# This might be redundant if done in main processing, but allows plot-specific override.
if (!'dut' %in% names(df) && 'file_name_source' %in% names(df)) {
    df[, dut := stringr::str_extract(file_name_source, '2212[^_]*')]
}

df # Return the data processed for this specific plot
"

# --- Separate Ace Editor Templates for Plotting ---

# Text Output Template
ggplot_text_template <- "# Text output code
# Available: df (processed data), input (UI inputs)
# Return: Any printable R output (summary, table, custom text, etc.)

# Example text outputs:
list(
  'Data Summary' = summary(df),
  'Row Count' = nrow(df),
  'Column Names' = names(df),
  'Unique Series' = if('series' %in% names(df)) unique(df$series) else 'No series column',
  'Date Range' = if('timestamp' %in% names(df)) {
    paste('From:', min(df$timestamp, na.rm=TRUE), 'To:', max(df$timestamp, na.rm=TRUE))
  } else 'No timestamp column',
  'Sample Data' = head(df, 10)
)
"

# Static Plot Template (ggplot2)
ggplot_static_template <- "# Static plot code (ggplot2)
# Available: df (processed data), input (UI inputs)
# Return: ggplot object

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

# Build aesthetic mapping
aes_mapping <- aes(x = x_axis, y = value)

# Add color mapping if specified
if (!is.null(input$plot_color) && input$plot_color != 'null' && input$plot_color %in% names(plot_df)) {
  aes_mapping$colour <- as.name(input$plot_color)
}

# Add line type mapping if specified  
if (!is.null(input$plot_linetype) && input$plot_linetype != 'null' && input$plot_linetype %in% names(plot_df)) {
  aes_mapping$linetype <- as.name(input$plot_linetype)
}

# Create plot with combined aesthetics
p <- ggplot(plot_df, aes_mapping)

# Choose geometry based on fast rendering option
if (!is.null(input$plot_vector) && input$plot_vector == TRUE) {
  p <- p + geom_scattermore(pointsize = 2)
} else {
  p <- p + geom_line()
}

# Add labels and title with proper legend names
plot_labs <- labs(
  title = if(!is.null(input$plot_title) && nzchar(input$plot_title)) input$plot_title else 'Plot Title',
  x = x_label,
  y = if(!is.null(input$plot_ylabel)) input$plot_ylabel else 'Y Axis'
)

# Add proper legend labels
if (!is.null(input$plot_color) && input$plot_color != 'null' && input$plot_color %in% names(plot_df)) {
  plot_labs$colour <- input$plot_color
}
if (!is.null(input$plot_linetype) && input$plot_linetype != 'null' && input$plot_linetype %in% names(plot_df)) {
  plot_labs$linetype <- input$plot_linetype
}

p <- p + plot_labs

# Apply theme with font sizes
title_size <- if(!is.null(input$title_font_size)) input$title_font_size else 14
xaxis_size <- if(!is.null(input$xaxis_font_size)) input$xaxis_font_size else 12
yaxis_size <- if(!is.null(input$yaxis_font_size)) input$yaxis_font_size else 12

p <- p + theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = title_size),
    axis.title.x = element_text(size = xaxis_size),
    axis.title.y = element_text(size = yaxis_size),
    axis.text.x = element_text(size = xaxis_size * 0.9),
    axis.text.y = element_text(size = yaxis_size * 0.9)
  )

# Add best fit line if specified
if (!is.null(input$add_smooth) && input$add_smooth == TRUE) {
  p <- p + geom_smooth(method = 'loess', se = TRUE, alpha = 0.3)
}

# Add horizontal reference lines
if (!is.null(input$hline_1) && !is.na(input$hline_1)) {
  p <- p + geom_hline(yintercept = input$hline_1, linetype = 'dashed', color = 'red', alpha = 0.7)
}
if (!is.null(input$hline_2) && !is.na(input$hline_2)) {
  p <- p + geom_hline(yintercept = input$hline_2, linetype = 'dashed', color = 'blue', alpha = 0.7)
}

# Add vertical reference lines
if (!is.null(input$vline_1) && !is.na(input$vline_1)) {
  p <- p + geom_vline(xintercept = input$vline_1, linetype = 'dashed', color = 'red', alpha = 0.7)
}
if (!is.null(input$vline_2) && !is.na(input$vline_2)) {
  p <- p + geom_vline(xintercept = input$vline_2, linetype = 'dashed', color = 'blue', alpha = 0.7)
}

# Add faceting if specified
if (!is.null(input$plot_facet_end) && input$plot_facet_end > 0 && 'series' %in% names(plot_df)) {
  facet_start <- if(input$plot_facet_start > 0) input$plot_facet_start else 1
  plot_df$facet_var <- str_sub(plot_df$series, facet_start, input$plot_facet_end)
  p <- p + facet_wrap(~facet_var, nrow = input$plot_facet_nrow, scales = 'free_y')
}

p # Return ggplot object
"

# Interactive Plot Template (plotly)
ggplot_interactive_template <- "# Interactive plot code (plotly)
# Available: df (processed data), input (UI inputs)  
# Return: plotly object

# Handle X-axis transformation
plot_df <- copy(df) # Work on a copy

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
      # Default to timestamp
      plot_df[, x_axis := timestamp]
      x_label <- input$plot_xlabel
    }
  } else {
    plot_df[, x_axis := timestamp]
    x_label <- 'Timestamp'
  }
} else {
  # No timestamp column
  plot_df[, x_axis := .I] # Use row index
  x_label <- 'Index'
}

# Create base plotly plot with transformed x-axis
if (!is.null(input$plot_color) && input$plot_color != 'null' && input$plot_color %in% names(plot_df)) {
  # With color grouping
  p <- plot_ly(plot_df, x = ~x_axis, y = ~value, 
               color = as.formula(paste('~', input$plot_color)),
               type = 'scatter', mode = 'lines',
               hovertemplate = paste('<b>%{fullData.name}</b><br>',
                                   x_label, ': %{x}<br>',
                                   'Value: %{y}<extra></extra>'))
} else {
  # Without color grouping
  p <- plot_ly(plot_df, x = ~x_axis, y = ~value, 
               type = 'scatter', mode = 'lines',
               hovertemplate = paste(x_label, ': %{x}<br>',
                                   'Value: %{y}<extra></extra>'))
}

# Get font sizes
title_size <- if(!is.null(input$title_font_size)) input$title_font_size else 14
xaxis_size <- if(!is.null(input$xaxis_font_size)) input$xaxis_font_size else 12
yaxis_size <- if(!is.null(input$yaxis_font_size)) input$yaxis_font_size else 12

# Add best fit line if specified
if (!is.null(input$add_smooth) && input$add_smooth == TRUE) {
  # Add smooth line using plotly's built-in functionality
  tryCatch({
    smooth_fit <- loess(value ~ x_axis, data = plot_df)
    smooth_values <- fitted(smooth_fit)
    
    p <- p |> add_trace(
      type = 'scatter', mode = 'lines',
      x = plot_df$x_axis, y = smooth_values,
      line = list(color = 'rgba(255,0,0,0.5)', dash = 'dot'),
      name = 'Trend Line',
      showlegend = TRUE
    )
  }, error = function(e) {
    # Fallback: simple linear trend
    lm_fit <- lm(value ~ x_axis, data = plot_df)
    p <<- p |> add_trace(
      type = 'scatter', mode = 'lines',
      x = plot_df$x_axis, y = fitted(lm_fit),
      line = list(color = 'rgba(255,0,0,0.5)', dash = 'dot'),
      name = 'Linear Trend',
      showlegend = TRUE
    )
  })
}

# Add horizontal reference lines
shapes_list <- list()
if (!is.null(input$hline_1) && !is.na(input$hline_1)) {
  shapes_list[[length(shapes_list) + 1]] <- list(
    type = 'line',
    x0 = min(plot_df$x_axis, na.rm = TRUE), x1 = max(plot_df$x_axis, na.rm = TRUE),
    y0 = input$hline_1, y1 = input$hline_1,
    line = list(color = 'red', dash = 'dash', width = 2)
  )
}
if (!is.null(input$hline_2) && !is.na(input$hline_2)) {
  shapes_list[[length(shapes_list) + 1]] <- list(
    type = 'line',
    x0 = min(plot_df$x_axis, na.rm = TRUE), x1 = max(plot_df$x_axis, na.rm = TRUE),
    y0 = input$hline_2, y1 = input$hline_2,
    line = list(color = 'blue', dash = 'dash', width = 2)
  )
}

# Add vertical reference lines
if (!is.null(input$vline_1) && !is.na(input$vline_1)) {
  shapes_list[[length(shapes_list) + 1]] <- list(
    type = 'line',
    x0 = input$vline_1, x1 = input$vline_1,
    y0 = min(plot_df$value, na.rm = TRUE), y1 = max(plot_df$value, na.rm = TRUE),
    line = list(color = 'red', dash = 'dash', width = 2)
  )
}
if (!is.null(input$vline_2) && !is.na(input$vline_2)) {
  shapes_list[[length(shapes_list) + 1]] <- list(
    type = 'line',
    x0 = input$vline_2, x1 = input$vline_2,
    y0 = min(plot_df$value, na.rm = TRUE), y1 = max(plot_df$value, na.rm = TRUE),
    line = list(color = 'blue', dash = 'dash', width = 2)
  )
}

# Add layout and labels with font sizes
p <- p |> layout(
  title = list(
    text = if(!is.null(input$plot_title) && nzchar(input$plot_title)) input$plot_title else 'Interactive Plot',
    font = list(size = title_size)
  ),
  xaxis = list(
    title = list(text = x_label, font = list(size = xaxis_size)),
    tickfont = list(size = xaxis_size * 0.9)
  ),
  yaxis = list(
    title = list(text = if(!is.null(input$plot_ylabel)) input$plot_ylabel else 'Y Axis', font = list(size = yaxis_size)),
    tickfont = list(size = yaxis_size * 0.9)
  ),
  hovermode = 'x unified',
  legend = list(orientation = 'h', y = -0.2),
  shapes = shapes_list
)

p # Return plotly object
"

# Table Output Template
ggplot_table_template <- "# Table output code
# Available: df (processed data), input (UI inputs)
# Return: DT::datatable object

# Create summary table based on processed data
if (is.null(df) || nrow(df) == 0) {
  # No data case
  data.frame(Message = 'No processed data available') |> 
    datatable(options = list(dom = 't'), rownames = FALSE, class = 'compact')
} else {
  # Create comprehensive summary table
  if ('series' %in% names(df) && 'value' %in% names(df)) {
    # Group by series and create summary statistics
    summary_table <- df[, .(
      Count = .N,
      Mean = round(mean(value, na.rm = TRUE), 4),
      Median = round(median(value, na.rm = TRUE), 4),
      Min = round(min(value, na.rm = TRUE), 4),
      Max = round(max(value, na.rm = TRUE), 4),
      Std_Dev = round(sd(value, na.rm = TRUE), 4),
      Missing = sum(is.na(value)),
      First_Value = round(first(value), 4),
      Last_Value = round(last(value), 4)
    ), by = series][order(-Count)]
    
    # Add date range if timestamp available
    if ('timestamp' %in% names(df)) {
      date_ranges <- df[, .(
        Start_Time = format(min(timestamp, na.rm = TRUE), '%Y-%m-%d %H:%M:%S'),
        End_Time = format(max(timestamp, na.rm = TRUE), '%Y-%m-%d %H:%M:%S'),
        Duration_Hours = round(as.numeric(difftime(max(timestamp, na.rm = TRUE), 
                                                  min(timestamp, na.rm = TRUE), 
                                                  units = 'hours')), 2)
      ), by = series]
      
      summary_table <- merge(summary_table, date_ranges, by = 'series')
    }
    
  } else {
    # Fallback: basic data summary
    summary_table <- data.frame(
      Column = names(df),
      Type = sapply(df, function(x) class(x)[1]),
      Count = sapply(df, function(x) sum(!is.na(x))),
      Missing = sapply(df, function(x) sum(is.na(x))),
      Sample_Values = sapply(df, function(x) {
        if (is.numeric(x)) {
          paste(round(head(x[!is.na(x)], 3), 3), collapse = ', ')
        } else {
          paste(head(x[!is.na(x)], 3), collapse = ', ')
        }
      }),
      stringsAsFactors = FALSE
    )
  }
  
  # Create interactive datatable
  datatable(
    summary_table,
    options = list(
      scrollX = TRUE,
      pageLength = 15,
      searching = TRUE,
      lengthMenu = c(10, 15, 25, 50),
      columnDefs = list(list(className = 'dt-center', targets = '_all'))
    ),
    rownames = FALSE,
    filter = 'top',
    class = 'compact stripe hover',
    caption = if(!is.null(input$plot_title) && nzchar(input$plot_title)) 
                paste('Data Summary:', input$plot_title) else 'Data Summary Table'
  ) |> formatStyle(
    columns = names(summary_table),
    fontSize = '12px'
  )
}
"

# Final Conditional Template
ggplot_final_template <- "# Final conditional code to select output type
# This code runs after all the above templates and selects the appropriate output
# Available: df (processed data), input (UI inputs)

if (is.null(df) || nrow(df) == 0) {
  # No data case
  renderUI({
    div(class = 'text-center p-4',
      h4('No Data Available', class = 'text-muted'),
      p('Please process data in the Processing Data tab first.')
    )
  })
} else if (!is.null(input$plot_type)) {
  # Select output based on plot type
  if (input$plot_type == 'text') {
    # Execute text template code and render as print
    text_output <- eval(parse(text = text_code))
    renderPrint({ text_output })
  } else if (input$plot_type == 'static') {
    # Execute static template code and render as plot
    static_plot <- eval(parse(text = static_code))
    renderPlot({ static_plot }, height = 600)
  } else if (input$plot_type == 'dynamic') {
    # Execute interactive template code and render as plotly
    interactive_plot <- eval(parse(text = interactive_code))
    renderPlotly({ interactive_plot })
  } else if (input$plot_type == 'table') {
    # Execute table template code and render as DT
    table_output <- eval(parse(text = table_code))
    table_output # Return DT object directly
  } else {
    # Fallback
    renderUI({
      div(class = 'text-center p-4',
        h4('Unknown Plot Type', class = 'text-muted'),
        p('Please select a valid plot type: Text, Static, Interactive, or Table.')
      )
    })
  }
} else {
  # No plot type selected
  renderUI({
    div(class = 'text-center p-4',
      h4('Select Plot Type', class = 'text-muted'),
      p('Choose Text, Static, Interactive, or Table from the plot controls.')
    )
  })
}
"

# Combined template (for backward compatibility, but now built from parts)
ggplot_template <- paste(
  "# Combined plotting code - built from separate templates",
  "# Text code:",
  "text_code <- '", gsub("'", "\\\\'", ggplot_text_template), "'",
  "",
  "# Static code:", 
  "static_code <- '", gsub("'", "\\\\'", ggplot_static_template), "'",
  "",
  "# Interactive code:",
  "interactive_code <- '", gsub("'", "\\\\'", ggplot_interactive_template), "'",
  "",
  "# Table code:",
  "table_code <- '", gsub("'", "\\\\'", ggplot_table_template), "'",
  "",
  "# Final conditional code:",
  ggplot_final_template,
  sep = "\n"
)

# --- For Data Table Display (ui_data_table_display) ---
DT_head <- "# R code to display a table.
# df: The input data.table.
# datatable() is commonly used.
# Example:
# datatable(head(df, 100), options = list(scrollX = TRUE, pageLength = 5), rownames = FALSE, filter = 'top', class='compact stripe')
if (is.data.frame(df) || is.data.table(df)) {
  datatable(head(df, 100), 
                options = list(scrollX = TRUE, scrollY = '300px', pageLength = 10, searching = TRUE, lengthMenu = c(10, 25, 50, 100)), 
                rownames = FALSE, 
                filter = 'none', # 'top' can be slow for wide tables
                class='compact stripe hover cell-border', 
                width='100%',
                escape = FALSE) # escape=FALSE if you have HTML in cells, be careful with XSS
} else {
  df # If not a data frame (e.g., summary output from skimr)
}
"

r_code_combined_data_summary <- "# R code for Combined Data Summary table.
# df: The combined data.table.
# Expects 'value' and 'series' columns.
if (is.null(df)) {
  # No data case
  data.frame(
    Message = 'No data available yet. Please:',
    Steps = c(
      '1. Upload files in a Data Import tab',
      '2. Configure pre/post-processing settings',
      '3. Click \"Process & Combine All Files in This Importer\"',
      '4. Data will appear here automatically'
    )
  ) |> datatable(options = list(dom = 't', ordering = FALSE), rownames = FALSE, class = 'compact')
} else if (is.data.table(df) && nrow(df) > 0 && all(c('value', 'series') %in% names(df))) {
  # Expected case with proper data structure
  summary_dt <- df[, .(
    Min = round(min(value, na.rm = TRUE), 3),
    Mean = round(mean(value, na.rm = TRUE), 3),
    Median = round(median(value, na.rm = TRUE), 3),
    Max = round(max(value, na.rm = TRUE), 3),
    SD = round(sd(value, na.rm = TRUE), 3),
    Count = .N,
    Missing_Values = sum(is.na(value))
  ), by = .(series)][order(series)]
  datatable(summary_dt, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE, filter='top', class='compact stripe')
} else if (!is.null(df) && (is.data.frame(df) || is.data.table(df)) && nrow(df) > 0) {
  # Fallback if expected columns are not present but we have some data
  tryCatch({
    # Suppress the specific dplyr warning from skimr
    suppressWarnings(skim(df)) |> renderPrint() # Using renderPrint for skim output
  }, error = function(e) {
    # If skimr fails, show basic info
    data.frame(
      Info = c('Data available but structure unexpected', 'Column names:', paste(names(df), collapse = ', '), paste('Rows:', nrow(df)), paste('Columns:', ncol(df)))
    ) |> datatable(options = list(dom = 't', ordering = FALSE), rownames = FALSE, class = 'compact')
  })
} else {
  # Empty data case
  data.frame(
    Message = c('Data imported but empty after processing.', 'Check processing settings and data files.')
  ) |> datatable(options = list(dom = 't', ordering = FALSE), rownames = FALSE, class = 'compact')
}
"

r_code_combined_data_sample <- "# R code for Combined Data Sample table.
# df: The combined data.table.
if (is.null(df)) {
  # No data case
  data.frame(
    Message = 'No data available yet. Upload files and process them in Data Import tabs.'
  ) |> datatable(options = list(dom = 't', ordering = FALSE), rownames = FALSE, class = 'compact')
} else if (is.data.table(df) && nrow(df) > 0) {
  sample_size <- min(nrow(df), 100) # Show up to 100 rows
  datatable(df[sample(.N, sample_size)], 
                options = list(scrollX = TRUE, scrollY = '400px', pageLength = 10, searching = TRUE), 
                rownames = FALSE, 
                filter='top', 
                class='compact stripe')
} else if (is.data.frame(df) && nrow(df) > 0) {
  # Handle case where df is data.frame but not data.table
  setDT(df)
  sample_size <- min(nrow(df), 100)
  datatable(df[sample(.N, sample_size)], 
                options = list(scrollX = TRUE, scrollY = '400px', pageLength = 10, searching = TRUE), 
                rownames = FALSE, 
                filter='top', 
                class='compact stripe')
} else {
  data.frame(
    Message = 'Data processed but empty. Check processing settings.'
  ) |> datatable(options = list(dom = 't', ordering = FALSE), rownames = FALSE, class = 'compact')
}
"

# --- For R Code Helper Tab ---
helper_code_template <- "# R Code Helper
# Type R code here to execute it.
# Example: List installed packages
# installed.packages()[, 'Package'] |> head(20) |> paste(collapse = '\\n') |> cat()

# Example: Get help on a function
# help('lm') # This will open help in RStudio, not directly in Shiny UI

# Example: Show vignette (if HTML can be rendered)
# vignette_path <- system.file('doc', 'grid.html', package = 'grid')
# if (file.exists(vignette_path)) {
#   htmltools::HTML(readLines(vignette_path))
# } else {
#   'Vignette not found.'
# }

# Example: Basic calculation
# data.frame(x = 1:5, y = (1:5)^2) |> datatable()

# Available in this environment:
# - 'input' (main Shiny app's input)
# - 'output' (main Shiny app's output - use with caution)

# Get current working directory
getwd()
"

# --- For Downloader Tab (Refactored) ---
downloader_code_refactored <- "# Batch Downloader - Select and Download Multiple Plots
# This creates a user-friendly interface for selecting which plots to download

library(shiny)
library(DT)

# Get available plots
available_plots <- names(dynamic_plots_map)

if (length(available_plots) == 0) {
  # No plots available
  div(class = 'alert alert-warning',
    h4('No Active Plots Found'),
    p('Create some plots first, then return here to download them.')
  )
} else {
  # Create selection interface
  div(
    h4('Select Plots to Download'),
    p(class = 'text-muted', 'Choose which plots you want to include in the ZIP file:'),
    
    # Plot selection checkboxes
    div(class = 'mb-3',
      lapply(available_plots, function(plot_id) {
        # Get a friendly name for the plot
        friendly_name <- gsub('_', ' ', plot_id)
        friendly_name <- tools::toTitleCase(friendly_name)
        
        div(class = 'form-check',
          tags$input(
            type = 'checkbox',
            class = 'form-check-input',
            id = paste0('select_plot_', plot_id),
            value = plot_id,
            checked = 'checked'  # Default to selected
          ),
          tags$label(
            class = 'form-check-label',
            `for` = paste0('select_plot_', plot_id),
            friendly_name
          )
        )
      })
    ),
    
    # Select/Deselect All buttons
    div(class = 'mb-3',
      actionButton('select_all_plots', 'Select All', class = 'btn btn-sm btn-outline-primary me-2'),
      actionButton('deselect_all_plots', 'Deselect All', class = 'btn btn-sm btn-outline-secondary')
    ),
    
         # Download options
     div(class = 'mb-3',
       h5('Download Options'),
       p(class = 'text-muted small', 'Plots will be saved in their native format: Interactive plots as HTML, Static plots as PNG, Text outputs as TXT files.'),
       div(class = 'row',
         div(class = 'col-md-6',
           numericInput('download_dpi', 'DPI for PNG files',
             value = 300, min = 72, max = 600, step = 50
           )
         ),
         div(class = 'col-md-6',
           numericInput('download_width', 'Plot Width (inches)',
             value = 10, min = 4, max = 20, step = 1
           )
         )
       ),
       div(class = 'row',
         div(class = 'col-md-6',
           numericInput('download_height', 'Plot Height (inches)',
             value = 7, min = 3, max = 15, step = 1
           )
         )
       )
     ),
    
    # Download button
    div(class = 'text-center',
      downloadButton('batch_download_zip', 
        'Download Selected Plots as ZIP',
        class = 'btn btn-success btn-lg',
        icon = icon('download')
      )
    )
  )
}
"
