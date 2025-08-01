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

# Modular Static Plot Templates (ggplot2) - Broken down for better usability

# 1. Data Processing Template
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

# 2. Base Plot Setup Template
ggplot_base_setup_template <- "# Base Plot Setup 
# Available: plot_df (processed data), input (UI inputs), x_label
# Purpose: Create base plot with aesthetics and geometry

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

# Get size and alpha values
line_width <- if(!is.null(input$line_width)) input$line_width else 1
point_size <- if(!is.null(input$point_size)) input$point_size else 2
alpha_val <- if(!is.null(input$alpha)) input$alpha else 1.0

# Create plot with combined aesthetics
p <- ggplot(plot_df, aes_mapping)

# Choose geometry based on geom_type input
geom_type <- if(!is.null(input$geom_type)) input$geom_type else 'geom_line'

if (geom_type == 'geom_line') {
  p <- p + geom_line(linewidth = line_width, alpha = alpha_val)
} else if (geom_type == 'geom_point') {
  p <- p + geom_point(size = point_size, alpha = alpha_val)
} else if (geom_type == 'geom_line_point') {
  p <- p + geom_line(linewidth = line_width, alpha = alpha_val) + 
           geom_point(size = point_size, alpha = alpha_val)
} else if (geom_type == 'geom_area') {
  p <- p + geom_area(alpha = alpha_val * 0.7)
} else if (geom_type == 'geom_smooth') {
  p <- p + geom_smooth(method = 'loess', se = TRUE, alpha = alpha_val * 0.3, linewidth = line_width)
} else if (geom_type == 'geom_col') {
  p <- p + geom_col(alpha = alpha_val)
} else {
  # Default fallback
  p <- p + geom_line(linewidth = line_width, alpha = alpha_val)
}

p # Return base plot
"

# 3. Themes & Styling Template
ggplot_themes_styling_template <- "# Themes & Styling
# Available: p (base plot), plot_df, input (UI inputs), x_label
# Purpose: Apply themes, colors, fonts, and legend settings

# Apply theme
theme_choice <- if(!is.null(input$plot_theme)) input$plot_theme else 'theme_classic'
if (theme_choice == 'theme_classic') {
  p <- p + theme_classic()
} else if (theme_choice == 'theme_minimal') {
  p <- p + theme_minimal()
} else if (theme_choice == 'theme_dark') {
  p <- p + theme_dark()
} else if (theme_choice == 'theme_light') {
  p <- p + theme_light()
} else if (theme_choice == 'theme_bw') {
  p <- p + theme_bw()
} else if (theme_choice == 'theme_void') {
  p <- p + theme_void()
} else {
  p <- p + theme_classic()
}

# Apply color palette
if (!is.null(input$plot_color) && input$plot_color != 'null' && input$plot_color %in% names(plot_df)) {
  color_palette <- if(!is.null(input$color_palette)) input$color_palette else 'default'
  
  if (color_palette == 'viridis') {
    p <- p + scale_color_viridis_d()
  } else if (color_palette %in% c('Set1', 'Set2', 'Dark2', 'Paired')) {
    p <- p + scale_color_brewer(type = 'qual', palette = color_palette)
  }
  # Default palette is handled automatically by ggplot
}

# Get font sizes
title_size <- if(!is.null(input$title_font_size)) input$title_font_size else 14
xaxis_size <- if(!is.null(input$xaxis_font_size)) input$xaxis_font_size else 12
yaxis_size <- if(!is.null(input$yaxis_font_size)) input$yaxis_font_size else 12
legend_size <- if(!is.null(input$legend_font_size)) input$legend_font_size else 12

# Apply legend settings
legend_pos <- if(!is.null(input$legend_position)) input$legend_position else 'right'

# Customize theme with all settings
p <- p + theme(
  plot.title = element_text(hjust = 0.5, size = title_size),
  axis.title.x = element_text(size = xaxis_size),
  axis.title.y = element_text(size = yaxis_size),
  axis.text.x = element_text(size = xaxis_size * 0.9),
  axis.text.y = element_text(size = yaxis_size * 0.9),
  legend.position = legend_pos,
  legend.text = element_text(size = legend_size),
  legend.title = element_text(size = legend_size)
)

# Add labels and title
plot_labs <- labs(
  title = if(!is.null(input$plot_title) && nzchar(input$plot_title)) input$plot_title else 'Plot Title',
  x = x_label,
  y = if(!is.null(input$plot_ylabel)) input$plot_ylabel else 'Y Axis',
  caption = if(!is.null(input$plot_caption) && nzchar(input$plot_caption)) input$plot_caption else NULL
)

# Add proper legend labels
if (!is.null(input$plot_color) && input$plot_color != 'null' && input$plot_color %in% names(plot_df)) {
  plot_labs$colour <- input$plot_color
}
if (!is.null(input$plot_linetype) && input$plot_linetype != 'null' && input$plot_linetype %in% names(plot_df)) {
  plot_labs$linetype <- input$plot_linetype
}

p <- p + plot_labs

p # Return styled plot
"

# 4. Statistical Overlays Template
ggplot_statistical_overlays_template <- "# Statistical Overlays & Lines
# Available: p (styled plot), plot_df, input (UI inputs)
# Purpose: Add trend lines, mean/median lines, reference lines

# Collect line legend information for the new linetype-based approach
line_legend_names <- c()
line_legend_colors <- c()
line_legend_linetypes <- c()

# Add statistical overlays with enhanced controls
if (!is.null(input$add_smooth) && input$add_smooth == TRUE) {
  smooth_method <- if(!is.null(input$smooth_method)) input$smooth_method else 'loess'
  smooth_linetype <- if(!is.null(input$smooth_linetype)) input$smooth_linetype else 'solid'
  smooth_linewidth <- if(!is.null(input$smooth_linewidth)) input$smooth_linewidth else 1
  smooth_color <- if(!is.null(input$smooth_color)) input$smooth_color else 'red'
  smooth_legend <- if(!is.null(input$smooth_legend)) input$smooth_legend else TRUE
  smooth_name <- if(!is.null(input$smooth_name) && nzchar(input$smooth_name)) input$smooth_name else 'Trend'
  
  if (smooth_legend) {
    # Try to extract equation from smooth fit for legend name
    tryCatch({
      if (smooth_method == 'lm') {
        # For linear model, extract coefficients
        temp_model <- lm(value ~ x_axis, data = plot_df)
        coef_vals <- coef(temp_model)
        if (length(coef_vals) >= 2) {
          equation <- paste0('y = ', round(coef_vals[2], 3), 'x + ', round(coef_vals[1], 3))
          smooth_name_with_eq <- paste0(smooth_name, ' (', equation, ')')
        } else {
          smooth_name_with_eq <- smooth_name
        }
      } else {
        # For other methods, just use the method name
        smooth_name_with_eq <- paste0(smooth_name, ' (', smooth_method, ')')
      }
    }, error = function(e) {
      smooth_name_with_eq <- smooth_name
    })
    
    line_legend_names <- c(line_legend_names, smooth_name_with_eq)
    line_legend_colors <- c(line_legend_colors, smooth_color)
    line_legend_linetypes <- c(line_legend_linetypes, smooth_linetype)
    
    # Add trend line with linetype mapping for legend
    p <- p + geom_smooth(
      aes(linetype = smooth_name_with_eq),
      method = smooth_method, 
      se = TRUE, 
      alpha = 0.3, 
      linewidth = smooth_linewidth,
      color = smooth_color,
      show.legend = TRUE
    )
  } else {
    # Add trend line with no legend
    p <- p + geom_smooth(
      method = smooth_method, 
      se = TRUE, 
      alpha = 0.3, 
      linetype = smooth_linetype,
      linewidth = smooth_linewidth,
      color = smooth_color,
      show.legend = FALSE
    )
  }
}

# Mean and Median Lines - REMOVED

# Enhanced reference lines with full control
if (!is.null(input$enable_hline_1) && input$enable_hline_1 == TRUE && !is.null(input$hline_1)) {
  hline_1_linetype <- if(!is.null(input$hline_1_linetype)) input$hline_1_linetype else 'dashed'
  hline_1_linewidth <- if(!is.null(input$hline_1_linewidth)) input$hline_1_linewidth else 1
  hline_1_color <- if(!is.null(input$hline_1_color)) input$hline_1_color else 'red'
  hline_1_legend <- if(!is.null(input$hline_1_legend)) input$hline_1_legend else FALSE
  hline_1_name <- if(!is.null(input$hline_1_name) && nzchar(input$hline_1_name)) input$hline_1_name else 'H-Line 1'
  
  if (hline_1_legend) {
    line_legend_names <- c(line_legend_names, hline_1_name)
    line_legend_colors <- c(line_legend_colors, hline_1_color)
    line_legend_linetypes <- c(line_legend_linetypes, hline_1_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_hline(
      aes(yintercept = input$hline_1, linetype = hline_1_name),
      linewidth = hline_1_linewidth,
      color = hline_1_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_hline(
      yintercept = input$hline_1,
      linetype = hline_1_linetype,
      linewidth = hline_1_linewidth,
      color = hline_1_color,
      alpha = 0.7
    )
  }
}

if (!is.null(input$enable_hline_2) && input$enable_hline_2 == TRUE && !is.null(input$hline_2)) {
  hline_2_linetype <- if(!is.null(input$hline_2_linetype)) input$hline_2_linetype else 'dashed'
  hline_2_linewidth <- if(!is.null(input$hline_2_linewidth)) input$hline_2_linewidth else 1
  hline_2_color <- if(!is.null(input$hline_2_color)) input$hline_2_color else 'blue'
  hline_2_legend <- if(!is.null(input$hline_2_legend)) input$hline_2_legend else FALSE
  hline_2_name <- if(!is.null(input$hline_2_name) && nzchar(input$hline_2_name)) input$hline_2_name else 'H-Line 2'
  
  if (hline_2_legend) {
    line_legend_names <- c(line_legend_names, hline_2_name)
    line_legend_colors <- c(line_legend_colors, hline_2_color)
    line_legend_linetypes <- c(line_legend_linetypes, hline_2_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_hline(
      aes(yintercept = input$hline_2, linetype = hline_2_name),
      linewidth = hline_2_linewidth,
      color = hline_2_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_hline(
      yintercept = input$hline_2,
      linetype = hline_2_linetype,
      linewidth = hline_2_linewidth,
      color = hline_2_color,
      alpha = 0.7
    )
  }
}

if (!is.null(input$enable_hline_3) && input$enable_hline_3 == TRUE && !is.null(input$hline_3)) {
  hline_3_linetype <- if(!is.null(input$hline_3_linetype)) input$hline_3_linetype else 'dashed'
  hline_3_linewidth <- if(!is.null(input$hline_3_linewidth)) input$hline_3_linewidth else 1
  hline_3_color <- if(!is.null(input$hline_3_color)) input$hline_3_color else 'purple'
  hline_3_legend <- if(!is.null(input$hline_3_legend)) input$hline_3_legend else FALSE
  hline_3_name <- if(!is.null(input$hline_3_name) && nzchar(input$hline_3_name)) input$hline_3_name else 'H-Line 3'
  
  if (hline_3_legend) {
    line_legend_names <- c(line_legend_names, hline_3_name)
    line_legend_colors <- c(line_legend_colors, hline_3_color)
    line_legend_linetypes <- c(line_legend_linetypes, hline_3_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_hline(
      aes(yintercept = input$hline_3, linetype = hline_3_name),
      linewidth = hline_3_linewidth,
      color = hline_3_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_hline(
      yintercept = input$hline_3,
      linetype = hline_3_linetype,
      linewidth = hline_3_linewidth,
      color = hline_3_color,
      alpha = 0.7
    )
  }
}

if (!is.null(input$enable_hline_4) && input$enable_hline_4 == TRUE && !is.null(input$hline_4)) {
  hline_4_linetype <- if(!is.null(input$hline_4_linetype)) input$hline_4_linetype else 'dashed'
  hline_4_linewidth <- if(!is.null(input$hline_4_linewidth)) input$hline_4_linewidth else 1
  hline_4_color <- if(!is.null(input$hline_4_color)) input$hline_4_color else 'orange'
  hline_4_legend <- if(!is.null(input$hline_4_legend)) input$hline_4_legend else FALSE
  hline_4_name <- if(!is.null(input$hline_4_name) && nzchar(input$hline_4_name)) input$hline_4_name else 'H-Line 4'
  
  if (hline_4_legend) {
    line_legend_names <- c(line_legend_names, hline_4_name)
    line_legend_colors <- c(line_legend_colors, hline_4_color)
    line_legend_linetypes <- c(line_legend_linetypes, hline_4_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_hline(
      aes(yintercept = input$hline_4, linetype = hline_4_name),
      linewidth = hline_4_linewidth,
      color = hline_4_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_hline(
      yintercept = input$hline_4,
      linetype = hline_4_linetype,
      linewidth = hline_4_linewidth,
      color = hline_4_color,
      alpha = 0.7
    )
  }
}

# Enhanced vertical reference lines
if (!is.null(input$enable_vline_1) && input$enable_vline_1 == TRUE && !is.null(input$vline_1)) {
  vline_1_linetype <- if(!is.null(input$vline_1_linetype)) input$vline_1_linetype else 'dashed'
  vline_1_linewidth <- if(!is.null(input$vline_1_linewidth)) input$vline_1_linewidth else 1
  vline_1_color <- if(!is.null(input$vline_1_color)) input$vline_1_color else 'red'
  vline_1_legend <- if(!is.null(input$vline_1_legend)) input$vline_1_legend else FALSE
  vline_1_name <- if(!is.null(input$vline_1_name) && nzchar(input$vline_1_name)) input$vline_1_name else 'V-Line 1'
  
  if (vline_1_legend) {
    line_legend_names <- c(line_legend_names, vline_1_name)
    line_legend_colors <- c(line_legend_colors, vline_1_color)
    line_legend_linetypes <- c(line_legend_linetypes, vline_1_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_vline(
      aes(xintercept = input$vline_1, linetype = vline_1_name),
      linewidth = vline_1_linewidth,
      color = vline_1_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_vline(
      xintercept = input$vline_1,
      linetype = vline_1_linetype,
      linewidth = vline_1_linewidth,
      color = vline_1_color,
      alpha = 0.7
    )
  }
}

if (!is.null(input$enable_vline_2) && input$enable_vline_2 == TRUE && !is.null(input$vline_2)) {
  vline_2_linetype <- if(!is.null(input$vline_2_linetype)) input$vline_2_linetype else 'dashed'
  vline_2_linewidth <- if(!is.null(input$vline_2_linewidth)) input$vline_2_linewidth else 1
  vline_2_color <- if(!is.null(input$vline_2_color)) input$vline_2_color else 'blue'
  vline_2_legend <- if(!is.null(input$vline_2_legend)) input$vline_2_legend else FALSE
  vline_2_name <- if(!is.null(input$vline_2_name) && nzchar(input$vline_2_name)) input$vline_2_name else 'V-Line 2'
  
  if (vline_2_legend) {
    line_legend_names <- c(line_legend_names, vline_2_name)
    line_legend_colors <- c(line_legend_colors, vline_2_color)
    line_legend_linetypes <- c(line_legend_linetypes, vline_2_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_vline(
      aes(xintercept = input$vline_2, linetype = vline_2_name),
      linewidth = vline_2_linewidth,
      color = vline_2_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_vline(
      xintercept = input$vline_2,
      linetype = vline_2_linetype,
      linewidth = vline_2_linewidth,
      color = vline_2_color,
      alpha = 0.7
    )
  }
}

if (!is.null(input$enable_vline_3) && input$enable_vline_3 == TRUE && !is.null(input$vline_3)) {
  vline_3_linetype <- if(!is.null(input$vline_3_linetype)) input$vline_3_linetype else 'dashed'
  vline_3_linewidth <- if(!is.null(input$vline_3_linewidth)) input$vline_3_linewidth else 1
  vline_3_color <- if(!is.null(input$vline_3_color)) input$vline_3_color else 'purple'
  vline_3_legend <- if(!is.null(input$vline_3_legend)) input$vline_3_legend else FALSE
  vline_3_name <- if(!is.null(input$vline_3_name) && nzchar(input$vline_3_name)) input$vline_3_name else 'V-Line 3'
  
  if (vline_3_legend) {
    line_legend_names <- c(line_legend_names, vline_3_name)
    line_legend_colors <- c(line_legend_colors, vline_3_color)
    line_legend_linetypes <- c(line_legend_linetypes, vline_3_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_vline(
      aes(xintercept = input$vline_3, linetype = vline_3_name),
      linewidth = vline_3_linewidth,
      color = vline_3_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_vline(
      xintercept = input$vline_3,
      linetype = vline_3_linetype,
      linewidth = vline_3_linewidth,
      color = vline_3_color,
      alpha = 0.7
    )
  }
}

if (!is.null(input$enable_vline_4) && input$enable_vline_4 == TRUE && !is.null(input$vline_4)) {
  vline_4_linetype <- if(!is.null(input$vline_4_linetype)) input$vline_4_linetype else 'dashed'
  vline_4_linewidth <- if(!is.null(input$vline_4_linewidth)) input$vline_4_linewidth else 1
  vline_4_color <- if(!is.null(input$vline_4_color)) input$vline_4_color else 'orange'
  vline_4_legend <- if(!is.null(input$vline_4_legend)) input$vline_4_legend else FALSE
  vline_4_name <- if(!is.null(input$vline_4_name) && nzchar(input$vline_4_name)) input$vline_4_name else 'V-Line 4'
  
  if (vline_4_legend) {
    line_legend_names <- c(line_legend_names, vline_4_name)
    line_legend_colors <- c(line_legend_colors, vline_4_color)
    line_legend_linetypes <- c(line_legend_linetypes, vline_4_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_vline(
      aes(xintercept = input$vline_4, linetype = vline_4_name),
      linewidth = vline_4_linewidth,
      color = vline_4_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_vline(
      xintercept = input$vline_4,
      linetype = vline_4_linetype,
      linewidth = vline_4_linewidth,
      color = vline_4_color,
      alpha = 0.7
    )
  }
}

# Apply the new linetype-based legend system that preserves main plot legend
if (length(line_legend_names) > 0) {
  # Create named vectors for the scale
  legend_linetype_values <- setNames(line_legend_linetypes, line_legend_names)
  
  # Add the linetype scale with color override for the legend
  p <- p + scale_linetype_manual(
    name = 'Lines', 
    values = legend_linetype_values,
    guide = guide_legend(
      override.aes = list(color = line_legend_colors),
      order = 2  # Place lines legend after main legend
    )
  )
}

p # Return plot with overlays
"

# 5. Grid & Axes Template  
ggplot_grid_axes_template <- "# Grid & Axes Controls
# Available: p (plot with overlays), plot_df, input (UI inputs)
# Purpose: Apply grid controls, axis transformations, and limits

# transformations
x_trans <- if(!is.null(input$x_trans)) input$x_trans else 'identity'
y_trans <- if(!is.null(input$y_trans)) input$y_trans else 'identity'

# major grid spacing
maj_x_by  <- if(!is.null(input$major_vgrid_breaks)) input$major_vgrid_breaks else 10
maj_y_by  <- if(!is.null(input$major_hgrid_breaks)) input$major_hgrid_breaks else 5

# number of minor intervals BETWEEN each pair of majors
nmin_x    <- if(!is.null(input$minor_vgrid_breaks)) input$minor_vgrid_breaks else 3
nmin_y    <- if(!is.null(input$minor_hgrid_breaks)) input$minor_hgrid_breaks else 3

# major grid line styles
maj_x_col <- if(!is.null(input$major_vgrid_color)) input$major_vgrid_color else 'grey80'
maj_y_col <- if(!is.null(input$major_hgrid_color)) input$major_hgrid_color else 'grey80'
maj_x_lw  <- if(!is.null(input$major_vgrid_linewidth)) input$major_vgrid_linewidth else 0.5
maj_y_lw  <- if(!is.null(input$major_hgrid_linewidth)) input$major_hgrid_linewidth else 0.5
maj_x_lt  <- if(!is.null(input$major_vgrid_linetype)) input$major_vgrid_linetype else 'solid'
maj_y_lt  <- if(!is.null(input$major_hgrid_linetype)) input$major_hgrid_linetype else 'solid'

# minor grid line styles
min_x_col <- if(!is.null(input$minor_vgrid_color)) input$minor_vgrid_color else 'grey95'
min_y_col <- if(!is.null(input$minor_hgrid_color)) input$minor_hgrid_color else 'grey95'
min_x_lw  <- if(!is.null(input$minor_vgrid_linewidth)) input$minor_vgrid_linewidth else 0.25
min_y_lw  <- if(!is.null(input$minor_hgrid_linewidth)) input$minor_hgrid_linewidth else 0.25
min_x_lt  <- if(!is.null(input$minor_vgrid_linetype)) input$minor_vgrid_linetype else 'dashed'
min_y_lt  <- if(!is.null(input$minor_hgrid_linetype)) input$minor_hgrid_linetype else 'dashed'

# grid enable flags
enable_maj_x <- !is.null(input$enable_major_vgrid) && input$enable_major_vgrid == TRUE
enable_maj_y <- !is.null(input$enable_major_hgrid) && input$enable_major_hgrid == TRUE
enable_min_x <- !is.null(input$enable_minor_vgrid) && input$enable_minor_vgrid == TRUE
enable_min_y <- !is.null(input$enable_minor_hgrid) && input$enable_minor_hgrid == TRUE

# axis hard limits
y_start_enabled <- !is.null(input$enable_y_start) && input$enable_y_start == TRUE
y_end_enabled <- !is.null(input$enable_y_end) && input$enable_y_end == TRUE

if (y_start_enabled || y_end_enabled) {
  y_data_range <- range(plot_df$value, na.rm = TRUE)
  y_start <- if(y_start_enabled && !is.null(input$y_start)) input$y_start else y_data_range[1]
  y_end <- if(y_end_enabled && !is.null(input$y_end)) input$y_end else y_data_range[2]
  ylim_values <- c(y_start, y_end)
} else {
  ylim_values <- NULL
}

# X axis limits (support both numeric and timestamp)
x_limit_type <- if(!is.null(input$x_limit_type)) input$x_limit_type else 'numeric'

if (x_limit_type == 'numeric') {
  x_start_enabled <- !is.null(input$enable_x_start_numeric) && input$enable_x_start_numeric == TRUE
  x_end_enabled <- !is.null(input$enable_x_end_numeric) && input$enable_x_end_numeric == TRUE
  
  if (x_start_enabled || x_end_enabled) {
    x_data_range <- range(plot_df$x_axis, na.rm = TRUE)
    x_start <- if(x_start_enabled && !is.null(input$x_start_numeric)) input$x_start_numeric else x_data_range[1]
    x_end <- if(x_end_enabled && !is.null(input$x_end_numeric)) input$x_end_numeric else x_data_range[2]
    xlim_values <- c(x_start, x_end)
  } else {
    xlim_values <- NULL
  }
} else if (x_limit_type == 'timestamp') {
  x_start_enabled <- !is.null(input$enable_x_start_timestamp) && input$enable_x_start_timestamp == TRUE
  x_end_enabled <- !is.null(input$enable_x_end_timestamp) && input$enable_x_end_timestamp == TRUE
  
  if (x_start_enabled || x_end_enabled) {
    x_data_range <- range(plot_df$x_axis, na.rm = TRUE)
    
    x_start <- if(x_start_enabled && !is.null(input$x_start_date) && !is.null(input$x_start_time)) {
      as.POSIXct(paste(
        as.character(input$x_start_date), 
        format(input$x_start_time, '%H:%M:%S')
      ))
    } else {
      x_data_range[1]
    }
    
    x_end <- if(x_end_enabled && !is.null(input$x_end_date) && !is.null(input$x_end_time)) {
      as.POSIXct(paste(
        as.character(input$x_end_date), 
        format(input$x_end_time, '%H:%M:%S')
      ))
    } else {
      x_data_range[2]
    }
    
    xlim_values <- c(x_start, x_end)
  } else {
    xlim_values <- NULL
  }
} else {
  xlim_values <- NULL
}

# -----------------------------------------------------------------------------
# 2. Helper to compute minor breaks from majors + a count
# -----------------------------------------------------------------------------
make_minor_breaks <- function(majors, n_between) {
  if (length(majors) < 2 || n_between < 1) return(numeric(0))
  
  minors <- c()
  for (i in seq_len(length(majors) - 1)) {
    a <- majors[i]
    b <- majors[i + 1]
    
    if (inherits(a, c('POSIXct', 'POSIXt', 'Date'))) {
      # For time data - handle POSIXct properly
      time_diff_seconds <- as.numeric(difftime(b, a, units = 'secs'))
      step_seconds <- time_diff_seconds / (n_between + 1)
      
      # Create minor time points
      for (j in 1:n_between) {
        minor_time <- a + (j * step_seconds)
        minors <- c(minors, as.numeric(minor_time))
      }
    } else {
      # For numeric data
      step <- (b - a) / (n_between + 1)
      minor_vals <- seq(a + step, b - step, length.out = n_between)
      minors <- c(minors, minor_vals)
    }
  }
  
  # Convert back to original class
  if (length(majors) > 0 && inherits(majors[1], c('POSIXct', 'POSIXt'))) {
    minors <- as.POSIXct(minors, origin = '1970-01-01', tz = attr(majors[1], 'tzone'))
  } else if (length(majors) > 0 && inherits(majors[1], 'Date')) {
    minors <- as.Date(minors, origin = '1970-01-01')
  }
  
  return(minors)
}

# -----------------------------------------------------------------------------
# 3. Compute major breaks based on data type
# -----------------------------------------------------------------------------
x_breaks <- NULL
y_breaks <- NULL
x_minor_breaks <- NULL
y_minor_breaks <- NULL

# Get vertical grid type setting (user choice: 'auto', 'numeric', 'timestamp')
vertical_grid_type <- if(!is.null(input$vertical_grid_type)) input$vertical_grid_type else 'auto'

# X-axis breaks (handle timestamps and numeric based on user choice)
if (enable_maj_x) {
  # Determine how to treat x-axis data based on user selection
  use_timestamp_mode <- (vertical_grid_type == 'timestamp') || 
                       (vertical_grid_type == 'auto' && inherits(plot_df$x_axis, c('POSIXct', 'POSIXt', 'Date')))
  
  if (use_timestamp_mode) {
    # For timestamp data, maj_x_by is interpreted as minutes
    x_data_range <- range(plot_df$x_axis, na.rm = TRUE)
    
    # Create breaks at exact minute intervals for consistent spacing
    interval_seconds <- maj_x_by * 60  # Convert minutes to seconds
    
    # Round start time down to nearest interval and end time up to nearest interval
    start_time <- x_data_range[1]
    end_time <- x_data_range[2]
    
    # Create sequence with exact time intervals
    x_breaks <- seq(from = start_time, to = end_time, by = interval_seconds)
    
    # Ensure we have at least 2 breaks for the grid to work
    if (length(x_breaks) < 2) {
      # If interval is too large, create at least start and end
      x_breaks <- seq(from = start_time, to = end_time, length.out = 3)
    }
  } else {
    # For numeric data (force convert if needed)
    x_numeric_data <- as.numeric(plot_df$x_axis)
    x_data_range <- range(x_numeric_data, na.rm = TRUE)
    x_breaks <- seq(
      from = floor(x_data_range[1] / maj_x_by) * maj_x_by,
      to = ceiling(x_data_range[2] / maj_x_by) * maj_x_by,
      by = maj_x_by
    )
  }
  
  # Calculate minor breaks if enabled
  if (enable_min_x && !is.null(x_breaks)) {
    x_minor_breaks <- make_minor_breaks(x_breaks, nmin_x)
  }
}

# Y-axis breaks (always numeric in this case)
if (enable_maj_y && is.numeric(plot_df$value)) {
  y_data_range <- range(plot_df$value, na.rm = TRUE)
  y_breaks <- seq(
    from = floor(y_data_range[1] / maj_y_by) * maj_y_by,
    to = ceiling(y_data_range[2] / maj_y_by) * maj_y_by,
    by = maj_y_by
  )
  
  # Calculate minor breaks if enabled
  if (enable_min_y && !is.null(y_breaks)) {
    y_minor_breaks <- make_minor_breaks(y_breaks, nmin_y)
  }
}

# -----------------------------------------------------------------------------
# 4. Apply scales with custom breaks (trans only works with continuous scales)
# -----------------------------------------------------------------------------
if (!is.null(x_breaks)) {
  # Determine which scale function to use
  use_timestamp_mode <- (vertical_grid_type == 'timestamp') || 
                       (vertical_grid_type == 'auto' && inherits(plot_df$x_axis, c('POSIXct', 'POSIXt', 'Date')))
  
  if (use_timestamp_mode) {
    if (inherits(plot_df$x_axis, c('POSIXct', 'POSIXt'))) {
      p <- p + scale_x_datetime(
        breaks = x_breaks,
        minor_breaks = x_minor_breaks
      )
    } else if (inherits(plot_df$x_axis, 'Date')) {
      p <- p + scale_x_date(
        breaks = x_breaks,
        minor_breaks = x_minor_breaks
      )
    }
  } else {
    # Use continuous scale with transformation support
    p <- p + scale_x_continuous(
      trans = x_trans,
      breaks = x_breaks,
      minor_breaks = x_minor_breaks
    )
  }
} else {
  # No custom breaks - apply appropriate scale
  use_timestamp_mode <- (vertical_grid_type == 'timestamp') || 
                       (vertical_grid_type == 'auto' && inherits(plot_df$x_axis, c('POSIXct', 'POSIXt', 'Date')))
  
  if (use_timestamp_mode) {
    if (inherits(plot_df$x_axis, c('POSIXct', 'POSIXt'))) {
      p <- p + scale_x_datetime()
    } else if (inherits(plot_df$x_axis, 'Date')) {
      p <- p + scale_x_date()
    }
  } else {
    p <- p + scale_x_continuous(trans = x_trans)
  }
}

if (!is.null(y_breaks)) {
  p <- p + scale_y_continuous(
    trans = y_trans,
    breaks = y_breaks,
    minor_breaks = y_minor_breaks
  )
} else {
  p <- p + scale_y_continuous(trans = y_trans)
}

# -----------------------------------------------------------------------------
# 5. Apply grid line theming
# -----------------------------------------------------------------------------
theme_updates <- list()

# Major grid lines
if (enable_maj_x) {
  theme_updates$panel.grid.major.x <- element_line(
    color = maj_x_col,
    linewidth = maj_x_lw,
    linetype = maj_x_lt
  )
} else {
  theme_updates$panel.grid.major.x <- element_blank()
}

if (enable_maj_y) {
  theme_updates$panel.grid.major.y <- element_line(
    color = maj_y_col,
    linewidth = maj_y_lw,
    linetype = maj_y_lt
  )
} else {
  theme_updates$panel.grid.major.y <- element_blank()
}

# Minor grid lines
if (enable_min_x) {
  theme_updates$panel.grid.minor.x <- element_line(
    color = min_x_col,
    linewidth = min_x_lw,
    linetype = min_x_lt
  )
} else {
  theme_updates$panel.grid.minor.x <- element_blank()
}

if (enable_min_y) {
  theme_updates$panel.grid.minor.y <- element_line(
    color = min_y_col,
    linewidth = min_y_lw,
    linetype = min_y_lt
  )
} else {
  theme_updates$panel.grid.minor.y <- element_blank()
}

# Apply all theme updates at once
p <- p + do.call(theme, theme_updates)

# Store limits for use in final template
assign('xlim_values', xlim_values, envir = .GlobalEnv) 
assign('ylim_values', ylim_values, envir = .GlobalEnv)

p # Return plot with grid and axis controls
"

# 6. Faceting & Final Template
ggplot_faceting_final_template <- "# Faceting & Final Assembly
# Available: p (complete plot), plot_df, input (UI inputs), xlim_values, ylim_values  
# Purpose: Add faceting, resolve coord_cartesian vs free scales, apply final legend

# Check if faceting is being used
has_faceting <- !is.null(input$plot_facet_end) && input$plot_facet_end > 0 && 'series' %in% names(plot_df)

# Apply hard boundaries using coord_cartesian for precise plot edges
# Note: coord_cartesian conflicts with facet free scales, so we only apply it when not faceting
if ((!is.null(xlim_values) || !is.null(ylim_values)) && !has_faceting) {
  p <- p + coord_cartesian(xlim = xlim_values, ylim = ylim_values, expand = FALSE)
} else if (!is.null(xlim_values) || !is.null(ylim_values)) {
  # For faceted plots, use scale limits instead of coord_cartesian to preserve free scales
  if (!is.null(xlim_values)) {
    p <- p + scale_x_continuous(limits = xlim_values, expand = c(0, 0))
  }
  if (!is.null(ylim_values)) {
    p <- p + scale_y_continuous(limits = ylim_values, expand = c(0, 0))
  }
}

# Line legends are now handled in the Statistical Overlays template
# No additional legend processing needed here

# Add faceting if specified
if (has_faceting) {
  facet_start <- if(input$plot_facet_start > 0) input$plot_facet_start else 1
  plot_df$facet_var <- str_sub(plot_df$series, facet_start, input$plot_facet_end)
  p <- p + facet_wrap(~facet_var, nrow = input$plot_facet_nrow, scales = 'free_y')
}

p # Return final ggplot object
"

# Static Plot Template (ggplot2)
ggplot_static_template <- "# Combined Static Plot Template
# This template combines all 6 modular templates for a complete static plot
# Available: df (processed data), input (UI inputs)
# Return: ggplot object

# Step 1: Data Processing
eval(parse(text = ggplot_data_processing_template))
# Result: plot_df, x_label

# Step 2: Base Plot Setup  
eval(parse(text = ggplot_base_setup_template))
# Result: p (base plot)

# Step 3: Themes & Styling
eval(parse(text = ggplot_themes_styling_template))
# Result: p (styled plot)

# Step 4: Statistical Overlays
eval(parse(text = ggplot_statistical_overlays_template))
# Result: p (plot with overlays)

# Step 5: Grid & Axes
eval(parse(text = ggplot_grid_axes_template))
# Result: p (plot with grid/axes), xlim_values, ylim_values

# Step 6: Faceting & Final
eval(parse(text = ggplot_faceting_final_template))
# Result: p (final ggplot object)

p # Return final plot
"

ggplot_interactive_template <- r"---(
# Interactive plot code (plotly)
# Available: df (processed data), input (UI inputs)  
# Return: htmltools tagList object

# Ensure required libraries are available
library(plotly)
library(data.table)
library(htmltools)

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
  tryCatch({
    smooth_fit <- loess(value ~ x_axis, data = plot_df)
    p <- p |> add_trace(
      type = 'scatter', mode = 'lines',
      x = plot_df$x_axis, y = fitted(smooth_fit),
      line = list(color = 'rgba(255,0,0,0.5)', dash = 'dot'),
      name = 'Trend Line', showlegend = TRUE
    )
  }, error = function(e) {
    lm_fit <- lm(value ~ x_axis, data = plot_df)
    p <- p |> add_trace(
      type = 'scatter', mode = 'lines',
      x = plot_df$x_axis, y = fitted(lm_fit),
      line = list(color = 'rgba(255,0,0,0.5)', dash = 'dot'),
      name = 'Linear Trend', showlegend = TRUE
    )
  })
}

# Add reference lines
shapes_list <- list()
if (!is.null(input$hline_1) && !is.na(input$hline_1)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = 0, x1 = 1, xref = "paper", y0 = input$hline_1, y1 = input$hline_1, line = list(color = 'red', dash = 'dash'))
}
if (!is.null(input$hline_2) && !is.na(input$hline_2)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = 0, x1 = 1, xref = "paper", y0 = input$hline_2, y1 = input$hline_2, line = list(color = 'blue', dash = 'dash'))
}
if (!is.null(input$hline_3) && !is.na(input$hline_3)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = 0, x1 = 1, xref = "paper", y0 = input$hline_3, y1 = input$hline_3, line = list(color = 'purple', dash = 'dash'))
}
if (!is.null(input$hline_4) && !is.na(input$hline_4)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = 0, x1 = 1, xref = "paper", y0 = input$hline_4, y1 = input$hline_4, line = list(color = 'orange', dash = 'dash'))
}
if (!is.null(input$vline_1) && !is.na(input$vline_1)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = input$vline_1, x1 = input$vline_1, y0 = 0, y1 = 1, yref = "paper", line = list(color = 'red', dash = 'dash'))
}
if (!is.null(input$vline_2) && !is.na(input$vline_2)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = input$vline_2, x1 = input$vline_2, y0 = 0, y1 = 1, yref = "paper", line = list(color = 'blue', dash = 'dash'))
}
if (!is.null(input$vline_3) && !is.na(input$vline_3)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = input$vline_3, x1 = input$vline_3, y0 = 0, y1 = 1, yref = "paper", line = list(color = 'purple', dash = 'dash'))
}
if (!is.null(input$vline_4) && !is.na(input$vline_4)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = input$vline_4, x1 = input$vline_4, y0 = 0, y1 = 1, yref = "paper", line = list(color = 'orange', dash = 'dash'))
}

# Add layout and labels
p <- p |> layout(
  title = list(text = if(!is.null(input$plot_title) && nzchar(input$plot_title)) input$plot_title else 'Interactive Plot', font = list(size = title_size)),
  xaxis = list(title = list(text = x_label, font = list(size = xaxis_size)), tickfont = list(size = xaxis_size * 0.9)),
  yaxis = list(title = list(text = if(!is.null(input$plot_ylabel)) input$plot_ylabel else 'Y Axis', font = list(size = yaxis_size)), tickfont = list(size = yaxis_size * 0.9)),
  hovermode = 'x unified',
  legend = list(orientation = 'h', y = -0.2),
  shapes = shapes_list,
  annotations = list(
    list(
      x = 1, y = 0, xref = 'paper', yref = 'paper',
      text = if(!is.null(input$plot_caption) && nzchar(input$plot_caption)) input$plot_caption else '',
      showarrow = FALSE, xanchor = 'right', yanchor = 'bottom',
      font = list(size = 10, color = 'grey')
    )
  )
)

# JavaScript for dynamic subtitles and table
js_code <- r"(
function(el, x) {
  if (typeof el._originalDataNames === 'undefined') {
    el._originalDataNames = el.data.map(trace => trace.name);
  }
  const tableContainer = document.getElementById('dynamic-stats-table-container');
  function updateDynamicElements(eventdata) {
    let new_names = [];
    let full_view = (eventdata && eventdata['xaxis.autorange']) || !eventdata;
    let tableHtml = '<table id=\"dynamic-stats-table\"><thead><tr><th>Trace Name</th><th>Min</th><th>Max</th></tr></thead><tbody>';
    for (let i = 0; i < el.data.length; i++) {
      let trace = el.data[i];
      let original_name = el._originalDataNames[i];
      if (!original_name || trace.mode !== 'lines') {
        new_names.push(original_name);
        continue;
      }
      let x_range;
      if(full_view){
        x_range = [Math.min(...trace.x), Math.max(...trace.x)];
      } else {
        x_range = [eventdata['xaxis.range[0]'], eventdata['xaxis.range[1]']];
      }
      let visible_y = [];
      for (let j = 0; j < trace.x.length; j++) {
        if (trace.x[j] >= x_range[0] && trace.x[j] <= x_range[1]) {
          visible_y.push(trace.y[j]);
        }
      }
      if (visible_y.length > 0) {
        let min_val = Math.min(...visible_y).toFixed(2);
        let max_val = Math.max(...visible_y).toFixed(2);
        new_names.push(original_name + ' (Min: ' + min_val + ', Max: ' + max_val + ')');
        tableHtml += '<tr><td>' + original_name + '</td><td>' + min_val + '</td><td>' + max_val + '</td></tr>';
      } else {
        new_names.push(original_name + ' (No data in view)');
        tableHtml += '<tr><td>' + original_name + '</td><td colspan=\"2\">No data in view</td></tr>';
      }
    }
    tableHtml += '</tbody></table>';
    Plotly.restyle(el, {name: new_names});
    if(tableContainer) {
       tableContainer.innerHTML = '<h4>Visible Data Summary</h4>' + tableHtml;
    }
  }
  updateDynamicElements(null);
  el.on('plotly_relayout', updateDynamicElements);
}
)"

# Attach the JavaScript to the plot object
p <- p |> htmlwidgets::onRender(js_code)

# Create a placeholder for the table and add styling
stats_table_div <- div(
  id = "dynamic-stats-table-container",
  h4("Visible Data Summary"),
  tags$style(HTML("
    #dynamic-stats-table { border-collapse: collapse; width: 50%; margin-top: 10px; font-family: sans-serif; font-size: 14px; }
    #dynamic-stats-table th, #dynamic-stats-table td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    #dynamic-stats-table th { background-color: #f2f2f2; }
  "))
)

# Combine the plot and the table placeholder into a single output
final_output <- navset_card_pill(
nav_panel(title = "plot", p),
nav_panel(title = "table", stats_table_div),
)

# Return the final combined object
final_output
)---"



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
  } else if (input$plot_type == 'interactive') {
    # Execute interactive template code and return plotly object directly
    interactive_plot <- eval(parse(text = interactive_code))
    renderUI({ interactive_plot })
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
