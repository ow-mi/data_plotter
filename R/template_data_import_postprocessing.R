# Data Import Post-processing Template
# For Data Import Post-processing (ui_data_importer)

r_code_data_processing <- "
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