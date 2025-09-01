# Enhanced Combined Data Processing Template
# For Data Combiner Module with Smart Filtering

r_code_combined_data_processing_template <- r"---(
# R code to process the COMBINED data from all importers.
# Available variables:
# - df: The data.table of combined data from all importers (raw combined).
# - input: Reactive list of UI inputs for the data combiner module.
# - filter_in(), filter_out(), rname(), string_eval(): Helper functions.
# Required output: A processed data.table.

# Ensure df is a data.table
if (!is.null(df)) {
  setDT(df)
} else {
  # Return empty data.table if no input data
  return(data.table())
}

# === New Filtering System ===
# Syntax: col1: valA, -valB | col2: valC
# - `|` separates filters for different columns.
# - `:` separates a column from its values.
# - `,` separates multiple values for the same column.
# - `-` prefix excludes a value.

# Helper function to parse and apply the new filter syntax
# Helper function to parse and apply the new filter syntax with range support
apply_custom_filters <- function(df, filter_string) {
  # If the filter string is empty, return the original data.table
  if (is.null(filter_string) || trimws(filter_string) == '') {
    return(df)
  }
  
  # Create an evaluation environment for dynamic expressions
  eval_env <- new.env(parent = globalenv())
  eval_env$df <- df
  
  # Process dynamic R code within quotes
  processed_string <- string_eval(filter_string, eval_env)
  
  # Split the string into separate column filters by '|'
  column_filters <- strsplit(processed_string, '\\|')[[1]]
  
  # Loop through each column filter
  for (filter_part in column_filters) {
    parts <- strsplit(trimws(filter_part), ':', fixed = TRUE)[[1]]
    if (length(parts) != 2) next
    
    column <- trimws(parts[1])
    values_string <- trimws(parts[2])
    
    if (!column %in% names(df)) {
      warning(paste("Column '", column, "' not found. Skipping filter.", sep=""))
      next
    }
    
    values <- trimws(strsplit(values_string, ',')[[1]])
    values <- values[values != '']
    
    # Separate values into 'include' and 'exclude' lists for equality checks
    include_values <- c()
    exclude_values <- c()
    
    # Apply range filters first
    for (val in values) {
      if (startsWith(val, '>=')) {
        comp_val <- trimws(sub('^>=', '', val))
        df <- df[get(column) >= comp_val]
      } else if (startsWith(val, '<=')) {
        comp_val <- trimws(sub('^<=', '', val))
        df <- df[get(column) <= comp_val]
      } else if (startsWith(val, '>')) {
        comp_val <- trimws(sub('^>', '', val))
        df <- df[get(column) > comp_val]
      } else if (startsWith(val, '<')) {
        comp_val <- trimws(sub('^<', '', val))
        df <- df[get(column) < comp_val]
      } else if (startsWith(val, '-')) {
        exclude_values <- c(exclude_values, sub('^-', '', val))
      } else {
        include_values <- c(include_values, val)
      }
    }
    
    # Apply the equality filters last
    if (length(include_values) > 0) {
      df <- filter_in(df, column, include_values)
    }
    if (length(exclude_values) > 0) {
      df <- filter_out(df, column, exclude_values)
    }
  }
  
  return(df)
}

# --- Main Filtering Logic ---
# Apply the filters from a single input string. 
# We assume the input is named `input$custom_filter`.
df <- apply_custom_filters(df, input$custom_filter)


# Custom processing section - Add any additional data transformations here
# Examples:
# - Data aggregation: df <- df[, .(mean_value = mean(value)), by = .(series, hour = hour(timestamp))]
# - Smart sampling: df <- df[, .SD[sample(.N, min(.N, 1000))], by = series]

# Return the processed data
df
)---"