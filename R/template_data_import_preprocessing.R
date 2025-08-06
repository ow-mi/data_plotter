# Data Import Pre-processing Template
# For Data Import Pre-processing (ui_data_importer)

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