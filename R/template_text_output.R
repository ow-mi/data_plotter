# Text Output Template
# Separate Ace Editor Templates for Plotting

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