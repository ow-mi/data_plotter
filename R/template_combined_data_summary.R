# Combined Data Summary Template

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
  datatable(summary_dt, options = list(scrollX = TRUE, pageLength = 100), rownames = FALSE, filter='top', class='compact stripe')
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