# Combined Data Sample Template

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