# Data Table Display Template
# For Data Table Display (ui_data_table_display)

DT_head <- "# R code to display a table.
# df: The input data.table.
# datatable() is commonly used.
# Example:
# datatable(head(df, 100), options = list(scrollX = TRUE, pageLength = 5), rownames = FALSE, filter = 'top', class='compact stripe')
if (is.data.frame(df) || is.data.table(df)) {
  datatable(head(df, 100), 
                options = list(scrollX = TRUE, 
                # scrollY = '300px', 
                pageLength = 100, 
                searching = TRUE, 
                lengthMenu = c(10, 25, 50, 100)
                ), 
                rownames = FALSE, 
                filter = 'none', # 'top' can be slow for wide tables
                class='compact stripe hover cell-border', 
                width='100%',
                escape = FALSE) # escape=FALSE if you have HTML in cells, be careful with XSS
} else {
  df # If not a data frame (e.g., summary output from skimr)
}
"