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