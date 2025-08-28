ui_data_combiner <- function(id) {
  ns <- NS(id)
  div(class = "module-container",
    layout_sidebar(
      sidebar = sidebar(
        title = "Combined Data Info",
        position = "left",
        width = 350,
        div(class = "text-center",
          icon("database", class = "fa-2x text-primary mb-2"),
          h5("Combined Data", class = "mb-3"),
          p(class = "text-muted small", 
            "Automatically combines processed data from all active Data Import tabs."
          ),
          p(class = "text-muted small",
            "Data updates in real-time when import tabs finish processing."
          )
        ),
        
        # Enhanced Filtering Controls
        div(class = "mb-3",
          h6("Smart Data Filtering", class = "text-muted mb-2"),
          p(class = "text-muted small mb-2", 
            "Format: column:value1;value2 | column:valueA;valueB"
          ),
          p(class = "text-muted small mb-3", 
            'Use quotes for dynamic evaluation: dut:"df[,unique(dut)]"'
          ),
          
          textAreaInput(
            ns("custom_filter"),
            "Filtering",
            placeholder = 'dut:"df[,unique(dut)]" | run:1;2;3 | series:temp.*',
            width = "100%",
            height = "80px"
          ) |> tooltip("
          # Syntax: col1: valA, -valB | col2: valC
          # - `|` separates filters for different columns.
          # - `:` separates a column from its values.
          # - `,` separates multiple values for the same column.
          # - `-` prefix excludes a value.
          # - Use single/double quotes for dynamic evaluation: dut:'df[,unique(dut)]'
          "),
          
          
          div(class = "row g-1 mt-2",
            div(class = "col-8",
              actionButton(
                ns("apply_combiner_filters"),
                "Process Data with Code",
                class = "btn-primary btn-sm w-100",
                icon = icon("play")
              )
            ),
            div(class = "col-4",
              actionButton(
                ns("refresh_data_check"),
                "Check Data",
                class = "btn-outline-info btn-sm w-100",
                icon = icon("refresh")
              )
            )
          )
        ),
        
        # Status Display
        div(class = "mb-3",
          h6("Status", class = "text-muted mb-2"),
          verbatimTextOutput("combiner_status", placeholder = TRUE)
        )
      ),
      
      # Main content with right sidebar for ace editor (similar to plotter)
      layout_sidebar(
        sidebar = sidebar(
          title = "Processing Code",
          width = 600,
          position = "right",
          aceEditor_pre(ns("r_code_combined_data_processing"), value = r_code_combined_data_processing_template),
          open = FALSE
        ),
        
        navset_card_pill(
        nav_panel(
          "Summary",
          icon = icon("chart-bar"),
          ui_data_table_display(
            ns("combined_data_summary"),
            r_code_on_df = if (exists("r_code_combined_data_summary")) r_code_combined_data_summary else "# Default summary code\nif (is.null(df) || nrow(df) == 0) {\n  data.frame(Message = 'No data available') |> \n    datatable(options = list(dom = 't'), rownames = FALSE, class = 'compact')\n} else {\n  summary_df <- df[, .(\n    Records = .N,\n    Min_Value = min(value, na.rm = TRUE),\n    Max_Value = max(value, na.rm = TRUE),\n    Mean_Value = mean(value, na.rm = TRUE)\n  ), by = .(Series = series)]\n  \n  datatable(summary_df, \n    options = list(scrollX = TRUE, pageLength = 100), \n    rownames = FALSE, class = 'compact stripe'\n  )\n}"
          )
        ),
        nav_panel(
          "Sample Data",
          icon = icon("table"),
          ui_data_table_display(
            ns("combined_data_sample"),
            r_code_on_df = if (exists("r_code_combined_data_sample")) r_code_combined_data_sample else "# Default sample code\nif (is.null(df) || nrow(df) == 0) {\n  data.frame(Message = 'No data available') |> \n    datatable(options = list(dom = 't'), rownames = FALSE, class = 'compact')\n} else {\n  sample_df <- head(df, 100)\n  datatable(sample_df, \n    options = list(scrollX = TRUE, pageLength = 100), \n    rownames = FALSE, class = 'compact stripe'\n  )\n}"
          )
        ),
        nav_panel(
          "File Info",
          icon = icon("file-alt"),
          ui_data_table_display(
            ns("combined_data_file_info"),
            r_code_on_df = "
# File information summary
if (is.null(df) || nrow(df) == 0) {
  data.frame(Message = 'No data available') |> 
    datatable(options = list(dom = 't'), rownames = FALSE, class = 'compact')
} else {
  file_info <- df[, .(
    Records = .N,
    Date_Range = paste(
      format(min(timestamp, na.rm = TRUE), '%Y-%m-%d %H:%M'),
      'to',
      format(max(timestamp, na.rm = TRUE), '%Y-%m-%d %H:%M')
    ),
    Unique_Series = length(unique(series)),
    Processing_Source = paste(unique(source_importer_id), collapse = ', ')
  ), by = .(File = file_name_source)][order(-Records)]
  
  datatable(file_info, 
    options = list(scrollX = TRUE, pageLength = 100, searching = TRUE), 
    rownames = FALSE, filter='top', class='compact stripe'
  )
}
            "
          )
        ),
        nav_panel(
          "Data Quality",
          icon = icon("check-circle"),
          ui_data_table_display(
            ns("combined_data_quality"),
            r_code_on_df = "
# Data quality metrics
if (is.null(df) || nrow(df) == 0) {
  data.frame(Message = 'No data available') |> 
    datatable(options = list(dom = 't'), rownames = FALSE, class = 'compact')
} else {
  quality_metrics <- df[, .(
    Total_Points = .N,
    Missing_Values = sum(is.na(value)),
    Missing_Percent = round(sum(is.na(value)) / .N * 100, 2),
    Zero_Values = sum(value == 0, na.rm = TRUE),
    Negative_Values = sum(value < 0, na.rm = TRUE),
    Min_Value = round(min(value, na.rm = TRUE), 4),
    Max_Value = round(max(value, na.rm = TRUE), 4),
    Has_Duplicates = any(duplicated(timestamp))
  ), by = .(Series = series)][order(-Total_Points)]
  
  datatable(quality_metrics, 
    options = list(scrollX = TRUE, pageLength = 100, searching = TRUE), 
    rownames = FALSE, filter='top', class='compact stripe'
  ) |> formatStyle('Missing_Percent', 
    backgroundColor = styleInterval(c(5, 20), c('lightgreen', 'yellow', 'lightcoral'))
  )
}
            "
          )
        ),
        nav_panel(
          "Columns",
          icon = icon("columns"),
          ui_data_table_display(
            ns("combined_data_columns"),
            r_code_on_df = "
# Column information
if (is.null(df) || nrow(df) == 0) {
  data.frame(Message = 'No data available') |> 
    datatable(options = list(dom = 't'), rownames = FALSE, class = 'compact')
} else {
  col_info <- data.frame(
    Column = names(df),
    Type = sapply(df, function(x) class(x)[1]),
    Sample_Values = sapply(df, function(x) {
      if (is.numeric(x)) {
        paste(round(head(x[!is.na(x)], 3), 3), collapse = ', ')
      } else {
        paste(head(x[!is.na(x)], 3), collapse = ', ')
      }
    }),
    Missing_Count = sapply(df, function(x) sum(is.na(x))),
    Unique_Count = sapply(df, function(x) length(unique(x[!is.na(x)]))),
    stringsAsFactors = FALSE
  )
  
  datatable(col_info, 
    options = list(scrollX = TRUE, pageLength = 100, searching = FALSE), 
    rownames = FALSE, class='compact stripe'
  )
}
            "
          )
        ),
        nav_panel(
          "Processing Log",
          icon = icon("history"),
          ui_data_table_display(
            ns("combined_data_log"),
            r_code_on_df = "
# Processing status and log
if (is.null(df) || nrow(df) == 0) {
  data.frame(
    Status = 'No Data',
    Message = 'No files have been processed yet. Upload and process files in Data Import tabs.',
    Timestamp = Sys.time()
  ) |> datatable(options = list(dom = 't'), rownames = FALSE, class = 'compact')
} else {
  log_info <- data.frame(
    Status = 'Data Available',
    Total_Records = nrow(df),
    Unique_Files = length(unique(df$file_name_source)),
    Unique_Series = length(unique(df$series)),
    Date_Range = paste(
      format(min(df$timestamp, na.rm = TRUE), '%Y-%m-%d %H:%M'),
      'to',
      format(max(df$timestamp, na.rm = TRUE), '%Y-%m-%d %H:%M')
    ),
    Sources = paste(unique(df$source_importer_id), collapse = ', '),
    Last_Updated = format(Sys.time(), '%Y-%m-%d %H:%M:%S'),
    stringsAsFactors = FALSE
  )
  
  datatable(log_info, 
    options = list(dom = 't', ordering = FALSE, pageLength = 100), 
    rownames = FALSE, class='compact'
  )
}
            "
          )
        )
      ) # Close navset_card_pill
    ) # Close layout_sidebar 
    )
  ) # Close module-container div
}