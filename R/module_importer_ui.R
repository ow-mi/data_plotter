ui_data_importer <- function(id) {
  ns <- NS(id)
  
  # Main content area with tabs - uses dynamic height
  div(class = "module-container",
    navset_card_pill(
      id = ns("main_tabs"),
      # full_screen = TRUE,
      # height now managed by CSS and JavaScript
      height = "100%",
    
      # Files Tab with its own sidebar
      nav_panel(
      title = "Files",
      icon = icon("folder"),
      layout_sidebar(
        sidebar = sidebar(
          title = "File Controls",
          width = 350, # Increased from 280
          class = "p-2", # Reduced padding
          

          
          # File Filtering (primary selection method)
          div(class = "mb-2",
            h6("File Filtering", class = "text-muted mb-1 small", style = "font-weight: 600;"),
            p(class = "text-muted small mb-2", "Use these filters to control which files are processed:"),
            div(class = "row g-1",
              div(class = "col-12",
                textInput(
                  ns("filter_in_files_1"),
                  NULL,
                  placeholder = "Include files (regex) - leave empty for all",
                  width = "100%"
                ) |> tooltip("Regex pattern to include only matching files. Leave empty to include all files.")
              ),
              div(class = "col-12",
                textInput(
                  ns("filter_out_files_1"),
                  NULL,
                  placeholder = "Exclude files (regex) - e.g., '_backup|temp'",
                  width = "100%"
                ) |> tooltip("Regex pattern to exclude matching files. Applied after include filter.")
              )
            )
          ),
          
          # Custom Tab Name (compact)
          div(class = "mb-2",
            h6("Tab Name", class = "text-muted mb-1 small"),
            textInput(
              ns("tab_name"),
              NULL,
              value = "",
              placeholder = "Custom name...",
              width = "100%"
            ) |> tooltip("Enter a custom name for this data import tab - applies automatically")
          )
        ),
        
        # Files content
        card_body(
          h5("Uploaded Files", class = "card-title"),
          ui_data_table_display(ns("data_table_input_file_list"))
        )
      )
    ),
    
    # Preview Tab with its own sidebar
    nav_panel(
      title = "Preview", 
      icon = icon("eye"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Processing Controls",
          width = 350, # Increased from 280
          class = "p-2", # Reduced padding

          # Process All Files Button
          div(class = "mt-auto",
            actionButton(
              ns("combine_data"),
              "Process All Files",
              icon = icon("cogs"),
              class = "btn-success w-100",
              style = "margin-top: 10px;"
            ) |> tooltip("Process and combine all files with current settings")
          ),

          # Preview File Selection (compact)
          div(class = "mb-2",
            selectInput(
              ns("input_preview_file_selector"),
              "Preview File",
              choices = NULL,
              width = "100%"
            ) |> tooltip("Choose a file to preview in the Preview tab"),
            
            actionButton(
              ns("reload_data"),
              "Reload",
              icon = icon("refresh"),
              class = "btn-outline-secondary btn-sm w-100",
              style = "margin-top: 2px;"
            ) |> tooltip("Refresh the data preview with current settings")
          ),
          
          # Processing Controls (compact)
          div(class = "mb-2",
            h6("Processing", class = "text-muted mb-1 small"),
            div(class = "row g-1",
              div(class = "col-6",
                numericInput(
                  ns("skip_n_rows_start"),
                  "Skip Rows",
                  value = 0, min = 0,
                  width = "100%"
                ) |> tooltip("Number of rows to skip at the beginning of each file")
              ),
              div(class = "col-6",
                numericInput(
                  ns("file_skip_every_nth"), 
                  "Sample Nth",
                  value = 0, min = 0,
                  width = "100%"
                ) |> tooltip("Read every Nth row (0 = all rows, 2 = every 2nd row, etc.)")
              )
            )
          ),
          
          # Data Transformation (compact)
          div(class = "mb-2",
            h6("Transformation", class = "text-muted mb-1 small"),
            textInput(
              ns("filter_in_1"),
              NULL,
              placeholder = "Filter in pattern",
              width = "100%"
            ) |> tooltip("Regex to include only matching rows"),
            textInput(
              ns("filter_out_1"), 
              NULL,
              placeholder = "Filter out pattern",
              width = "100%"
            ) |> tooltip("Regex to exclude matching rows"),
            textInput(
              ns("rename_1"),
              NULL,
              placeholder = "old:new,old2:new2",
              width = "100%"
            ) |> tooltip("Rename columns using old:new format"),
            textInput(
              ns("date_format"),
              NULL,
              placeholder = "ymd_HMS, dmy, etc.",
              width = "100%"
            ) |> tooltip("Lubridate format for parsing dates")
          ),
          
        ),
        
        # Preview content with tabs
        navset_card_pill(
          nav_panel(
            "Raw Data",
            icon = icon("table"),
            ui_data_table_display(ns("data_table_input_data"), r_code_on_df = DT_head)
          ),
          nav_panel(
            "Processed Data", 
            icon = icon("filter"),
            ui_data_table_display(ns("data_table_modified_data"), r_code_on_df = DT_head)
          ),
          nav_panel(
            "Summary Table",
            icon = icon("chart-bar"),
            ui_data_table_display(ns("data_table_modified_data_sum"), r_code_on_df = if (exists("r_code_combined_data_summary")) r_code_combined_data_summary else "# Default summary code\nif (is.null(df) || nrow(df) == 0) {\n  data.frame(Message = 'No data available') |> \n    datatable(options = list(dom = 't'), rownames = FALSE, class = 'compact')\n} else {\n  summary_df <- df[, .(\n    Records = .N,\n    Min_Value = min(value, na.rm = TRUE),\n    Max_Value = max(value, na.rm = TRUE),\n    Mean_Value = mean(value, na.rm = TRUE)\n  ), by = .(Series = series)]\n  \n  datatable(summary_df, \n    options = list(scrollX = TRUE, pageLength = 15), \n    rownames = FALSE, class = 'compact stripe'\n  )\n}")
          ),
          nav_panel(
            "Skimrdsf",
                          icon = icon("chart-line"),
            ui_data_table_display(ns("data_table_skim"), r_code_on_df = 
               "df |> skim() |> renderPrint()" # Using renderPrint for skim output
            )
          )
        )
      )
    ),
    
    # Process Tab with its own sidebar (added)
    nav_panel(
      title = "Code",
      icon = icon("cogs"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Actions",
          width = 240,
          
          # Process All Files (moved from main sidebar to bottom)
        ),
        
        # Process content
        layout_columns(
          col_widths = c(6, 6),
          
          # Pre-processing
          card(
            card_header("Pre-processing Code"),
            card_body(
              p("R code to read and initially process each file:", class = "text-muted small"),
              aceEditor_pre(ns("r_code_pre_process"), value = r_code_data_pre_process)
            )
          ),
          
          # Post-processing  
          card(
            card_header("Post-processing Code"),
            card_body(
              p("R code to transform each processed file:", class = "text-muted small"),
              aceEditor_pre(ns("r_code_post_process"), value = r_code_data_processing)
            )
                      )
          )
        )
      )
    )
  ) # Close module-container div
}