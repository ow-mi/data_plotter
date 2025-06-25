# app.R
library(shiny)
library(shinyTime)
library(shinyjqui) # For jqui_sortable, though dataProcessor module is removed
library(bslib)
library(shinyAce)
library(shinyFiles)
library(stringr)
library(data.table)
library(DT)
library(R.utils) # For gunzip, though fread handles .gz directly
library(ggplot2)
library(plotly)
library(lubridate)
library(jsonlite)
library(fasttime) # Used in r_code_data_processing
library(readxl)
library(fst)
library(nanoparquet)
library(tools) # For file_ext
library(scattermore) # Used in r_code_plot_process_template and ggplot_template
library(purrr) # For map2 used in server_data_import
library(promises)
library(future)
plan(multisession) # Or multicore if appropriate for the environment
library(rstudioapi) # Potentially for development, not strictly runtime
library(skimr) # Used in r_code_combined_data_summary and default DT displays
library(htmlwidgets) # For saveWidget
library(spsComps) # For shinyCatch
library(base64enc) # For folder upload decoding
# Source helper functions and default R code strings
source("./functions.R")
source("./r_code_.R")

# Custom downloadButton to prevent default browser download behavior 
# for dynamic content
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL # Remove the download attribute
  tag
}

# --- UI Module Definitions ---

ui_plotter <- function(id) {
  ns <- NS(id) # Ensure ns is defined for the main module ID
  navset_card_pill(
    id = ns("plotter_tabs"),
    full_screen = TRUE,
    
    # Input Data Tab
    nav_panel(
      title = "Input Data",
      icon = icon("database"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Plot Settings",
          width = 300,
          class = "p-2",
          
          # Plot Name (auto-apply)
          div(class = "mb-3",
            h6("Plot Name", class = "text-muted mb-1 small"),
            textInput(
              ns("plot_name"),
              NULL,
              value = "",
              placeholder = "Custom plot name...",
              width = "100%"
            ) |> tooltip("Enter a custom name for this plotter tab - applies automatically")
          )
        ),
        
        # Two tables side by side
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Input Data (Raw from Combiner)"),
            ui_data_table_display(
              ns("plot_data_table_input_data"), 
              r_code_on_df = DT_head
            )
          ),
          card(
            card_header("Input Data Summary"),
            ui_data_table_display(
              ns("plot_data_table_input_data_summary"), 
              r_code_on_df = r_code_combined_data_summary
            )
          )
        )
      )
    ),
    
    # Data Processing Tab
    nav_panel(
      title = "Processing Data",
      icon = icon("cogs"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Processing Controls",
          width = 350,
          class = "p-2",
          
          # Data Processing Action
          div(class = "mb-3",
            actionButton(
              ns("data_render"), 
              "Process Data",
              icon = icon("play", lib = "font-awesome"),
              class = "btn-primary w-100",
              style = "margin-bottom: 10px;"
            ) |> tooltip("Apply data processing settings and prepare data for plotting")
          ),
          div(class = "mb-2",
            h6("Sampling", class = "text-muted mb-1 small"),
            numericInput(
              ns("sample_n_plot"),
              "Sample Fraction",
              value = 0, min = 0, max = 1, step = 0.1,
              width = "100%"
            ) |> tooltip("Fraction of data to use (0 = all data, 0.1 = 10%, etc.)")
          ),
          
          # Source Filtering (compact)
          div(class = "mb-2",
            h6("Source Filtering", class = "text-muted mb-1 small"),
            uiOutput(ns("source_filter_checkboxes"))
          ),
          
          # Data Sampling (compact)
          div(class = "mb-2",
            h6("File Filtering", class = "text-muted mb-1 small"),
            textInput(
              ns("filter_in_files_plot"), 
              NULL,
              placeholder = "Include files (regex)",
              width = "100%"
            ) |> tooltip("Include only files matching these patterns (comma-separated, supports regex)"),
            textInput(
              ns("filter_out_files_plot"), 
              NULL,
              placeholder = "Exclude series (regex)",
              width = "100%"
            ) |> tooltip("Exclude files matching these patterns (comma-separated, supports regex)")
          ),
          
          # Series Filtering (compact)
          div(class = "mb-2",
            h6("Series Filtering", class = "text-muted mb-1 small"),
            textInput(
              ns("filter_in_plot"), 
              NULL,
              placeholder = "Include series (regex)",
              width = "100%"
            ) |> tooltip("Include only series matching these patterns (comma-separated, supports regex)"),
            textInput(
              ns("filter_out_plot"), 
              NULL,
              placeholder = "Exclude series (regex)",
              width = "100%"
            ) |> tooltip("Exclude series matching these patterns (comma-separated, supports regex)"),
            textInput(
              ns("rename_plot"), 
              NULL,
              placeholder = "Rename: old1,new1,old2,new2",
              width = "100%"
            ) |> tooltip("Rename series: old1,new1,old2,new2 (comma-separated pairs)")
          ),
          
          # Time Filtering (compact)
          div(class = "mb-2",
            h6("Time Filtering", class = "text-muted mb-1 small"),
            # Start Date and Time
            div(class = "mb-1",
              p(class = "text-muted small mb-1", "Start"),
              div(class = "row g-1",
                div(class = "col-7",
                  dateInput(
                    ns("filter_time_start_date"), 
                    NULL,
                    value = as.Date("1970-01-01"),
                    width = "100%"
                  ) |> tooltip("Start date for filtering")
                ),
                div(class = "col-5",
                  timeInput(
                    ns("filter_time_start_time"), 
                    NULL,
                    value = strptime("00:00:00", "%T"),
                    # width = "100%"
                  ) |> tooltip("Start time")
                )
              )
            ),
            # End Date and Time
            div(class = "mb-1",
              p(class = "text-muted small mb-1", "End"),
              div(class = "row g-1",
                div(class = "col-7",
                  dateInput(
                    ns("filter_time_end_date"), 
                    NULL,
                    value = Sys.Date(),
                    width = "100%"
                  ) |> tooltip("End date for filtering")
                ),
                div(class = "col-5",
                  timeInput(
                    ns("filter_time_end_time"), 
                    NULL,
                    value = strptime("23:59:59", "%T"),
                    # width = "100%"
                  ) |> tooltip("End time")
                )
              )
            )
          )
        ),
        
        # Processing Data Content with right sidebar
        layout_sidebar(
          sidebar = sidebar(
            title = "Processing R Code",
            width = 600,
            position = "right",
            aceEditor_pre(
              ns("r_code_plot_process"), 
              value = r_code_plot_process_template
            ),
            open = FALSE
          ),
          
          # Two tables side by side for processed data
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header("Processed Data (after R code)"),
              ui_data_table_display(
                ns("plot_data_table_modified_data"), 
                r_code_on_df = DT_head
              )
            ),
            card(
              card_header("Processed Data Summary"),
              ui_data_table_display(
                ns("plot_data_table_modified_data_summary"), 
                r_code_on_df = r_code_combined_data_summary
              )
            )
          )
        )
      )
    ),
    
    # Plotting Tab
    nav_panel(
      title = "Plotting",
      icon = icon("chart-line"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Plot Controls",
          width = 350,
          class = "p-2",
          
          # Main Action Row (compact)
          div(class = "mb-3",
            actionButton(
              ns("plot_render"), 
              "Create Plot",
              icon = icon("chart-line", lib = "font-awesome"), 
              class = "btn-success w-100",
              style = "margin-bottom: 5px;"
            ) |> tooltip("Generate plot with current settings"),
            selectInput(
              ns("plot_type"), 
              NULL, 
              choices = c("Interactive" = "dynamic", "Static" = "static", "Text" = "text", "Table" = "table"), 
              selected = "dynamic",
              width = "100%"
            ) |> tooltip("Choose plot output format: Interactive (plotly), Static (PNG), Text summary, or Table"),
            
            # Fast rendering only for static plots - moved here
            conditionalPanel(
              condition = paste0("input['", ns("plot_type"), "'] == 'static'"),
              checkboxInput(
                ns("plot_vector"), 
                "Fast Rendering (Large Data)",
                value = FALSE
              ) |> tooltip("Use scattermore for faster rendering of large datasets (>10k points)")
            )
          ),
          
          # Labels & Titles (compact)
          div(class = "mb-2",
            h6("Labels & Titles", class = "text-muted mb-1 small"),
            textInput(
              ns("plot_title"), 
              NULL,
              placeholder = "Plot title...",
              width = "100%"
            ) |> tooltip("Main title for the plot"),
            div(class = "row g-1",
              div(class = "col-6",
                selectizeInput(
                  ns("plot_xlabel"),
                  NULL,
                  choices = c("Timestamp", "Duration Days", "Duration Hours", "Duration Minutes", "Duration Seconds"),
                  options = list(create = TRUE, placeholder = "X-axis...")
                ) |> tooltip("X-axis label and transformation")
              ),
              div(class = "col-6",
                selectizeInput(
                  ns("plot_ylabel"),
                  NULL,
                  choices = c("Temperature [°C]", "Current [A]", "Value"),
                  selected = "Value",
                  options = list(create = TRUE, placeholder = "Y-axis...")
                ) |> tooltip("Y-axis label")
              )
            )
          ),
          
          # Font Sizes (compact)
          div(class = "mb-2",
            h6("Font Sizes", class = "text-muted mb-1 small"),
            div(class = "row g-1",
              div(class = "col-4",
                numericInput(
                  ns("title_font_size"),
                  "Title",
                  value = 14, min = 8, max = 24,
                  width = "100%"
                ) |> tooltip("Font size for plot title")
              ),
              div(class = "col-4",
                numericInput(
                  ns("xaxis_font_size"),
                  "X-Axis",
                  value = 12, min = 8, max = 20,
                  width = "100%"
                ) |> tooltip("Font size for X-axis label and ticks")
              ),
              div(class = "col-4",
                numericInput(
                  ns("yaxis_font_size"),
                  "Y-Axis",
                  value = 12, min = 8, max = 20,
                  width = "100%"
                ) |> tooltip("Font size for Y-axis label and ticks")
              )
            )
          ),
          
          # Aesthetics (compact)
          div(class = "mb-2",
            h6("Aesthetics", class = "text-muted mb-1 small"),
            div(class = "row g-1",
              div(class = "col-6",
                selectInput(
                  ns("plot_color"), 
                  "Color By", 
                  choices = c("None" = "null"),
                  selected = "null"
                ) |> tooltip("Group data by color using this variable")
              ),
              div(class = "col-6",
                selectInput(
                  ns("plot_linetype"), 
                  "Line Type", 
                  choices = c("None" = "null"),
                  selected = "null"
                ) |> tooltip("Group data by line type using this variable")
              )
            )
          ),
          
          # Faceting (compact)
          div(class = "mb-2",
            h6("Faceting", class = "text-muted mb-1 small"),
            p(class = "text-muted small mb-1", "Split plot into subplots based on series names"),
            div(class = "row g-1",
              div(class = "col-4",
                numericInput(
                  ns("plot_facet_start"), 
                  "Start",
                  value = 0, min = -1, max = 100,
                  width = "100%"
                ) |> tooltip("Starting character position for series name grouping")
              ),
              div(class = "col-4",
                numericInput(
                  ns("plot_facet_end"), 
                  "End",
                  value = 0, min = -1, max = 100,
                  width = "100%"
                ) |> tooltip("Ending character position for series name grouping")
              ),
              div(class = "col-4",
                numericInput(
                  ns("plot_facet_nrow"), 
                  "Rows",
                  value = 2, min = 1, max = 100,
                  width = "100%"
                ) |> tooltip("Number of rows in faceted layout")
              )
            )
          ),
          
          # Reference Lines (compact)
          div(class = "mb-2",
            h6("Reference Lines", class = "text-muted mb-1 small"),
            # Horizontal Lines
            div(class = "mb-1",
              p(class = "text-muted small mb-1", "Horizontal Lines (Y-values)"),
              div(class = "row g-1",
                div(class = "col-6",
                  numericInput(
                    ns("hline_1"), 
                    "HLine 1",
                    value = NA,
                    width = "100%"
                  ) |> tooltip("Y-value for first horizontal reference line")
                ),
                div(class = "col-6",
                  numericInput(
                    ns("hline_2"), 
                    "HLine 2",
                    value = NA,
                    width = "100%"
                  ) |> tooltip("Y-value for second horizontal reference line")
                )
              )
            ),
            # Vertical Lines
            div(class = "mb-1",
              p(class = "text-muted small mb-1", "Vertical Lines (X-values)"),
              div(class = "row g-1",
                div(class = "col-6",
                  numericInput(
                    ns("vline_1"), 
                    "VLine 1",
                    value = NA,
                    width = "100%"
                  ) |> tooltip("X-value for first vertical reference line")
                ),
                div(class = "col-6",
                  numericInput(
                    ns("vline_2"), 
                    "VLine 2",
                    value = NA,
                    width = "100%"
                  ) |> tooltip("X-value for second vertical reference line")
                )
              )
            ),
            # Best fit line
            div(class = "mb-1",
              checkboxInput(
                ns("add_smooth"), 
                "Add best fit line",
                value = FALSE
              ) |> tooltip("Add a smooth trend line to the plot")
            )
          ),
          
          # Download
          div(class = "mt-auto",
            downloadButton(
              ns("download_output"), 
              "Download Plot",
              icon = icon("download"),
              class = "btn-outline-primary w-100"
            ) |> tooltip("Download the current plot as file")
          )
        ),
        
        # Plot Display Content
        layout_sidebar(
          sidebar = sidebar(
            title = "Plotting R Code",
            width = 600,
            position = "right",
            
            # Tabbed ace editors for different plot types
            navset_card_pill(
              nav_panel(
                "Text",
                icon = icon("file-text"),
                aceEditor_pre(ns("r_code_plot_text"), value = ggplot_text_template)
              ),
              nav_panel(
                "Static",
                icon = icon("image"),
                aceEditor_pre(ns("r_code_plot_static"), value = ggplot_static_template)
              ),
              nav_panel(
                "Interactive", 
                icon = icon("chart-line"),
                aceEditor_pre(ns("r_code_plot_interactive"), value = ggplot_interactive_template)
              ),
              nav_panel(
                "Table",
                icon = icon("table"),
                aceEditor_pre(ns("r_code_plot_table"), value = ggplot_table_template)
              ),
              nav_panel(
                "Final",
                icon = icon("code"),
                aceEditor_pre(ns("r_code_plot_final"), value = ggplot_final_template)
              )
            ),
            open = FALSE
          ),
          card(
            full_screen = TRUE,
            card_header("Generated Output"),
            uiOutput(ns("plot_output")) # Changed to renderUI for flexibility
          )
        )
      )
    )
  )
}


ui_data_importer <- function(id) {
  ns <- NS(id)
  
  # Main content area with tabs - removed main sidebar constraint
  navset_card_pill(
    id = ns("main_tabs"),
    full_screen = TRUE,
    
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
          
          # Process All Files Button
          div(class = "mt-auto",
            actionButton(
              ns("combine_data"),
              "Process All Files",
              icon = icon("cogs"),
              class = "btn-success w-100",
              style = "margin-top: 10px;"
            ) |> tooltip("Process and combine all files with current settings")
          )
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
            ui_data_table_display(ns("data_table_modified_data_sum"), r_code_on_df = r_code_combined_data_summary)
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
}

ui_data_combiner <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        title = "Combined Data Info",
        position = "left",
        width = 280,
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
        
        # Filtering Controls
        div(class = "mb-3",
          h6("Data Filtering", class = "text-muted mb-2"),
          textInput(
            "combiner_filter_source",
            "Filter Sources",
            placeholder = "e.g., module_1|module_2",
            width = "100%"
          ) |> tooltip("Filter by source_importer_id (regex patterns)"),
          
          textInput(
            "combiner_filter_files",
            "Filter Files", 
            placeholder = "e.g., experiment.*\\.csv",
            width = "100%"
          ) |> tooltip("Filter by file_name_source (regex patterns)"),
          
          div(class = "row g-1 mt-2",
            div(class = "col-6",
              actionButton(
                "apply_combiner_filters",
                "Apply Filters",
                class = "btn-primary btn-sm w-100",
                icon = icon("filter")
              )
            ),
            div(class = "col-6", 
              actionButton(
                "clear_combiner_filters",
                "Clear Filters",
                class = "btn-outline-secondary btn-sm w-100",
                icon = icon("eraser")
              )
            )
          )
        ),
        
        # Data Management
        div(class = "mb-3",
          h6("Data Management", class = "text-muted mb-2"),
          actionButton(
            "clear_combined_data",
            "Clear All Data",
            class = "btn-danger btn-sm w-100",
            icon = icon("trash")
          ) |> tooltip("Remove all data from importers and clear combined dataset"),
          
          div(class = "mt-2",
            verbatimTextOutput("combiner_status", placeholder = TRUE)
          )
        )
      ),
      navset_card_pill(
        nav_panel(
          "Summary",
          icon = icon("chart-bar"),
          ui_data_table_display(
            ns("combined_data_summary"),
            r_code_on_df = r_code_combined_data_summary
          )
        ),
        nav_panel(
          "Sample Data",
          icon = icon("table"),
          ui_data_table_display(
            ns("combined_data_sample"),
            r_code_on_df = r_code_combined_data_sample
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
    options = list(scrollX = TRUE, pageLength = 15, searching = TRUE), 
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
    options = list(scrollX = TRUE, pageLength = 15, searching = TRUE), 
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
    options = list(scrollX = TRUE, pageLength = 15, searching = FALSE), 
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
    options = list(dom = 't', ordering = FALSE), 
    rownames = FALSE, class='compact'
  )
}
            "
          )
        )
      )
    )
  )
}

ui_data_table_display <- function(id, r_code_on_df, title = NULL) {
  ns <- NS(id)
  card( # Wrapped in card for better structure
    full_screen = TRUE,
    card_header(
      title,
      popover(
        # Gear icon on the right
        bsicons::bs_icon("gear", class = "ms-auto"),
        title = "Table Display R Code",
        # Switched to aceEditor for consistency, though textAreaInput was fine
        aceEditor_pre(
            ns("code_input"),
            value = r_code_on_df
            # mode = "r",
            # theme = "chrome", # Lighter theme for popover
            # height = "150px",
            # fontSize = 12
        ),
        action_input_tip(
          ns("apply_code"), 
          "Apply Display Code", 
          tip = "Apply R code to customize table view"
        )
      )
    ),
    card_body(
        padding = "0.25rem", # Reduced padding
        uiOutput(ns("data_table")) # For datatable or other renderPrint outputs
    )
  )
}


# --- Main UI Definition ---
ui <- page_navbar(
  theme = bs_theme(bootswatch = "minty", version = 5),
  title = "Data Plotter v1.1",
  id = "mainmenu",

  # Input Data Management Tab
  nav_panel(
    title = "Input Data",
    icon = icon("upload"),
    layout_sidebar(
      sidebar = sidebar(
        title = "File Management",
        width = 400,
        class = "p-3",
        
        # File Upload Section
        div(class = "mb-3",
          h5("Upload Files"),
          
          # File upload buttons in tabs or side by side
          navset_pill(
            nav_panel(
              "Files",
              icon = icon("file"),
              fileInput(
                "global_file_upload",
                "Select Individual Files",
                multiple = TRUE,
                buttonLabel = list(icon("upload"), "Browse Files"),
                placeholder = "No files selected",
                width = "100%"
              ) |> tooltip("Upload individual data files: CSV, Excel (.xlsx), FST, Parquet, or compressed (.csv.gz)")
            ),
            nav_panel(
              "Folder",
              icon = icon("folder"),
              div(
                # Custom folder input with webkitdirectory
                tags$input(
                  id = "global_folder_upload",
                  type = "file",
                  webkitdirectory = NA,  # This enables folder selection
                  multiple = TRUE,
                  style = "width: 100%; padding: 8px; border: 1px solid #ccc; border-radius: 4px; margin-bottom: 8px;"
                ),
                
                # File type filter
                div(class = "mb-2",
                  h6("File Type Filters", class = "text-muted mb-1 small"),
                  textInput(
                    "folder_ignore_extensions",
                    NULL,
                    value = "txt,log,tmp,bak,old,~",
                    placeholder = "txt,log,tmp,bak...",
                    width = "100%"
                  ) |> tooltip("Comma-separated list of file extensions to ignore (without dots). Leave empty to upload all files."),
                  textInput(
                    "folder_allow_extensions",
                    NULL,
                    value = "csv,xlsx,xls,fst,parquet,gz",
                    placeholder = "csv,xlsx,fst,parquet...",
                    width = "100%"
                  ) |> tooltip("Comma-separated list of file extensions to ALLOW (without dots). Leave empty to allow all files (except ignored ones).")
                ),
                
                p(class = "text-muted small", "Select a folder to upload all files within it. Files will be named as: folder-subfolder-filename")
              ) |> tooltip("Upload entire folders - files will be automatically named with folder structure")
            )
          ),
          
          checkboxInput(
            "overwrite_files",
            "Allow file overwrite",
            value = FALSE
          ) |> tooltip("If checked, uploading files with the same name will replace existing files")
        ),
        
        # File Actions
        div(class = "mb-3",
          h5("File Actions"),
          actionButton(
            "remove_selected_files",
            "Remove Selected",
            icon = icon("trash"),
            class = "btn-danger w-100 mb-2"
          ) |> tooltip("Remove selected files from the list"),
          
          actionButton(
            "clear_all_files",
            "Clear All Files",
            icon = icon("trash-alt"),
            class = "btn-outline-danger w-100"
          ) |> tooltip("Remove all uploaded files")
        ),
        
        # File Statistics
        div(class = "mb-3",
          h5("Statistics"),
          verbatimTextOutput("file_stats")
        )
      ),
      
      # Main content - file list
      div(class = "p-3",
        h4("Uploaded Files"),
        p(class = "text-muted", "Manage your uploaded files here. These files will be available in all Data Import tabs."),
        DT::DTOutput("global_file_list")
      )
    )
  ),

  # Dynamic data import tabs will be inserted here
  nav_panel(
    title = "Combined Data", 
    icon = icon("database"),
    ui_data_combiner("combiner")
  ),
  
  # Placeholder for where plots are inserted before
  nav_panel(
    title = "Analysis", 
    icon = icon("chart-area"),
    div(class = "text-center p-5",
      icon("chart-line", class = "fa-3x text-muted mb-3"),
      h4("No Plotters Created Yet"),
             p(class = "text-muted", "Click 'Add Plotter' to create your first plot."),
       p(class = "text-muted small", "Plotters will appear as tabs before this one.")
    )
  ),

  nav_spacer(),
  
  # Utility Tools Menu
  nav_menu(
    title = "Tools",
    icon = icon("tools"),
    align = "right",
    nav_panel(
      title = "R Code Helper",
      icon = icon("code"),
      layout_sidebar(
        sidebar = sidebar(
          title = "R Code Sandbox",
          position = "left",
                     width = 500,
           p(class = "text-muted small", "Test R code in a safe environment"),
          aceEditor_pre("helper_input", value = helper_code_template)
        ),
        div(class = "p-3",
          h5("Code Output"),
          uiOutput("helper_output")
        )
      )
    ),
    nav_panel(
      title = "Batch Download",
      icon = icon("download"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Download Configuration",
          position = "left",
          width = 500,
                     p(class = "text-muted small", "Configure batch download of all plots"),
          aceEditor_pre("downloader_input", value = downloader_code_refactored)
        ),
        div(class = "p-3",
          h5("Download Interface"),
          uiOutput("downloader_output")
        )
      )
    )
  ),
  
  # Templates Menu
  nav_menu(
    title = "Templates",
    icon = icon("bookmark"),
    align = "right",
    nav_item(
      div(class = "px-3 py-2",
        h6("Load Template"),
        fileInput(
          "template_upload", 
          "Upload Template",
          accept = ".csv", 
          width = "100%",
          buttonLabel = list(icon("upload"), "Browse")
        ) |> tooltip("Load a previously saved template configuration")
      )
    ),
    nav_item(hr()),
    nav_item(
      div(class = "px-3 py-2",
        h6("Save Template"),
        textInput(
          "template_file_name", 
          "Filename", 
          value = paste0("template_", Sys.Date()),
          placeholder = "Enter template name..."
        ),
        downloadButton(
          "download_template", 
          "Download Template",
          icon = icon("download"),
          class = "btn-outline-primary",
          width = "100%"
        ) |> tooltip("Save current app settings as a template")
      )
    )
  ),
  
  # Theme Toggle
  nav_item(
    input_dark_mode(mode = "light") |> 
      tooltip("Toggle light/dark theme")
  ),
  
  # Add Data Import
  # nav_menu(
  #   title = "Data",
  #   icon = icon("plus-circle"),
  #   align = "right",
  #   nav_item(
  #     div(class = "px-3 py-2",
  #       action_input_tip(
  #         "insert_importer", 
  #         "Add Data Import Tab",
  #         icon = icon("file-import"), 
  #         class = "btn-info",
  #         width = "100%",
  #         tip = "Create a new data import tab for uploading and processing files"
  #       )
  #     )
  #   )
  # ),
    nav_item(
      div(class = "px-3 py-2",
        action_input_tip(
          "insert_importer", 
          "Add Data Import Tab",
          icon = icon("file-import"), 
          class = "btn-info",
          width = "100%",
          tip = "Create a new data import tab for uploading and processing files"
        )
      )
    ),
    nav_item(
      div(class = "px-3 py-2",
        action_input_tip(
          "insert_plot", 
          "Add Plotter Tab",
          icon = icon("chart-area"), 
          class = "btn-success",
          width = "100%",
          tip = "Create a new plotter for visualizing your combined data"
        )
      )
    ),
  
  # Add Plotter
  # nav_menu(
  #   title = "Plot",
  #   icon = icon("plus-square"),
  #   align = "right",
  #   nav_item(
  #     div(class = "px-3 py-2",
  #       action_input_tip(
  #         "insert_plot", 
  #         "Add Plotter Tab",
  #         icon = icon("chart-area"), 
  #         class = "btn-success",
  #         width = "100%",
  #         tip = "Create a new plotter for visualizing your combined data"
  #       )
  #     )
  #   )
  # )
)

# Add JavaScript for dynamic tab renaming and folder upload
ui <- tagList(
  ui,
  tags$script(HTML("
    console.log('Tab renaming JavaScript loaded');
    
    // Store tab mappings to help with repeated renames
    window.tabMappings = window.tabMappings || {};
    
    // Handle custom message for selecting/deselecting all plots
    Shiny.addCustomMessageHandler('selectAllPlots', function(data) {
      var checkboxes = document.querySelectorAll('input[id^=\"select_plot_\"]');
      checkboxes.forEach(function(checkbox) {
        checkbox.checked = data.select;
      });
    });
    
    // Folder upload handler
    $(document).ready(function() {
      console.log('Setting up folder upload handler');
      
      // Handle folder selection
      $(document).on('change', '#global_folder_upload', function(e) {
        console.log('Folder upload change detected');
        var files = e.target.files;
        console.log('Number of files selected:', files.length);
        
        if (files.length > 0) {
          // Get file type filters from Shiny inputs
          var ignoreExtensions = [];
          var allowExtensions = [];
          
          // Read the filter inputs
          var ignoreInput = $('#folder_ignore_extensions').val();
          var allowInput = $('#folder_allow_extensions').val();
          
          if (ignoreInput && ignoreInput.trim() !== '') {
            ignoreExtensions = ignoreInput.toLowerCase().split(',').map(function(ext) {
              return ext.trim().replace(/^\\./, ''); // Remove leading dot if present
            });
          }
          
          if (allowInput && allowInput.trim() !== '') {
            allowExtensions = allowInput.toLowerCase().split(',').map(function(ext) {
              return ext.trim().replace(/^\\./, ''); // Remove leading dot if present
            });
          }
          
          console.log('Ignore extensions:', ignoreExtensions);
          console.log('Allow extensions:', allowExtensions);
          
          // Create a FormData object to mimic Shiny's file input structure
          var fileDataArray = [];
          var filesFiltered = 0;
          
          for (var i = 0; i < files.length; i++) {
            var file = files[i];
            
            // Extract folder structure from webkitRelativePath
            var relativePath = file.webkitRelativePath || file.name;
            var fileName = relativePath.split('/').pop(); // Get just the filename
            
            // Get file extension
            var fileExt = '';
            var lastDotIndex = fileName.lastIndexOf('.');
            if (lastDotIndex > 0 && lastDotIndex < fileName.length - 1) {
              fileExt = fileName.substring(lastDotIndex + 1).toLowerCase();
            }
            
            // Apply file type filters
            var shouldInclude = true;
            
            // Check if file extension should be ignored
            if (ignoreExtensions.length > 0 && ignoreExtensions.includes(fileExt)) {
              console.log('Ignoring file (extension blacklisted):', fileName, 'ext:', fileExt);
              shouldInclude = false;
              filesFiltered++;
            }
            
            // Check if file extension is in allow list (if allow list is specified)
            if (shouldInclude && allowExtensions.length > 0 && !allowExtensions.includes(fileExt)) {
              console.log('Ignoring file (not in allow list):', fileName, 'ext:', fileExt);
              shouldInclude = false;
              filesFiltered++;
            }
            
            if (!shouldInclude) {
              continue; // Skip this file
            }
            
            console.log('Processing file:', relativePath, 'ext:', fileExt);
            
            // Create new file name with folder structure
            var pathParts = relativePath.split('/');
            var folderName = pathParts[0]; // Root folder name
            
            // Create hierarchical name: folder-subfolder-filename
            var newName;
            if (pathParts.length > 2) {
              // Has subfolders
              var subfolders = pathParts.slice(1, -1).join('-');
              newName = folderName + '-' + subfolders + '-' + fileName;
            } else {
              // Direct file in root folder
              newName = folderName + '-' + fileName;
            }
            
            console.log('Original path:', relativePath, '-> New name:', newName);
            
            // Store file info for Shiny
            fileDataArray.push({
              name: newName,
              size: file.size,
              type: file.type,
              lastModified: file.lastModified,
              originalPath: relativePath,
              file: file,
              extension: fileExt
            });
          }
          
          // Send folder data to Shiny
          console.log('Sending folder data to Shiny:', fileDataArray.length, 'files (', filesFiltered, 'filtered out)');
          Shiny.setInputValue('global_folder_upload_data', {
            files: fileDataArray.map(function(f) {
              return {
                name: f.name,
                size: f.size,
                type: f.type,
                lastModified: f.lastModified,
                originalPath: f.originalPath,
                extension: f.extension
              };
            }),
            filesFiltered: filesFiltered,
            totalFiles: files.length,
            timestamp: Date.now()
          });
          
          // Create temporary URLs for the files and send them
          var filePromises = fileDataArray.map(function(fileData, index) {
            return new Promise(function(resolve) {
              var reader = new FileReader();
              reader.onload = function(e) {
                resolve({
                  index: index,
                  name: fileData.name,
                  dataURL: e.target.result,
                  originalPath: fileData.originalPath
                });
              };
              reader.readAsDataURL(fileData.file);
            });
          });
          
          Promise.all(filePromises).then(function(fileResults) {
            console.log('All files read, sending to Shiny');
            Shiny.setInputValue('global_folder_upload_files', {
              files: fileResults,
              timestamp: Date.now()
            });
          });
        }
      });
    });
    
    // Handle custom message for updating tab titles
    Shiny.addCustomMessageHandler('updateNavTabTitle', function(data) {
      console.log('=== TAB RENAME MESSAGE RECEIVED ===');
      console.log('Full data object:', data);
      
      var moduleId = data.moduleId;
      var newTitle = data.newTitle;
      var oldTitle = data.oldTitle;
      
      console.log('Module ID:', moduleId);
      console.log('New Title:', newTitle);
      console.log('Old Title:', oldTitle);
      
      // Check if we have a stored mapping for this module
      if (window.tabMappings[moduleId]) {
        console.log('Found stored mapping for module:', window.tabMappings[moduleId]);
        oldTitle = window.tabMappings[moduleId]; // Use the stored current title
      }
      
      // Log all current navigation elements for debugging
      console.log('Current navigation elements:');
      var allNavs = document.querySelectorAll('.nav-link, .navbar-nav a, .nav-item a, .navbar-nav .nav-link');
      allNavs.forEach(function(nav, index) {
        console.log(index + ':', {
          text: nav.textContent.trim(),
          href: nav.getAttribute('href'),
          dataValue: nav.getAttribute('data-value'),
          ariaControls: nav.getAttribute('aria-controls'),
          id: nav.getAttribute('id'),
          classes: nav.className
        });
      });
      
      var success = false;
      var updatedElement = null;
      
      // Strategy 1: Find by exact text match with old title
      if (oldTitle && !success) {
        console.log('Strategy 1: Looking for exact text match with:', oldTitle);
        var textMatches = Array.from(document.querySelectorAll('.nav-link, .navbar-nav a, .nav-item a')).filter(function(link) {
          return link.textContent.trim() === oldTitle.trim();
        });
        
        console.log('Found', textMatches.length, 'elements matching old title');
        
        if (textMatches.length > 0) {
          textMatches[0].textContent = newTitle;
          updatedElement = textMatches[0];
          success = true;
          console.log('SUCCESS: Updated via text match');
        }
      }
      
      // Strategy 2: Look for any element containing 'Import' and update the first one (for initial renames)
      if (!success && oldTitle && oldTitle.includes('Import')) {
        console.log('Strategy 2: Looking for elements containing Import');
        var importElements = Array.from(document.querySelectorAll('.nav-link, .navbar-nav a, .nav-item a')).filter(function(link) {
          return link.textContent.trim().includes('Import');
        });
        
        console.log('Found', importElements.length, 'elements containing Import');
        
        if (importElements.length > 0) {
          importElements[0].textContent = newTitle;
          updatedElement = importElements[0];
          success = true;
          console.log('SUCCESS: Updated via Import search');
        }
      }
      
      // Strategy 3: Look for the most recently updated tab (stored in our mapping)
      if (!success && window.tabMappings[moduleId]) {
        console.log('Strategy 3: Looking for previously mapped tab');
        var mappedTitle = window.tabMappings[moduleId];
        var mappedElements = Array.from(document.querySelectorAll('.nav-link, .navbar-nav a, .nav-item a')).filter(function(link) {
          return link.textContent.trim() === mappedTitle.trim();
        });
        
        console.log('Found', mappedElements.length, 'elements matching mapped title:', mappedTitle);
        
        if (mappedElements.length > 0) {
          mappedElements[0].textContent = newTitle;
          updatedElement = mappedElements[0];
          success = true;
          console.log('SUCCESS: Updated via mapping');
        }
      }
      
      // Strategy 4: Brute force - find any nav element and update it (last resort)
      if (!success) {
        console.log('Strategy 4: Brute force update of first available nav element');
        var allNavElements = document.querySelectorAll('.nav-link, .navbar-nav a, .nav-item a');
        
        // Look for elements that might be import tabs (avoid updating other tabs)
        var candidateElements = Array.from(allNavElements).filter(function(element) {
          var text = element.textContent.trim();
          return text.includes('Import') || text.includes('Data') || text === oldTitle;
        });
        
        if (candidateElements.length > 0) {
          candidateElements[0].textContent = newTitle;
          updatedElement = candidateElements[0];
          success = true;
          console.log('SUCCESS: Updated via brute force');
        }
      }
      
      // Store the mapping for future renames
      if (success && updatedElement) {
        window.tabMappings[moduleId] = newTitle;
        console.log('Stored mapping:', moduleId, '->', newTitle);
        
        // Also add a data attribute to the element for future identification
        updatedElement.setAttribute('data-module-id', moduleId);
      }
      
      if (success) {
        console.log('=== TAB RENAME SUCCESSFUL ===');
      } else {
        console.log('=== TAB RENAME FAILED ===');
      }
    });
    
    console.log('Tab renaming handler registered');
  "))
)

# --- Server Logic ---
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024^2) # 1GB
  # 2GB for future globals if needed
  # options(future.globals.maxSize = 2 * 1024^3)

  # --- Template Save/Load Logic ---
  getAceEditorInputs <- function(input_obj) {
    # More specific to our Ace IDs
    ace_inputs <- names(input_obj)[grepl("r_code", names(input_obj))]
    ace_values <- lapply(ace_inputs, function(id) input_obj[[id]])
    names(ace_values) <- ace_inputs
    return(ace_values)
  }

  output$download_template <- downloadHandler(
    filename = function() {
      req(input$template_file_name)
      paste0(tools::file_path_sans_ext(input$template_file_name), ".csv")
    },
    content = function(file) {
      tryCatch({
        all_inputs_list <- reactiveValuesToList(input)
        
        cat("=== TEMPLATE SAVE DEBUG ===\n")
        cat("Total inputs captured:", length(all_inputs_list), "\n")
        
        # Check for plotter inputs specifically
        plotter_inputs <- names(all_inputs_list)[grepl("plotter_[0-9]+-", names(all_inputs_list))]
        cat("Plotter inputs found during save:", length(plotter_inputs), "\n")
        if (length(plotter_inputs) > 0) {
          cat("Sample plotter inputs:", head(plotter_inputs, 5), "\n")
          # Show values of first few plotter inputs
          cat("Sample plotter input values:\n")
          for (i in 1:min(5, length(plotter_inputs))) {
            input_name <- plotter_inputs[i]
            input_val <- all_inputs_list[[input_name]]
            cat("  ", input_name, ":", class(input_val), "=", 
                if(is.null(input_val)) "NULL" else {
                  if(length(input_val) > 1) paste(input_val[1:min(3, length(input_val))], collapse=", ")
                  else substr(as.character(input_val), 1, 50)
                }, "\n")
          }
        }
        
        # Get current module counts for template metadata
        current_importer_count <- isolate(importer_counter())
        current_plotter_count <- isolate(plotter_counter())
        
        # Add metadata about the app state
        metadata <- list(
          "template_version" = "1.0",
          "created_date" = as.character(Sys.time()),
          "importer_count" = current_importer_count,
          "plotter_count" = current_plotter_count,
          "total_inputs" = length(all_inputs_list)
        )
        
        # Combine metadata and inputs
        all_data <- c(metadata, all_inputs_list)
        
        # Create data frame for CSV export with better error handling
        if (length(all_data) > 0) {
          inputs_df_list <- lapply(names(all_data), function(key) {
            tryCatch({
              val <- all_data[[key]]
              # Convert complex objects or NULLs to character for CSV
              if (is.null(val)) {
                val_str <- "NULL_VALUE"
                val_type <- "NULL"
              } else if (is.list(val) || length(val) > 1) {
                # Use a rare separator for lists
                val_str <- paste(unlist(val), collapse = "|||")
                val_type <- paste(class(val), collapse = ",")
              } else {
                val_str <- as.character(val)
                val_type <- class(val)[1]
              }
              
              data.frame(
                key = key, 
                value = val_str, 
                type = val_type,
                stringsAsFactors = FALSE
              )
            }, error = function(e) {
              # If there's an error processing this input, create a safe entry
              data.frame(
                key = key,
                value = "ERROR_IN_PROCESSING",
                type = "error",
                stringsAsFactors = FALSE
              )
            })
          })
          
          # Filter out any NULL results and bind
          valid_inputs <- Filter(function(x) !is.null(x) && nrow(x) > 0, inputs_df_list)
          
          if (length(valid_inputs) > 0) {
            inputs_df <- do.call(rbind, valid_inputs)
          } else {
            # Create minimal template if no valid inputs
            inputs_df <- data.frame(
              key = c("template_version", "created_date"),
              value = c("1.0", as.character(Sys.time())),
              type = c("character", "character"),
              stringsAsFactors = FALSE
            )
          }
        } else {
          # Create empty data frame with proper structure
          inputs_df <- data.frame(
            key = character(0), 
            value = character(0),
            type = character(0),
            stringsAsFactors = FALSE
          )
        }
        
        write.csv(inputs_df, file, row.names = FALSE)
        showNotification(paste("Template saved with", current_importer_count, "importers and", current_plotter_count, "plotters"), type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error saving template:", e$message), type = "error", duration = 10)
        # Create minimal file to prevent download failure
        minimal_df <- data.frame(
          key = "error",
          value = paste("Template save failed:", e$message),
          type = "error",
          stringsAsFactors = FALSE
        )
        write.csv(minimal_df, file, row.names = FALSE)
      })
    }
  )

  observeEvent(input$template_upload, {
    req(input$template_upload)
    tryCatch({
      template_df <- read.csv(
        input$template_upload$datapath, 
        stringsAsFactors = FALSE
      )
      
      # Validate template structure
      required_columns <- c("key", "value")
      if (!all(required_columns %in% names(template_df))) {
        showNotification(
          paste("Invalid template file. Missing columns:", 
                paste(required_columns[!required_columns %in% names(template_df)], collapse = ", ")),
          type = "error",
          duration = 10
        )
        return()
      }
      
      # Filter out any error entries from failed saves
      template_df <- template_df[template_df$value != "ERROR_IN_PROCESSING", ]
      
      if (nrow(template_df) == 0) {
        showNotification("Template file contains no valid data", type = "error")
        return()
      }
      
      # Extract metadata if available
      importer_count_needed <- 1  # Default
      plotter_count_needed <- 0   # Default
      
      if ("importer_count" %in% template_df$key) {
        importer_val <- template_df$value[template_df$key == "importer_count"]
        importer_count_needed <- as.numeric(importer_val[1])  # Take only first element
        if (is.na(importer_count_needed) || importer_count_needed < 1) {
          importer_count_needed <- 1  # Ensure at least 1
        }
      }
      if ("plotter_count" %in% template_df$key) {
        plotter_val <- template_df$value[template_df$key == "plotter_count"] 
        plotter_count_needed <- as.numeric(plotter_val[1])  # Take only first element
        if (is.na(plotter_count_needed) || plotter_count_needed < 0) {
          plotter_count_needed <- 0  # Ensure non-negative
        }
      }
      
      # Get current counts
      current_importer_count <- isolate(importer_counter())
      current_plotter_count <- isolate(plotter_counter())
      
      # Ensure single values for while loop conditions
      importer_count_needed <- as.integer(importer_count_needed)
      plotter_count_needed <- as.integer(plotter_count_needed)
      current_importer_count <- as.integer(current_importer_count)
      current_plotter_count <- as.integer(current_plotter_count)
      
      # Create additional importers if needed
      if (current_importer_count < importer_count_needed) {
        tabs_to_create <- importer_count_needed - current_importer_count
        for (i in 1:tabs_to_create) {
          current_count <- isolate(importer_counter()) + 1
          importer_counter(current_count)
          import_id <- paste0("data_import_module_", current_count)

          nav_insert(
            id = "mainmenu",
            target = "Combined Data",
            position = "before",
            nav = nav_panel(
              title = paste("Import", current_count),
              ui_data_importer(import_id)
            ),
            session = session
          )
          
          # Call the server module for the new importer
          importer_module_output <- server_data_import(import_id, global_files)
          
          # Store the module's output
          current_instances <- importer_instances()
          current_instances[[import_id]] <- importer_module_output
          importer_instances(current_instances)
        }
      }
      
      # Create additional plotters if needed
      if (current_plotter_count < plotter_count_needed) {
        tabs_to_create <- plotter_count_needed - current_plotter_count
        for (i in 1:tabs_to_create) {
          current_count <- isolate(plotter_counter()) + 1
          plotter_counter(current_count)
          plot_id <- paste0("plotter_", current_count)

          nav_insert(
            id = "mainmenu",
            target = "Analysis",
            position = "before",
            nav = nav_panel(
              title = plot_id,
              ui_plotter(plot_id)
            ),
            session = session
          )
          
          # Call the server module for the new plotter
          plotter_module_output <- server_plotter(plot_id, data_combiner$df, input)
          
          # Store the module's output
          current_instances <- plotter_instances()
          current_instances[[plot_id]] <- plotter_module_output
          plotter_instances(current_instances)
        }
      }
      
      # Wait a moment for tabs to be created
      Sys.sleep(1.0)
      
      # Wait additional time for plotter modules to initialize if any were created
      if (plotter_count_needed > current_plotter_count) {
        Sys.sleep(3.0)  # Extra time for plotter initialization (increased)
      }
      
      # Now restore the settings in phases
      
      # Phase 1: Restore Ace Editors (these might need modules to be ready)
      ace_keys <- template_df$key[grepl("r_code", template_df$key)]
      for (ace_key in ace_keys) {
        val_str <- template_df$value[template_df$key == ace_key]
        if (length(val_str) > 0 && !is.na(val_str[1]) && val_str[1] != "NULL_VALUE" && val_str[1] != "") {
          tryCatch({
            updateAceEditor(session, ace_key, value = as.character(val_str[1]))
          }, error = function(e) {
            warning("Failed to update ace editor '", ace_key, "': ", e$message)
          })
        }
      }
      
      # Short delay between ace editors and other inputs
      Sys.sleep(0.2)
      
      # Phase 2: Restore other inputs (excluding metadata and ace editors)
      metadata_keys <- c("template_version", "created_date", "importer_count", "plotter_count", "total_inputs")
      other_keys <- template_df$key[!grepl("r_code", template_df$key) & !template_df$key %in% metadata_keys]
      
      # Split other keys into plotter-specific and general inputs
      # Plotter inputs have format: "plotter_X-input_name"
      plotter_keys <- other_keys[grepl("plotter_[0-9]+-", other_keys)]
      general_keys <- other_keys[!grepl("plotter_[0-9]+-", other_keys)]
      
      cat("=== TEMPLATE LOADING DEBUG ===\n")
      cat("Total other keys:", length(other_keys), "\n")
      cat("Plotter keys found:", length(plotter_keys), "\n")
      cat("General keys found:", length(general_keys), "\n")
      if (length(plotter_keys) > 0) {
        cat("Sample plotter keys:", head(plotter_keys, 3), "\n")
      }
      
      # First apply general inputs
      for (key_val in general_keys) {
        val_str <- template_df$value[template_df$key == key_val]
        # Ensure we have a single value
        if (length(val_str) > 0) {
          val_str <- val_str[1]  # Take only the first element
        }
        
        if (!is.na(val_str) && val_str != "" && val_str != "ERROR_IN_PROCESSING" && val_str != "NULL_VALUE") {
            tryCatch({
              current_input_val <- isolate(input[[key_val]])
              # Attempt to convert back to original type if simple
              if (is.logical(current_input_val) && 
                        !is.na(as.logical(val_str))) {
                  updateCheckboxInput(session, key_val, value = as.logical(val_str))
                  cat("Updated checkbox:", key_val, "\n")
              } else if (is.numeric(current_input_val) && 
                        !is.na(as.numeric(val_str))) {
                  updateNumericInput(session, key_val, value = as.numeric(val_str))
                  cat("Updated numeric:", key_val, "\n")
              } else if (is.character(current_input_val) || 
                        is.null(current_input_val)) {
                  # For selectizeInput with multiple=TRUE, split by "|||"
                  if (grepl("|||", val_str, fixed = TRUE)) {
                      selected_values <- strsplit(val_str, "|||", fixed = TRUE)[[1]]
                      updateSelectizeInput(
                        session, key_val, 
                        selected = selected_values
                      )
                      cat("Updated selectize (multiple):", key_val, "\n")
                  } else {
                      # Check if this is a selectInput by trying updateSelectInput first
                      tryCatch({
                        updateSelectInput(session, key_val, selected = as.character(val_str))
                        cat("Updated select:", key_val, "\n")
                      }, error = function(e2) {
                        # Fallback to text input
                        updateTextInput(session, key_val, value = as.character(val_str))
                        cat("Updated text (fallback):", key_val, "\n")
                      })
                  }
              } else {
                  # Try generic update for other input types
                  tryCatch({
                    updateTextInput(session, key_val, value = as.character(val_str))
                    cat("Updated as text (generic):", key_val, "\n")
                  }, error = function(e2) {
                    cat("Could not update input type for:", key_val, "\n")
                  })
              }
            }, error = function(e) {
              warning("Failed to update general input '", key_val, "': ", e$message)
            })
        }
      }
      
      # Extra delay before plotter-specific inputs
      if (length(plotter_keys) > 0) {
        Sys.sleep(1.0)  # Increased delay for plotter inputs
      }
      
      # Then apply plotter-specific inputs with additional error handling
      for (key_val in plotter_keys) {
        val_str <- template_df$value[template_df$key == key_val]
        # Ensure we have a single value
        if (length(val_str) > 0) {
          val_str <- val_str[1]  # Take only the first element
        }
        
        if (!is.na(val_str) && val_str != "" && val_str != "ERROR_IN_PROCESSING" && val_str != "NULL_VALUE") {
            cat("Attempting to update plotter input:", key_val, "with value:", substr(val_str, 1, 50), "\n")
            tryCatch({
              current_input_val <- isolate(input[[key_val]])
              cat("Current input value for", key_val, ":", class(current_input_val), 
                  if(!is.null(current_input_val)) substr(as.character(current_input_val), 1, 30) else "NULL", "\n")
              
              # Attempt to convert back to original type if simple
              if (is.logical(current_input_val) && 
                        !is.na(as.logical(val_str))) {
                  updateCheckboxInput(session, key_val, value = as.logical(val_str))
                  cat("Updated checkbox:", key_val, "\n")
              } else if (inherits(current_input_val, "Date")) {
                  # Handle Date inputs
                  tryCatch({
                    date_val <- as.Date(val_str)
                    updateDateInput(session, key_val, value = date_val)
                    cat("Updated date:", key_val, "\n")
                  }, error = function(e) {
                    cat("Failed to update date input", key_val, ":", e$message, "\n")
                  })
              } else if (inherits(current_input_val, c("POSIXlt", "POSIXt"))) {
                  # Handle datetime inputs (these are complex selectizeInputs)
                  tryCatch({
                    if (grepl("|||", val_str, fixed = TRUE)) {
                      selected_values <- strsplit(val_str, "|||", fixed = TRUE)[[1]]
                      updateSelectizeInput(session, key_val, selected = selected_values)
                      cat("Updated datetime selectize:", key_val, "\n")
                    }
                  }, error = function(e) {
                    cat("Failed to update datetime input", key_val, ":", e$message, "\n")
                  })
              } else if (is.numeric(current_input_val) && 
                        !is.na(as.numeric(val_str))) {
                  updateNumericInput(session, key_val, value = as.numeric(val_str))
                  cat("Updated numeric:", key_val, "\n")
              } else if (is.character(current_input_val) || 
                        is.null(current_input_val)) {
                  # For selectizeInput with multiple=TRUE, split by "|||"
                  if (grepl("|||", val_str, fixed = TRUE)) {
                      selected_values <- strsplit(val_str, "|||", fixed = TRUE)[[1]]
                      updateSelectizeInput(
                        session, key_val, 
                        selected = selected_values
                      )
                      cat("Updated selectize (multiple):", key_val, "\n")
                  } else {
                      # Check if this is a selectInput by trying updateSelectInput first
                      tryCatch({
                        updateSelectInput(session, key_val, selected = as.character(val_str))
                        cat("Updated select:", key_val, "\n")
                      }, error = function(e2) {
                        # Fallback to text input
                        updateTextInput(session, key_val, value = as.character(val_str))
                        cat("Updated text (fallback):", key_val, "\n")
                      })
                  }
              } else {
                  # Try generic update for other input types
                  tryCatch({
                    updateTextInput(session, key_val, value = as.character(val_str))
                    cat("Updated as text (generic):", key_val, "\n")
                  }, error = function(e2) {
                    cat("Could not update input type for:", key_val, "\n")
                  })
              }
            }, error = function(e) {
              cat("Failed to update plotter input '", key_val, "': ", e$message, ". Plotter may not be ready yet.\n")
            })
        }
      }
      
      showNotification(paste("Template loaded successfully:", importer_count_needed, "importers and", plotter_count_needed, "plotters created"), type = "message")
      
      # If we have plotter inputs, schedule a delayed update to ensure modules are fully ready
      if (length(plotter_keys) > 0) {
        # Store plotter inputs for delayed application
        plotter_inputs_to_apply <- list()
        for (key_val in plotter_keys) {
          val_str <- template_df$value[template_df$key == key_val]
          if (length(val_str) > 0) {
            val_str <- val_str[1]
          }
          if (!is.na(val_str) && val_str != "" && val_str != "ERROR_IN_PROCESSING" && val_str != "NULL_VALUE") {
            plotter_inputs_to_apply[[key_val]] <- val_str
          }
        }
        
        # Schedule delayed application of plotter inputs
        if (length(plotter_inputs_to_apply) > 0) {
          # Create a reactive value to control timing
          plotter_retry_count <- reactiveVal(0)
          plotter_successful_updates <- reactiveVal(0)
          
          # Set up a multi-retry delayed observer
          observe({
            current_count <- plotter_retry_count()
            
            if (current_count == 0) {
              # First attempt - wait 5 seconds for UI to fully render
              invalidateLater(5000)
              plotter_retry_count(1)
            } else if (current_count <= 3) {  # Try up to 3 times
              # Apply plotter inputs after delay
              cat("=== DELAYED PLOTTER INPUT APPLICATION (Attempt", current_count, ") ===\n")
              cat("Attempting delayed update of", length(plotter_inputs_to_apply), "plotter inputs\n")
              
              successful_count <- 0
              
              for (key_val in names(plotter_inputs_to_apply)) {
                val_str <- plotter_inputs_to_apply[[key_val]]
                cat("Delayed update attempt for:", key_val, "\n")
                cat("  Template value to apply:", if(nchar(val_str) > 100) paste0(substr(val_str, 1, 100), "...") else val_str, "\n")
                tryCatch({
                  current_input_val <- isolate(input[[key_val]])
                  cat("Input", key_val, "exists:", !is.null(current_input_val), 
                      "Type:", if(!is.null(current_input_val)) class(current_input_val) else "NULL", "\n")
                  cat("  Current UI value:", if(!is.null(current_input_val)) {
                    if(length(current_input_val) > 1) paste(current_input_val[1:min(3, length(current_input_val))], collapse=", ") 
                    else substr(as.character(current_input_val), 1, 50)
                  } else "NULL", "\n")
                  
                  if (!is.null(current_input_val)) {  # Check if input exists now
                    if (is.logical(current_input_val) && !is.na(as.logical(val_str))) {
                      updateCheckboxInput(session, key_val, value = as.logical(val_str))
                      cat("Delayed updated checkbox:", key_val, "to value:", as.logical(val_str), "\n")
                      # Check if update worked
                      Sys.sleep(0.1)  # Small delay to let update apply
                      new_val <- isolate(input[[key_val]])
                      cat("  Checkbox value after update:", new_val, "\n")
                      successful_count <- successful_count + 1
                    } else if (inherits(current_input_val, "Date")) {
                      # Handle Date inputs
                      tryCatch({
                        date_val <- as.Date(val_str)
                        updateDateInput(session, key_val, value = date_val)
                        cat("Delayed updated date:", key_val, "\n")
                        successful_count <- successful_count + 1
                      }, error = function(e) {
                        cat("Failed to update date input", key_val, ":", e$message, "\n")
                      })
                    } else if (inherits(current_input_val, c("POSIXlt", "POSIXt"))) {
                      # Handle datetime inputs (these are complex selectizeInputs)
                      tryCatch({
                        if (grepl("|||", val_str, fixed = TRUE)) {
                          selected_values <- strsplit(val_str, "|||", fixed = TRUE)[[1]]
                          updateSelectizeInput(session, key_val, selected = selected_values)
                          cat("Delayed updated datetime selectize:", key_val, "\n")
                          successful_count <- successful_count + 1
                        }
                      }, error = function(e) {
                        cat("Failed to update datetime input", key_val, ":", e$message, "\n")
                      })
                    } else if (is.numeric(current_input_val) && !is.na(as.numeric(val_str))) {
                                              updateNumericInput(session, key_val, value = as.numeric(val_str))
                        cat("Delayed updated numeric:", key_val, "to value:", as.numeric(val_str), "\n")
                        # Check if update worked
                        Sys.sleep(0.1)  # Small delay to let update apply
                        new_val <- isolate(input[[key_val]])
                        cat("  Numeric value after update:", new_val, "\n")
                        successful_count <- successful_count + 1
                    } else if (is.character(current_input_val) || is.null(current_input_val)) {
                      if (grepl("|||", val_str, fixed = TRUE)) {
                        selected_values <- strsplit(val_str, "|||", fixed = TRUE)[[1]]
                        updateSelectizeInput(session, key_val, selected = selected_values)
                        cat("Delayed updated selectize:", key_val, "\n")
                        successful_count <- successful_count + 1
                      } else {
                        tryCatch({
                          updateSelectInput(session, key_val, selected = as.character(val_str))
                          cat("Delayed updated select:", key_val, "to value:", as.character(val_str), "\n")
                          # Check if update worked
                          Sys.sleep(0.1)  # Small delay to let update apply
                          new_val <- isolate(input[[key_val]])
                          cat("  Select value after update:", new_val, "\n")
                          successful_count <- successful_count + 1
                        }, error = function(e2) {
                          updateTextInput(session, key_val, value = as.character(val_str))
                          cat("Delayed updated text:", key_val, "\n")
                          successful_count <- successful_count + 1
                        })
                      }
                    }
                  }
                }, error = function(e) {
                  cat("Delayed update failed for", key_val, ":", e$message, "\n")
                })
              }
              
              plotter_successful_updates(successful_count)
              cat("Successfully updated", successful_count, "out of", length(plotter_inputs_to_apply), "plotter inputs\n")
              
              # If we still have failures and haven't reached max attempts, try again
              if (successful_count < length(plotter_inputs_to_apply) && current_count < 3) {
                cat("Will retry in 3 seconds (attempt", current_count + 1, ")\n")
                invalidateLater(3000)
                plotter_retry_count(current_count + 1)
              } else {
                # Done trying
                if (successful_count > 0) {
                  showNotification(paste("Plotter inputs applied:", successful_count, "successful,", 
                                         length(plotter_inputs_to_apply) - successful_count, "failed"), 
                                   type = if(successful_count == length(plotter_inputs_to_apply)) "message" else "warning", 
                                   duration = 5)
                } else {
                  showNotification("Failed to apply plotter inputs - modules may not be fully loaded", 
                                   type = "error", duration = 10)
                }
                plotter_retry_count(99)  # Stop further updates
              }
            }
          })
        }
      }
    }, error = function(e) {
      showNotification(
        paste("Failed to load template:", e$message), 
        type = "error", 
        duration = 10
      )
    })
  })

  # --- Global File Management for Input Data Tab ---
  global_files <- reactiveVal(list())
  
  # File upload handler for individual files
  observeEvent(input$global_file_upload, {
    req(input$global_file_upload)
    
    current_files <- global_files()
    new_files <- input$global_file_upload
    
    for (i in 1:nrow(new_files)) {
      file_name <- new_files$name[i]
      file_path <- new_files$datapath[i]
      
      # Check if file already exists
      if (file_name %in% names(current_files) && !input$overwrite_files) {
        showNotification(
          paste("File", file_name, "already exists. Enable overwrite to replace it."),
          type = "warning"
        )
        next
      }
      
      # Store file info
      current_files[[file_name]] <- list(
        name = file_name,
        path = file_path,
        size = file.info(file_path)$size,
        uploaded = Sys.time(),
        type = tools::file_ext(file_name),
        upload_type = "individual"
      )
      
      # Copy file to a permanent location (optional, for persistence)
      # For now, we'll work with the temporary files
    }
    
    global_files(current_files)
    showNotification(paste("Uploaded", nrow(new_files), "individual file(s)"), type = "message")
  })
  
  # Folder upload handler - receives file data from JavaScript
  observeEvent(input$global_folder_upload_files, {
    req(input$global_folder_upload_files)
    
    cat("=== FOLDER UPLOAD RECEIVED ===\n")
    folder_data <- input$global_folder_upload_files
    cat("Number of files in folder:", length(folder_data$files), "\n")
    
    # Get filtering information from JavaScript
    total_files <- if (!is.null(folder_data$totalFiles)) folder_data$totalFiles else length(folder_data$files)
    files_filtered <- if (!is.null(folder_data$filesFiltered)) folder_data$filesFiltered else 0
    cat("Total files found:", total_files, ", Files filtered out:", files_filtered, "\n")
    
    current_files <- global_files()
    files_processed <- 0
    files_skipped <- 0
    
    # Process each file from the folder
    for (file_info in folder_data$files) {
      file_name <- file_info$name
      data_url <- file_info$dataURL
      original_path <- file_info$originalPath
      
      cat("Processing folder file:", file_name, "(original:", original_path, ")\n")
      
      # Check if file already exists
      if (file_name %in% names(current_files) && !input$overwrite_files) {
        cat("File already exists, skipping:", file_name, "\n")
        files_skipped <- files_skipped + 1
        next
      }
      
      # Decode the data URL and save to temp file
      tryCatch({
        # Remove data URL prefix
        data_part <- sub("^data:[^,]*,", "", data_url)
        
        # Decode base64
        file_content <- base64enc::base64decode(data_part)
        
        # Create temporary file
        temp_file <- tempfile(fileext = paste0(".", tools::file_ext(file_name)))
        writeBin(file_content, temp_file)
        
        # Store file info
        current_files[[file_name]] <- list(
          name = file_name,
          path = temp_file,
          size = length(file_content),
          uploaded = Sys.time(),
          type = tools::file_ext(file_name),
          upload_type = "folder",
          original_path = original_path
        )
        
        files_processed <- files_processed + 1
        cat("Successfully processed:", file_name, "\n")
        
      }, error = function(e) {
        cat("Error processing file", file_name, ":", e$message, "\n")
        showNotification(
          paste("Error processing", file_name, ":", e$message),
          type = "error"
        )
      })
    }
    
    global_files(current_files)
    
    # Show summary notification
    summary_msg <- paste("Folder upload complete:", files_processed, "files processed")
    if (files_skipped > 0) {
      summary_msg <- paste0(summary_msg, ", ", files_skipped, " files skipped (already exist)")
    }
    if (files_filtered > 0) {
      summary_msg <- paste0(summary_msg, ", ", files_filtered, " files filtered out by extension")
    }
    summary_msg <- paste0(summary_msg, " (", total_files, " total files found)")
    
    showNotification(summary_msg, type = "message", duration = 5)
    cat("Folder upload complete:", files_processed, "processed,", files_skipped, "skipped,", files_filtered, "filtered\n")
  })
  
  # Remove selected files
  observeEvent(input$remove_selected_files, {
    req(input$global_file_list_rows_selected)
    
    current_files <- global_files()
    selected_indices <- input$global_file_list_rows_selected
    file_names <- names(current_files)
    
    if (length(selected_indices) > 0 && length(file_names) >= max(selected_indices)) {
      files_to_remove <- file_names[selected_indices]
      for (file_name in files_to_remove) {
        current_files[[file_name]] <- NULL
      }
      global_files(current_files)
      showNotification(paste("Removed", length(files_to_remove), "file(s)"), type = "message")
    }
  })
  
  # Clear all files
  observeEvent(input$clear_all_files, {
    global_files(list())
    showNotification("All files cleared", type = "message")
  })
  
  # File list display
  output$global_file_list <- DT::renderDT({
    current_files <- global_files()
    
    if (length(current_files) == 0) {
      data.frame(
        Name = character(0),
        Type = character(0),
        Size = character(0),
        Source = character(0),
        Original_Path = character(0),
        Uploaded = character(0)
      )
    } else {
      file_df <- data.frame(
        Name = names(current_files),
        Type = sapply(current_files, function(x) x$type),
        Size = sapply(current_files, function(x) {
          size_mb <- round(x$size / 1024^2, 2)
          paste(size_mb, "MB")
        }),
        Source = sapply(current_files, function(x) {
          if (!is.null(x$upload_type)) {
            if (x$upload_type == "folder") "📁 Folder" else "📄 Individual"
          } else {
            "📄 Individual"  # Default for existing files
          }
        }),
        Original_Path = sapply(current_files, function(x) {
          if (!is.null(x$original_path)) {
            x$original_path
          } else {
            x$name  # Fallback to file name
          }
        }),
        Uploaded = sapply(current_files, function(x) {
          format(x$uploaded, "%Y-%m-%d %H:%M:%S")
        }),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(
        file_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          columnDefs = list(
            list(width = '200px', targets = 0),  # Name column
            list(width = '300px', targets = 4)   # Original_Path column
          )
        ),
        selection = "multiple",
        rownames = FALSE
      )
    }
  })
  
  # File statistics
  output$file_stats <- renderText({
    current_files <- global_files()
    
    if (length(current_files) == 0) {
      "No files uploaded"
    } else {
      total_size <- sum(sapply(current_files, function(x) x$size))
      total_size_mb <- round(total_size / 1024^2, 2)
      
      file_types <- table(sapply(current_files, function(x) x$type))
      type_summary <- paste(names(file_types), ":", file_types, collapse = ", ")
      
      paste(
        "Total files:", length(current_files), "\n",
        "Total size:", total_size_mb, "MB\n",
        "File types:", type_summary
      )
    }
  })

  # --- Dynamic Plotter Management ---
  plotter_instances <- reactiveVal(list())
  plotter_counter <- reactiveVal(0)

  # --- Dynamic Data Import Management ---
  importer_instances <- reactiveVal(list())
  importer_counter <- reactiveVal(0)
  
  # Store custom tab names for importers
  importer_tab_names <- reactiveVal(list())
  # Also store current displayed titles to help with repeated renames
  importer_current_titles <- reactiveVal(list())

  # --- Data Import and Combination Logic ---
  # Create reactive list of data frames from all active importers
  list_of_df_reactives_for_combiner <- reactive({
    current_importers <- importer_instances()
    if (length(current_importers) == 0) {
      return(list())
    }
    
    # Extract the 'df' reactive from each importer instance
    df_reactives <- lapply(current_importers, function(instance) {
      if (!is.null(instance) && "df" %in% names(instance)) {
        return(instance$df)
      }
      return(NULL)
    })
    
    # Filter out any NULL values
    Filter(Negate(is.null), df_reactives)
  })
  
  data_combiner <- server_data_combiner(
    "combiner", 
    list_of_df_reactives_for_combiner
  )

  # --- Dynamic Data Import Creation ---
  observeEvent(input$insert_importer, {
    current_count <- isolate(importer_counter()) + 1
    importer_counter(current_count)
    import_id <- paste0("data_import_module_", current_count)

    # Check if there's a custom name for this tab
    current_names <- importer_tab_names()
    tab_title <- if (!is.null(current_names[[import_id]])) {
      current_names[[import_id]]
    } else {
      paste("Import", current_count)
    }

    nav_insert(
      id = "mainmenu",
      target = "Combined Data",
      position = "before",
      nav = nav_panel(
        title = tab_title,
        ui_data_importer(import_id)
      ),
      session = session
    )
    
    # Call the server module for the new importer, passing global files
    importer_module_output <- server_data_import(import_id, global_files)
    
    # Store the module's output
    current_instances <- importer_instances()
    current_instances[[import_id]] <- importer_module_output
    importer_instances(current_instances)
    
    # Observe the tab rename trigger for this specific importer
    observeEvent(importer_module_output$tab_rename_trigger(), {
      rename_data <- importer_module_output$tab_rename_trigger()
      
      if (!is.null(rename_data)) {
        cat("=== MAIN SERVER RECEIVED TAB RENAME ===\n")
        cat("Module ID:", rename_data$moduleId, "\n")
        cat("New Name:", rename_data$newName, "\n")
        cat("Current Title:", rename_data$currentTitle, "\n")
        
        # Get the actual current title (might be different from the default)
        current_titles <- importer_current_titles()
        actual_old_title <- if (!is.null(current_titles[[rename_data$moduleId]])) {
          current_titles[[rename_data$moduleId]]
        } else {
          rename_data$currentTitle # Fall back to the default
        }
        
        cat("Actual old title:", actual_old_title, "\n")
        
        # Update stored tab names and current titles
        current_names <- importer_tab_names()
        current_names[[rename_data$moduleId]] <- rename_data$newName
        importer_tab_names(current_names)
        
        current_titles[[rename_data$moduleId]] <- rename_data$newName
        importer_current_titles(current_titles)
        
        # Send JavaScript message to update the tab
        session$sendCustomMessage("updateNavTabTitle", list(
          moduleId = rename_data$moduleId,
          newTitle = rename_data$newName,
          oldTitle = actual_old_title
        ))
        
        cat("Sent JavaScript message for tab rename\n")
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    showNotification(
      paste("Added data import tab:", current_count), 
      type = "message"
    )
  })

  # Initialize with one default data import tab
  observeEvent(session$clientData, {
    if (isolate(importer_counter()) == 0) {
      # Trigger the insert_importer logic programmatically
      current_count <- 1
      importer_counter(current_count)
      import_id <- paste0("data_import_module_", current_count)

      # Check for custom name
      current_names <- importer_tab_names()
      tab_title <- if (!is.null(current_names[[import_id]])) {
        current_names[[import_id]]
      } else {
        paste("Import", current_count)
      }

      nav_insert(
        id = "mainmenu",
        target = "Combined Data",
        position = "before",
        nav = nav_panel(
          title = tab_title,
          ui_data_importer(import_id)
        ),
        session = session
      )
      
      # Call the server module for the initial importer, passing global files
      importer_module_output <- server_data_import(import_id, global_files)
      
      # Store the module's output
      current_instances <- list()
      current_instances[[import_id]] <- importer_module_output
      importer_instances(current_instances)
      
      # Observe the tab rename trigger for the initial importer
      observeEvent(importer_module_output$tab_rename_trigger(), {
        rename_data <- importer_module_output$tab_rename_trigger()
        
        if (!is.null(rename_data)) {
          cat("=== MAIN SERVER RECEIVED INITIAL TAB RENAME ===\n")
          cat("Module ID:", rename_data$moduleId, "\n")
          cat("New Name:", rename_data$newName, "\n")
          cat("Current Title:", rename_data$currentTitle, "\n")
          
          # Get the actual current title (might be different from the default)
          current_titles <- importer_current_titles()
          actual_old_title <- if (!is.null(current_titles[[rename_data$moduleId]])) {
            current_titles[[rename_data$moduleId]]
          } else {
            rename_data$currentTitle # Fall back to the default
          }
          
          cat("Actual old title:", actual_old_title, "\n")
          
          # Update stored tab names and current titles
          current_names <- importer_tab_names()
          current_names[[rename_data$moduleId]] <- rename_data$newName
          importer_tab_names(current_names)
          
          current_titles[[rename_data$moduleId]] <- rename_data$newName
          importer_current_titles(current_titles)
          
          # Send JavaScript message to update the tab
          session$sendCustomMessage("updateNavTabTitle", list(
            moduleId = rename_data$moduleId,
            newTitle = rename_data$newName,
            oldTitle = actual_old_title
          ))
          
          cat("Sent JavaScript message for initial tab rename\n")
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    }
  }, once = TRUE)

  # --- Dynamic Plotter Creation ---
  observeEvent(input$insert_plot, {
    current_count <- isolate(plotter_counter()) + 1
    plotter_counter(current_count)
    plot_id <- paste0("plotter_", current_count)

    nav_insert( # Use the ID of the navset directly
      id = "mainmenu", # Target the main navbar ID
      target = "Analysis", # Insert before the "Analysis" tab
      position = "before",
      nav = nav_panel( # Use nav_panel directly
        title = plot_id,
        ui_plotter(plot_id) # Pass the unique ID to the UI module
      ),
      session = session # Pass session for nav_insert
    )
    
    # Call the server module for the new plotter
    # Pass combined data and main session input
    plotter_module_output <- server_plotter(plot_id, data_combiner$df, input)
    
    # Store the module's output (which includes the plot reactive and rename trigger)
    current_instances <- plotter_instances()
    current_instances[[plot_id]] <- plotter_module_output
    plotter_instances(current_instances)
    
    # Observe the plot rename trigger for this specific plotter
    observeEvent(plotter_module_output$plot_rename_trigger(), {
      rename_data <- plotter_module_output$plot_rename_trigger()
      
      if (!is.null(rename_data)) {
        cat("=== MAIN SERVER RECEIVED PLOT RENAME ===\n")
        cat("Module ID:", rename_data$moduleId, "\n")
        cat("New Name:", rename_data$newName, "\n")
        cat("Current Title:", rename_data$currentTitle, "\n")
        
        # Send JavaScript message to update the tab
        session$sendCustomMessage("updateNavTabTitle", list(
          moduleId = rename_data$moduleId,
          newTitle = rename_data$newName,
          oldTitle = rename_data$currentTitle
        ))
        
        cat("Sent JavaScript message for plot rename\n")
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    showNotification(paste("Added plotter tab:", plot_id), type = "message")
  })

  # --- Helper & Downloader R Code Execution ---
  ace_server_functions("helper_input") # Enable tooltips/autocomplete
  observeEvent(input$helper_input, {
    output$helper_output <- renderUI({
      spsComps::shinyCatch({ # Error catching for user code
        # Evaluate in a new environment
        eval(
          parse(text = input$helper_input), 
          envir = new.env(parent = globalenv())
        )
      })
    })
  }, ignoreInit = FALSE, ignoreNULL = FALSE) # Trigger on load

  ace_server_functions("downloader_input")
  observeEvent(input$downloader_input, {
    output$downloader_output <- renderUI({
      spsComps::shinyCatch({
        # Prepare the list of plot reactives for the downloader code
        plots_to_download_map <- lapply(plotter_instances(), function(instance) {
            if (!is.null(instance) && "plot" %in% names(instance) && 
                is.function(instance$plot)) {
                return(instance$plot) # Return the reactive function itself
            }
            return(NULL)
        })
        plots_to_download_map <- Filter(Negate(is.null), plots_to_download_map)

        # Environment for downloader code
        downloader_env <- new.env(parent = globalenv())
        downloader_env$dynamic_plots_map <- plots_to_download_map
        downloader_env$main_shiny_input <- input
        downloader_env$main_shiny_output <- output
        downloader_env$main_shiny_session <- session
        downloader_env$showNotification <- showNotification
        downloader_env$req <- req
        downloader_env$downloadHandler <- downloadHandler
        downloader_env$downloadButton <- downloadButton
        downloader_env$datatable <- datatable
        downloader_env$actionButton <- actionButton
        downloader_env$selectInput <- selectInput
        downloader_env$numericInput <- numericInput
        downloader_env$div <- div
        downloader_env$h4 <- h4
        downloader_env$h5 <- h5
        downloader_env$p <- p
        downloader_env$tags <- tags
        downloader_env$icon <- icon

        eval(parse(text = input$downloader_input), envir = downloader_env)
      })
    })
  }, ignoreInit = FALSE, ignoreNULL = FALSE) # Trigger on load
  
  # Batch Download Server Logic
  observeEvent(input$select_all_plots, {
    # JavaScript to select all checkboxes
    session$sendCustomMessage("selectAllPlots", list(select = TRUE))
  })
  
  observeEvent(input$deselect_all_plots, {
    # JavaScript to deselect all checkboxes
    session$sendCustomMessage("selectAllPlots", list(select = FALSE))
  })
  
  # Batch Download Handler
  output$batch_download_zip <- downloadHandler(
    filename = function() {
      paste0('Selected_Plots_', Sys.Date(), '.zip')
    },
    content = function(file) {
      library(zip)
      library(htmlwidgets)
      
      # Create temporary directory
      temp_dir <- tempfile(pattern = 'plot_exports_')
      dir.create(temp_dir)
      on.exit(unlink(temp_dir, recursive = TRUE))
      
      # Get selected plots
      plots_to_download_map <- lapply(plotter_instances(), function(instance) {
        if (!is.null(instance) && "plot" %in% names(instance) && 
            is.function(instance$plot)) {
          return(instance$plot)
        }
        return(NULL)
      })
      plots_to_download_map <- Filter(Negate(is.null), plots_to_download_map)
      
             if (length(plots_to_download_map) == 0) {
         showNotification('No plots available to download.', type = 'message')
         file.create(file)
         return()
       }
      
             # Get download options
       download_dpi <- if(!is.null(input$download_dpi)) input$download_dpi else 300
       download_width <- if(!is.null(input$download_width)) input$download_width else 10
       download_height <- if(!is.null(input$download_height)) input$download_height else 7
       
       files_to_zip <- c()
       
       # Process each selected plot
       for (plot_id in names(plots_to_download_map)) {
         # Check if this plot is selected (via checkbox)
         checkbox_id <- paste0('select_plot_', plot_id)
         if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
           
           plot_reactive_fn <- plots_to_download_map[[plot_id]]
           
           tryCatch({
             # The plot reactive returns another reactive, so we need to call it twice
             inner_reactive <- plot_reactive_fn()
             plot_obj <- if (is.function(inner_reactive)) inner_reactive() else inner_reactive
             
             if (!is.null(plot_obj)) {
               # Clean filename
               safe_name <- gsub('[^a-zA-Z0-9_.-]', '_', plot_id)
               
               if (inherits(plot_obj, 'htmlwidget') || inherits(plot_obj, 'plotly')) {
                 # Interactive plot - save as HTML
                 file_path <- file.path(temp_dir, paste0(safe_name, '.html'))
                 htmlwidgets::saveWidget(plot_obj, file_path, selfcontained = TRUE)
                 files_to_zip <- c(files_to_zip, file_path)
                 
               } else if (inherits(plot_obj, 'ggplot')) {
                 # Static ggplot - save as PNG
                 file_path <- file.path(temp_dir, paste0(safe_name, '.png'))
                 ggsave(filename = file_path, plot = plot_obj, 
                        device = 'png', width = download_width, height = download_height, 
                        dpi = download_dpi)
                 files_to_zip <- c(files_to_zip, file_path)
                 
               } else if (is.list(plot_obj) && !is.data.frame(plot_obj)) {
                 # List output (like from text template) - save as formatted text
                 file_path <- file.path(temp_dir, paste0(safe_name, '.txt'))
                 
                 # Format list nicely
                 formatted_output <- capture.output({
                   if (is.list(plot_obj) && length(names(plot_obj)) > 0) {
                     # Named list - format with headers
                     for (name in names(plot_obj)) {
                       cat("=== ", name, " ===\n")
                       if (is.data.frame(plot_obj[[name]])) {
                         print(plot_obj[[name]])
                       } else {
                         cat(paste(plot_obj[[name]], collapse = "\n"), "\n")
                       }
                       cat("\n")
                     }
                   } else {
                     # Regular list or other object
                     print(plot_obj)
                   }
                 })
                 
                 writeLines(formatted_output, file_path)
                 files_to_zip <- c(files_to_zip, file_path)
                 
               } else if (is.data.frame(plot_obj)) {
                 # Data frame - save as CSV
                 file_path <- file.path(temp_dir, paste0(safe_name, '.csv'))
                 write.csv(plot_obj, file_path, row.names = FALSE)
                 files_to_zip <- c(files_to_zip, file_path)
                 
               } else {
                 # Other output types - try to convert to text
                 file_path <- file.path(temp_dir, paste0(safe_name, '.txt'))
                 
                 # Safely convert to character
                 text_output <- tryCatch({
                   if (is.function(plot_obj)) {
                     "Function object - cannot display content"
                   } else {
                     capture.output(print(plot_obj))
                   }
                 }, error = function(e) {
                   paste("Error converting object to text:", e$message)
                 })
                 
                 writeLines(text_output, file_path)
                 files_to_zip <- c(files_to_zip, file_path)
               }
                            } else {
                 showNotification(paste('Plot', plot_id, 'returned NULL - skipping'), 
                                type = 'message')
               }
             }, error = function(e) {
               showNotification(paste('Error processing plot', plot_id, ':', e$message), 
                              type = 'error')
             })
         }
       }
      
      if (length(files_to_zip) > 0) {
        # Create zip file
        zip::zip(zipfile = file, files = basename(files_to_zip), 
                root = temp_dir, compression_level = 9)
                 showNotification(paste('Successfully zipped', length(files_to_zip), 'files.'), 
                         type = 'message')
       } else {
         showNotification('No plots were selected or generated.', type = 'message')
        file.create(file)
      }
    },
    contentType = 'application/zip'
  )
  
  # Placeholder for Typst script if it becomes functional
  # output$typst_script_placeholder <- renderUI({
  #   tags$script(
  #     type = "module",
  #     HTML("
  #     // import { $typst } from './js/snippet.mjs'; 
  #     // Assuming snippet.mjs is in www/js/
  #     // console.log('Typst module loaded placeholder');
  #   ")
  #   )
  # })

}

# --- Shiny App Definition ---
shinyApp(ui = ui, server = server)
