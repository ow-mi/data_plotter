ui_plotter <- function(id) {
  ns <- NS(id) # Ensure ns is defined for the main module ID
  div(class = "module-container", style = "position: relative;",
    # Floating action buttons container positioned just below the navbar
    div(
      style = "position: absolute; top: calc(var(--navbar-height, 56px) + 6px); left: 50%; transform: translateX(-50%); z-index: 1000; display: flex; gap: 8px; align-items: center;",
      actionButton(
        ns("data_render"), 
        "Process Data",
        icon = icon("play", lib = "font-awesome"), 
        class = "btn-primary btn-lg",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.2);"
      ) |> tooltip("Process the input data for this plot"),
      actionButton(
        ns("plot_render"), 
        "Create Plot",
        icon = icon("chart-line", lib = "font-awesome"), 
        class = "btn-success btn-lg",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.2);"
      ) |> tooltip("Generate the plot using current settings")
    ),

    navset_card_pill(
        id = ns("plotter_tabs"),
        # full_screen = TRUE, # Removed - using dynamic height instead
        height = "100%",
    
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
                r_code_on_df = if (exists("r_code_combined_data_summary")) r_code_combined_data_summary else "# Default summary code\nif (is.null(df) || nrow(df) == 0) {\n  data.frame(Message = 'No data available') |> \n    datatable(options = list(dom = 't'), rownames = FALSE, class = 'compact')\n} else {\n  summary_df <- df[, .(\n    Records = .N,\n    Min_Value = min(value, na.rm = TRUE),\n    Max_Value = max(value, na.rm = TRUE),\n    Mean_Value = mean(value, na.rm = TRUE)\n  ), by = .(Series = series)]\n  \n  datatable(summary_df, \n    options = list(scrollX = TRUE, pageLength = 15), \n    rownames = FALSE, class = 'compact stripe'\n  )\n}"
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
            
            # Processing Status (button moved to floating bar)
            div(class = "mb-3",
              verbatimTextOutput(ns("data_status"), placeholder = TRUE)
            ),
            
            # Data Processing Settings
            div(class = "mb-3",
              h6("Processing Settings", class = "text-muted mb-2"),
              
              # Data Sampling
              numericInput(
                ns("sample_n_plot"),
                "Sample Data (0 = all)",
                value = 0,
                min = 0,
                step = 1000,
                width = "100%"
              ) |> tooltip("Limit number of data points for performance. 0 = use all data.")
            ),
            
            # Data Filtering 
            div(class = "mb-3",
              h6("Data Filtering", class = "text-muted mb-2"),
              textInput(
                ns("filter_in_plot"),
                "Include Series (regex)",
                placeholder = "e.g., temp.*|pressure",
                width = "100%"
              ) |> tooltip("Regex pattern to include only matching series"),
              
              textInput(
                ns("filter_out_plot"),
                "Exclude Series (regex)", 
                placeholder = "e.g., debug|test",
                width = "100%"
              ) |> tooltip("Regex pattern to exclude matching series"),
              
              textInput(
                ns("rename_plot"),
                "Rename Series (regex)",
                placeholder = "e.g., s/old_name/new_name/g",
                width = "100%"
              ) |> tooltip("Perl-style regex replacement for series names")
            ),
            
            # Source Selection
            div(class = "mb-3",
              h6("Source Selection", class = "text-muted mb-2"),
              uiOutput(ns("source_filter_checkboxes"))
            ),
            
            # Time Range Filtering
            div(class = "mb-3",
              h6("Time Range", class = "text-muted mb-2"),
              checkboxInput(
                ns("filter_time_enabled"),
                "Enable Time Filtering",
                value = FALSE
              ),
              
              conditionalPanel(
                condition = paste0("input['", ns("filter_time_enabled"), "']"),
                dateInput(
                  ns("filter_time_start_date"),
                  "Start Date",
                  width = "100%"
                ),
                timeInput(
                  ns("filter_time_start_time"),
                  "Start Time",
                  value = strptime("00:00:00", "1970-01-01")
                ),
                dateInput(
                  ns("filter_time_end_date"),
                  "End Date", 
                  width = "100%"
                ),
                timeInput(
                  ns("filter_time_end_time"),
                  "End Time",
                  value = strptime("23:59:59", "%T")
                )
              )
            )
          ),
          
          # Main content with right sidebar for ace editor
          layout_sidebar(
            sidebar = sidebar(
              title = "Custom Processing Code",
              width = 600,
              position = "right",
              aceEditor_pre(ns("r_code_plot_process"), value = r_code_plot_process_template),
              open = FALSE
            ),
            
            # Processed data tables
            layout_columns(
              col_widths = c(6, 6),
              card(
                card_header("Processed Data"),
                ui_data_table_display(
                  ns("plot_data_table_modified_data"), 
                  r_code_on_df = DT_head
                )
              ),
              card(
                card_header("Processed Data Summary"),
                ui_data_table_display(
                  ns("plot_data_table_modified_data_summary"), 
                  r_code_on_df = if (exists("r_code_combined_data_summary")) r_code_combined_data_summary else "# Default summary code\nif (is.null(df) || nrow(df) == 0) {\n  data.frame(Message = 'No data available') |> \n    datatable(options = list(dom = 't'), rownames = FALSE, class = 'compact')\n} else {\n  summary_df <- df[, .(\n    Records = .N,\n    Min_Value = min(value, na.rm = TRUE),\n    Max_Value = max(value, na.rm = TRUE),\n    Mean_Value = mean(value, na.rm = TRUE)\n  ), by = .(Series = series)]\n  \n  datatable(summary_df, \n    options = list(scrollX = TRUE, pageLength = 15), \n    rownames = FALSE, class = 'compact stripe'\n  )\n}"
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
            
            # Compact Output Type
            div(class = "mb-2",
              h6("Output Type", class = "text-muted mb-1 small"),
              radioButtons(
                ns("plot_type"),
                NULL,
                choices = list(
                  "Interactive (plotly)" = "interactive",
                  "Static (ggplot2)" = "static", 
                  "Table (DT)" = "table",
                  "Text Summary" = "text"
                ),
                selected = "static",
                inline = TRUE
              ) |> tooltip("Choose the type of output to generate")
            ),
            
            # Interactive-specific settings
            conditionalPanel(
              condition = paste0("input['", ns("plot_type"), "'] == 'interactive'"),
              div(class = "mb-2",
                checkboxInput(
                  ns("plot_interactive_min_max"),
                  "Interactive Min/Max",
                  value = FALSE,
                  width = "100%"
                ) |> tooltip("Enable interactive min/max functionality for plotly plots")
              )
            ),
            
                        # Single Accordion for all Plot Settings
            accordion(
              id = ns("plot_settings_accordion"),
              
              # Basic Settings Panel
              accordion_panel(
                title = "Basic Settings",
                icon = icon("cog"),
                open = TRUE,
                
                # Compact title and caption
                div(class = "mb-2",
                  textInput(ns("plot_title"), "Title", placeholder = "Plot title...", width = "100%")
                ),
                div(class = "mb-2",
                  textInput(ns("plot_caption"), "Caption", placeholder = "Plot caption...", width = "100%")
                ),
                
                # Compact labels in row
                div(class = "row g-1",
                  div(class = "col-6",
                    selectInput(
                      ns("plot_xlabel"),
                      "X Label",
                      choices = list(
                        "Timestamp" = "Timestamp",
                        "Duration Minutes" = "Duration Minutes", 
                        "Duration Hours" = "Duration Hours",
                        "Duration Days" = "Duration Days",
                        "Duration Seconds" = "Duration Seconds"
                      ),
                      selected = "Timestamp",
                      width = "100%"
                    )
                  ),
                  div(class = "col-6",
                    selectizeInput(
                      ns("plot_ylabel"),
                      "Y Label",
                      choices = list(
                        "Temperature [°C]",
                        "Voltage [V]", 
                        "Current [A]", 
                        "Power [W]",
                        "Frequency [Hz]",
                        "Resistance [Ohm]",
                        "Pressure [kPa]",
                        "Humidity [%]",
                        "Percentage [%]"
                      ),
                      width = "100%",
                      selected = NULL, 
                      multiple = FALSE,
                      options = list(create = TRUE)
                    )
                  )
                )
              ),
            
              # Plot Type & Aesthetics Panel
              accordion_panel(
                title = "Plot Type & Aesthetics",
                icon = icon("paint-brush"),
                open = FALSE,
                
                div(class = "mb-2",
                  selectInput(
                    ns("geom_type"),
                    "Plot Type",
                    choices = list(
                      "Line" = "geom_line",
                      "Point" = "geom_point", 
                      "Line + Point" = "geom_line_point",
                      "Area" = "geom_area",
                      "Smooth" = "geom_smooth",
                      "Column" = "geom_col"
                    ),
                    selected = "geom_line",
                    width = "100%"
                  ) |> tooltip("Choose the type of plot geometry")
                ),
                
                div(class = "mb-2",
                  selectInput(
                    ns("plot_color"),
                    "Color by",
                    choices = list("None" = "null"),
                    selected = "null",
                    width = "100%"
                  ) |> tooltip("Group data by color using a column")
                ),
                
                div(class = "mb-2",
                  selectInput(
                    ns("plot_linetype"),
                    "Line Type by",
                    choices = list("None" = "null"),
                    selected = "null",
                    width = "100%"
                  ) |> tooltip("Group data by line type using a column")
                )
              ),
            
                          # Visual Style Panel
              accordion_panel(
                title = "Visual Style",
                icon = icon("palette"),
                open = FALSE,
                
                div(class = "row g-1",
                  div(class = "col-4",
                    numericInput(ns("line_width"), "Line Size", value = 1, min = 0.1, max = 5, step = 0.1, width = "100%")
                  ),
                  div(class = "col-4",
                    numericInput(ns("point_size"), "Point Size", value = 2, min = 0.5, max = 10, step = 0.5, width = "100%")
                  ),
                  div(class = "col-4",
                    numericInput(ns("alpha"), "Transparency", value = 1.0, min = 0.1, max = 1.0, step = 0.1, width = "100%")
                  )
                )
              ),
            
              # Theme & Colors Panel
              accordion_panel(
                title = "Theme & Colors",
                icon = icon("paint-brush"),
                open = FALSE,
                
                div(class = "mb-2",
                  selectInput(
                    ns("plot_theme"),
                    "Theme",
                    choices = list(
                      "Classic" = "theme_classic",
                      "Minimal" = "theme_minimal", 
                      "Dark" = "theme_dark",
                      "Light" = "theme_light",
                      "BW" = "theme_bw",
                      "Void" = "theme_void"
                    ),
                    selected = "theme_classic",
                    width = "100%"
                  )
                ),
                
                div(class = "mb-2",
                  selectInput(
                    ns("color_palette"),
                    "Color Palette",
                    choices = list(
                      "Default" = "default",
                      "Viridis" = "viridis",
                      "Set1" = "Set1",
                      "Set2" = "Set2", 
                      "Dark2" = "Dark2",
                      "Paired" = "Paired",
                      "Paletteer" = "paletteer"
                    ),
                    selected = "default",
                    width = "100%"
                  )
                ),
                
                # Conditional input for paletteer palette name
                conditionalPanel(
                  condition = paste0("input['", ns("color_palette"), "'] == 'paletteer'"),
                  div(class = "mb-2",
                    textInput(
                      ns("color_palette_paletter"),
                      "Paletteer Palette",
                      placeholder = "e.g., 'ggsci::npg', 'RColorBrewer::Set3'",
                      width = "100%"
                    ) |> tooltip("Enter paletteer palette name (e.g., 'ggsci::npg', 'RColorBrewer::Set3', 'viridis::plasma')")
                  )
                )
              ),
            
                          # Axis Controls Panel
              accordion_panel(
                title = "Axis Controls",
                icon = icon("arrows-alt-h"),
                open = FALSE,
                
                div(class = "row g-1",
                  div(class = "col-6",
                    selectInput(
                      ns("x_trans"),
                      "X Transform",
                      choices = list(
                        "Linear" = "identity",
                        "Log" = "log",
                        "Log10" = "log10",
                        "Sqrt" = "sqrt"
                      ),
                      selected = "identity",
                      width = "100%"
                    )
                  ),
                  div(class = "col-6",
                    selectInput(
                      ns("y_trans"), 
                      "Y Transform", 
                      choices = list(
                        "Linear" = "identity",
                        "Log" = "log",
                        "Log10" = "log10",
                        "Sqrt" = "sqrt"
                      ),
                      selected = "identity",
                      width = "100%"
                    )
                  )
                )
              ),
            
                          # Faceting Panel
              accordion_panel(
                title = "Faceting",
                icon = icon("th"),
                open = FALSE,
                
                div(class = "row g-1",
                  div(class = "col-4",
                    numericInput(ns("plot_facet_start"), "Start", value = 1, min = 1, width = "100%")
                  ),
                  div(class = "col-4",
                    numericInput(ns("plot_facet_end"), "End", value = 0, min = 0, width = "100%")
                  ),
                  div(class = "col-4",
                    numericInput(ns("plot_facet_nrow"), "Rows", value = 2, min = 1, width = "100%")
                  )
                ) |> tooltip("Create subplots by extracting characters from series names")
              ),
            
              # Grid Lines Panel
              accordion_panel(
                title = "Grid Lines",
                icon = icon("border-all"),
                open = FALSE,
                
                # Vertical Grid Type Selection
                div(class = "mb-2",
                  selectInput(
                    ns("vertical_grid_type"),
                    "Vertical Grid Type",
                    choices = list(
                      "Auto-detect" = "auto",
                      "Numeric Values" = "numeric", 
                      "Timestamp" = "timestamp"
                    ),
                    selected = "auto",
                    width = "100%"
                  ) |> tooltip("Choose how to interpret X-axis data for vertical grids")
                ),
              
              # Major Vertical Grid
              checkboxInput(ns("enable_major_vgrid"), "Major Vertical Grid"),
              conditionalPanel(
                condition = paste0("input['", ns("enable_major_vgrid"), "']"),
                div(class = "ms-3 border-start ps-2",
                  numericInput(
                    ns("major_vgrid_breaks"),
                    "Grid Interval",
                    value = 10,
                    min = 1,
                    step = 1,
                    width = "100%"
                  ) |> tooltip("Interval between vertical grid lines. For numeric: spacing value. For timestamp: interval in minutes."),
                  div(class = "row g-1",
                    div(class = "col-4",
                      selectInput(
                        ns("major_vgrid_linetype"),
                        "Line Type",
                        choices = list(
                          "Solid" = "solid",
                          "Dashed" = "dashed",
                          "Dotted" = "dotted"
                        ),
                        selected = "solid",
                        width = "100%"
                      )
                    ),
                    div(class = "col-4",
                      numericInput(
                        ns("major_vgrid_linewidth"),
                        "Width",
                        value = 0.5,
                        min = 0.1,
                        max = 2,
                        step = 0.1,
                        width = "100%"
                      )
                    ),
                    div(class = "col-4",
                      selectInput(
                        ns("major_vgrid_color"),
                        "Color",
                        choices = list(
                          "Grey" = "grey90",
                          "Light Grey" = "grey95",
                          "White" = "white",
                          "Black" = "black"
                        ),
                        selected = "grey90",
                        width = "100%"
                      )
                    )
                  )
                )
              ),
              
              # Major Horizontal Grid
              checkboxInput(ns("enable_major_hgrid"), "Major Horizontal Grid"),
              conditionalPanel(
                condition = paste0("input['", ns("enable_major_hgrid"), "']"),
                div(class = "ms-3 border-start ps-2",
                  numericInput(
                    ns("major_hgrid_breaks"),
                    "Grid Interval",
                    value = 5,
                    min = 0.1,
                    step = 0.1,
                    width = "100%"
                  ) |> tooltip("Spacing between horizontal grid lines"),
                  div(class = "row g-1",
                    div(class = "col-4",
                      selectInput(
                        ns("major_hgrid_linetype"),
                        "Line Type",
                        choices = list(
                          "Solid" = "solid",
                          "Dashed" = "dashed",
                          "Dotted" = "dotted"
                        ),
                        selected = "solid",
                        width = "100%"
                      )
                    ),
                    div(class = "col-4",
                      numericInput(
                        ns("major_hgrid_linewidth"),
                        "Width",
                        value = 0.5,
                        min = 0.1,
                        max = 2,
                        step = 0.1,
                        width = "100%"
                      )
                    ),
                    div(class = "col-4",
                      selectInput(
                        ns("major_hgrid_color"),
                        "Color",
                        choices = list(
                          "Grey" = "grey90",
                          "Light Grey" = "grey95",
                          "White" = "white",
                          "Black" = "black"
                        ),
                        selected = "grey90",
                        width = "100%"
                      )
                    )
                  )
                )
              ),
              
              # Minor Vertical Grid
              checkboxInput(ns("enable_minor_vgrid"), "Minor Vertical Grid"),
              conditionalPanel(
                condition = paste0("input['", ns("enable_minor_vgrid"), "']"),
                div(class = "ms-3 border-start ps-2",
                  numericInput(
                    ns("minor_vgrid_breaks"),
                    "Grid Interval",
                    value = 2,
                    min = 0.1,
                    step = 0.1,
                    width = "100%"
                  ) |> tooltip("Spacing between minor vertical grid lines"),
                  div(class = "row g-1",
                    div(class = "col-4",
                      selectInput(
                        ns("minor_vgrid_linetype"),
                        "Line Type",
                        choices = list(
                          "Solid" = "solid",
                          "Dashed" = "dashed",
                          "Dotted" = "dotted"
                        ),
                        selected = "solid",
                        width = "100%"
                      )
                    ),
                    div(class = "col-4",
                      numericInput(
                        ns("minor_vgrid_linewidth"),
                        "Width",
                        value = 0.25,
                        min = 0.1,
                        max = 1,
                        step = 0.05,
                        width = "100%"
                      )
                    ),
                    div(class = "col-4",
                      selectInput(
                        ns("minor_vgrid_color"),
                        "Color",
                        choices = list(
                          "Light Grey" = "grey95",
                          "Grey" = "grey90",
                          "White" = "white",
                          "Black" = "black"
                        ),
                        selected = "grey95",
                        width = "100%"
                      )
                    )
                  )
                )
              ),
              
              # Minor Horizontal Grid
              checkboxInput(ns("enable_minor_hgrid"), "Minor Horizontal Grid"),
              conditionalPanel(
                condition = paste0("input['", ns("enable_minor_hgrid"), "']"),
                div(class = "ms-3 border-start ps-2",
                  numericInput(
                    ns("minor_hgrid_breaks"),
                    "Grid Interval",
                    value = 1,
                    min = 0.1,
                    step = 0.1,
                    width = "100%"
                  ) |> tooltip("Spacing between minor horizontal grid lines"),
                  div(class = "row g-1",
                    div(class = "col-4",
                      selectInput(
                        ns("minor_hgrid_linetype"),
                        "Line Type",
                        choices = list(
                          "Solid" = "solid",
                          "Dashed" = "dashed",
                          "Dotted" = "dotted"
                        ),
                        selected = "solid",
                        width = "100%"
                      )
                    ),
                    div(class = "col-4",
                      numericInput(
                        ns("minor_hgrid_linewidth"),
                        "Width",
                        value = 0.25,
                        min = 0.1,
                        max = 1,
                        step = 0.05,
                        width = "100%"
                      )
                    ),
                    div(class = "col-4",
                      selectInput(
                        ns("minor_hgrid_color"),
                        "Color",
                        choices = list(
                          "Light Grey" = "grey95",
                          "Grey" = "grey90",
                          "White" = "white",
                          "Black" = "black"
                        ),
                        selected = "grey95",
                        width = "100%"
                      )
                    )
                  )
                )
              )
            ),
            
              # Axis Limits Panel
              accordion_panel(
                title = "Axis Limits",
                icon = icon("arrows-alt-v"),
                open = FALSE,
                
                # Y Axis Limits with individual toggles
                div(class = "row g-1",
                  div(class = "col-6",
                    checkboxInput(ns("enable_y_start"), "Set Y Start"),
                    conditionalPanel(
                      condition = paste0("input['", ns("enable_y_start"), "']"),
                      div(class = "ms-3 border-start ps-2",
                        numericInput(ns("y_start"), "Y Start", value = 0, width = "100%")
                      )
                    )
                  ),
                  div(class = "col-6",
                    checkboxInput(ns("enable_y_end"), "Set Y End"),
                    conditionalPanel(
                      condition = paste0("input['", ns("enable_y_end"), "']"),
                      div(class = "ms-3 border-start ps-2",
                        numericInput(ns("y_end"), "Y End", value = 100, width = "100%")
                      )
                    )
                  )
                ),
              
              # X Axis Type Selection
              selectInput(
                ns("x_limit_type"),
                "X-Axis Type",
                choices = list(
                  "Numeric Value" = "numeric",
                  "Timestamp" = "timestamp"
                ),
                selected = "numeric",
                width = "100%"
              ),
              
              # X Axis Limits with individual toggles
              conditionalPanel(
                condition = paste0("input['", ns("x_limit_type"), "'] == 'numeric'"),
                div(class = "row g-1",
                  div(class = "col-6",
                    checkboxInput(ns("enable_x_start_numeric"), "Set X Start"),
                    conditionalPanel(
                      condition = paste0("input['", ns("enable_x_start_numeric"), "']"),
                      div(class = "ms-3 border-start ps-2",
                        numericInput(
                          ns("x_start_numeric"),
                          "X Start",
                          value = 0,
                          width = "100%"
                        )
                      )
                    )
                  ),
                  div(class = "col-6",
                    checkboxInput(ns("enable_x_end_numeric"), "Set X End"),
                    conditionalPanel(
                      condition = paste0("input['", ns("enable_x_end_numeric"), "']"),
                      div(class = "ms-3 border-start ps-2",
                        numericInput(
                          ns("x_end_numeric"),
                          "X End",
                          value = 100,
                          width = "100%"
                        )
                      )
                    )
                  )
                )
              ),
              
              conditionalPanel(
                condition = paste0("input['", ns("x_limit_type"), "'] == 'timestamp'"),
                div(class = "row g-1",
                  div(class = "col-6",
                    checkboxInput(ns("enable_x_start_timestamp"), "Set X Start"),
                    conditionalPanel(
                      condition = paste0("input['", ns("enable_x_start_timestamp"), "']"),
                      div(class = "ms-3 border-start ps-2",
                        dateInput(
                          ns("x_start_date"),
                          "Start Date",
                          width = "100%"
                        ),
                        timeInput(
                          ns("x_start_time"),
                          "Start Time",
                          value = strptime("00:00:00", "%T")
                        )
                      )
                    )
                  ),
                  div(class = "col-6",
                    checkboxInput(ns("enable_x_end_timestamp"), "Set X End"),
                    conditionalPanel(
                      condition = paste0("input['", ns("enable_x_end_timestamp"), "']"),
                      div(class = "ms-3 border-start ps-2",
                        dateInput(
                          ns("x_end_date"),
                          "End Date",
                          width = "100%"
                        ),
                        timeInput(
                          ns("x_end_time"),
                          "End Time",
                          value = strptime("23:59:59", "%T")
                        )
                      )
                    )
                  )
                )
              )
            ),
            
              # Font & Legend Panel
              accordion_panel(
                title = "Font & Legend",
                icon = icon("font"),
                open = FALSE,
                
                # Font sizes
                div(class = "mb-2",
                  h6("Font Sizes", class = "text-muted mb-1 small"),
                  div(class = "row g-1",
                    div(class = "col-4",
                      numericInput(ns("title_font_size"), "Title", value = 14, min = 8, max = 30, width = "100%")
                    ),
                    div(class = "col-4",
                      numericInput(ns("xaxis_font_size"), "X-Axis", value = 12, min = 8, max = 24, width = "100%")
                    ),
                    div(class = "col-4",
                      numericInput(ns("yaxis_font_size"), "Y-Axis", value = 12, min = 8, max = 24, width = "100%")
                    )
                  )
                ),
                
                # Legend Controls
                div(class = "mb-2",
                  h6("Legend", class = "text-muted mb-1 small"),
                  div(class = "row g-2",
                    div(class = "col-6",
                      selectInput(
                        ns("legend_position"),
                        "Position",
                        choices = list(
                          "Right" = "right",
                          "Top" = "top", 
                          "Bottom" = "bottom",
                          "Left" = "left",
                          "None" = "none"
                        ),
                        selected = "right",
                        width = "100%"
                      )
                    ),
                    div(class = "col-6",
                      numericInput(ns("legend_font_size"), "Font Size", value = 12, min = 8, max = 20, width = "100%")
                    )
                  )
                ),
                
                # Caption Controls
                div(class = "mb-2",
                  h6("Caption", class = "text-muted mb-1 small"),
                  div(class = "row g-1",
                    div(class = "col-6",
                      numericInput(ns("caption_font_size"), "Font Size", value = 10, min = 6, max = 18, width = "100%")
                    ),
                    div(class = "col-6",
                      selectInput(
                        ns("caption_color"),
                        "Color",
                        choices = list(
                          "Black" = "black",
                          "Grey" = "grey50",
                          "Dark Grey" = "grey30",
                          "Light Grey" = "grey70",
                          "Blue" = "blue",
                          "Red" = "red"
                        ),
                        selected = "grey50",
                        width = "100%"
                      )
                    )
                  ),
                  div(class = "row g-1",
                    div(class = "col-6",
                      numericInput(ns("caption_x"), "X Position", value = 1, min = 0, max = 1, step = 0.1, width = "100%") |> tooltip("0 = left, 0.5 = center, 1 = right")
                    ),
                    div(class = "col-6",
                      numericInput(ns("caption_y"), "Y Position", value = 0, min = 0, max = 1, step = 0.1, width = "100%") |> tooltip("0 = bottom, 0.5 = center, 1 = top")
                    )
                  )
                )
              ),
            
              # Lines & Overlays Panel
              accordion_panel(
                title = "Lines & Overlays",
                icon = icon("chart-line"),
                open = FALSE,
                
                # Trend Line
                div(class = "mb-2",
                  checkboxInput(
                    ns("add_smooth"),
                    "Trend Line",
                    value = FALSE
                  ) |> tooltip("Add a smooth trend line to the plot"),
                  
                  conditionalPanel(
                    condition = paste0("input['", ns("add_smooth"), "']"),
                    div(class = "ms-3 border-start ps-2",
                      textInput(ns("smooth_name"), "Legend Name", value = "Trend", placeholder = "Name for legend...", width = "100%"),
                      selectInput(
                        ns("smooth_method"),
                        "Method",
                        choices = list(
                          "Loess" = "loess",
                          "Linear" = "lm",
                          "GAM" = "gam"
                        ),
                        selected = "loess",
                        width = "100%"
                      ),
                      div(class = "row g-1",
                        div(class = "col-3",
                          selectInput(ns("smooth_linetype"), "Type", choices = list("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted", "Dot-dash" = "dotdash"), selected = "solid", width = "100%")
                        ),
                        div(class = "col-3",
                          numericInput(ns("smooth_linewidth"), "Width", value = 1, min = 0.1, max = 3, step = 0.1, width = "100%")
                        ),
                        div(class = "col-3",
                          selectInput(ns("smooth_color"), "Color", choices = list("Red" = "red", "Blue" = "blue", "Green" = "green", "Purple" = "purple", "Orange" = "orange", "Black" = "black"), selected = "red", width = "100%")
                        ),
                        div(class = "col-3",
                          div(style = "margin-top: 25px;",
                            checkboxInput(ns("smooth_legend"), "Legend", value = TRUE)
                          )
                        )
                      )
                    )
                  )
                ),
              
              # Horizontal Line 1
              checkboxInput(ns("enable_hline_1"), "Horizontal Line 1"),
              conditionalPanel(
                condition = paste0("input['", ns("enable_hline_1"), "']"),
                div(class = "ms-3 border-start ps-2",
                  textInput(
                    ns("hline_1_name"),
                    "Legend Name",
                    value = "H-Line 1",
                    placeholder = "Name for legend...",
                    width = "100%"
                  ),
                  numericInput(
                    ns("hline_1"),
                    "Y Position",
                    value = 0,
                    width = "100%"
                  ),
                  div(class = "row g-1",
                    div(class = "col-3",
                      selectInput(
                        ns("hline_1_linetype"),
                        "Type",
                        choices = list(
                          "Dashed" = "dashed",
                          "Solid" = "solid",
                          "Dotted" = "dotted",
                          "Dot-dash" = "dotdash"
                        ),
                        selected = "dashed",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      numericInput(
                        ns("hline_1_linewidth"),
                        "Width",
                        value = 1,
                        min = 0.1,
                        max = 3,
                        step = 0.1,
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      selectInput(
                        ns("hline_1_color"),
                        "Color",
                        choices = list(
                          "Red" = "red",
                          "Blue" = "blue",
                          "Green" = "green",
                          "Purple" = "purple",
                          "Orange" = "orange",
                          "Black" = "black"
                        ),
                        selected = "red",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      div(style = "margin-top: 25px;",
                        checkboxInput(ns("hline_1_legend"), "Legend", value = FALSE)
                      )
                    )
                  )
                )
              ),
              
              # Horizontal Line 2
              checkboxInput(ns("enable_hline_2"), "Horizontal Line 2"),
              conditionalPanel(
                condition = paste0("input['", ns("enable_hline_2"), "']"),
                div(class = "ms-3 border-start ps-2",
                  textInput(
                    ns("hline_2_name"),
                    "Legend Name",
                    value = "H-Line 2",
                    placeholder = "Name for legend...",
                    width = "100%"
                  ),
                  numericInput(
                    ns("hline_2"),
                    "Y Position",
                    value = 0,
                    width = "100%"
                  ),
                  div(class = "row g-1",
                    div(class = "col-3",
                      selectInput(
                        ns("hline_2_linetype"),
                        "Type",
                        choices = list(
                          "Dashed" = "dashed",
                          "Solid" = "solid",
                          "Dotted" = "dotted",
                          "Dot-dash" = "dotdash"
                        ),
                        selected = "dashed",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      numericInput(
                        ns("hline_2_linewidth"),
                        "Width",
                        value = 1,
                        min = 0.1,
                        max = 3,
                        step = 0.1,
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      selectInput(
                        ns("hline_2_color"),
                        "Color",
                        choices = list(
                          "Blue" = "blue",
                          "Red" = "red",
                          "Green" = "green",
                          "Purple" = "purple",
                          "Orange" = "orange",
                          "Black" = "black"
                        ),
                        selected = "blue",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      div(style = "margin-top: 25px;",
                        checkboxInput(ns("hline_2_legend"), "Legend", value = FALSE)
                      )
                    )
                  )
                )
              ),
              
              # Horizontal Line 3
              checkboxInput(ns("enable_hline_3"), "Horizontal Line 3"),
              conditionalPanel(
                condition = paste0("input['", ns("enable_hline_3"), "']"),
                div(class = "ms-3 border-start ps-2",
                  textInput(
                    ns("hline_3_name"),
                    "Legend Name",
                    value = "H-Line 3",
                    placeholder = "Name for legend...",
                    width = "100%"
                  ),
                  numericInput(
                    ns("hline_3"),
                    "Y Position",
                    value = 0,
                    width = "100%"
                  ),
                  div(class = "row g-1",
                    div(class = "col-3",
                      selectInput(
                        ns("hline_3_linetype"),
                        "Type",
                        choices = list(
                          "Dashed" = "dashed",
                          "Solid" = "solid",
                          "Dotted" = "dotted",
                          "Dot-dash" = "dotdash"
                        ),
                        selected = "dashed",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      numericInput(
                        ns("hline_3_linewidth"),
                        "Width",
                        value = 1,
                        min = 0.1,
                        max = 3,
                        step = 0.1,
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      selectInput(
                        ns("hline_3_color"),
                        "Color",
                        choices = list(
                          "Purple" = "purple",
                          "Red" = "red",
                          "Blue" = "blue",
                          "Green" = "green",
                          "Orange" = "orange",
                          "Black" = "black"
                        ),
                        selected = "purple",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      div(style = "margin-top: 25px;",
                        checkboxInput(ns("hline_3_legend"), "Legend", value = FALSE)
                      )
                    )
                  )
                )
              ),
              
              # Horizontal Line 4
              checkboxInput(ns("enable_hline_4"), "Horizontal Line 4"),
              conditionalPanel(
                condition = paste0("input['", ns("enable_hline_4"), "']"),
                div(class = "ms-3 border-start ps-2",
                  textInput(
                    ns("hline_4_name"),
                    "Legend Name",
                    value = "H-Line 4",
                    placeholder = "Name for legend...",
                    width = "100%"
                  ),
                  numericInput(
                    ns("hline_4"),
                    "Y Position",
                    value = 0,
                    width = "100%"
                  ),
                  div(class = "row g-1",
                    div(class = "col-3",
                      selectInput(
                        ns("hline_4_linetype"),
                        "Type",
                        choices = list(
                          "Dashed" = "dashed",
                          "Solid" = "solid",
                          "Dotted" = "dotted",
                          "Dot-dash" = "dotdash"
                        ),
                        selected = "dashed",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      numericInput(
                        ns("hline_4_linewidth"),
                        "Width",
                        value = 1,
                        min = 0.1,
                        max = 3,
                        step = 0.1,
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      selectInput(
                        ns("hline_4_color"),
                        "Color",
                        choices = list(
                          "Orange" = "orange",
                          "Red" = "red",
                          "Blue" = "blue",
                          "Green" = "green",
                          "Purple" = "purple",
                          "Black" = "black"
                        ),
                        selected = "orange",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      div(style = "margin-top: 25px;",
                        checkboxInput(ns("hline_4_legend"), "Legend", value = FALSE)
                      )
                    )
                  )
                )
              ),
              
              # Vertical Line 1
              checkboxInput(ns("enable_vline_1"), "Vertical Line 1"),
              conditionalPanel(
                condition = paste0("input['", ns("enable_vline_1"), "']"),
                div(class = "ms-3 border-start ps-2",
                  textInput(
                    ns("vline_1_name"),
                    "Legend Name",
                    value = "V-Line 1",
                    placeholder = "Name for legend...",
                    width = "100%"
                  ),
                  numericInput(
                    ns("vline_1"),
                    "X Position",
                    value = 0,
                    width = "100%"
                  ),
                  div(class = "row g-1",
                    div(class = "col-3",
                      selectInput(
                        ns("vline_1_linetype"),
                        "Type",
                        choices = list(
                          "Dashed" = "dashed",
                          "Solid" = "solid",
                          "Dotted" = "dotted",
                          "Dot-dash" = "dotdash"
                        ),
                        selected = "dashed",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      numericInput(
                        ns("vline_1_linewidth"),
                        "Width",
                        value = 1,
                        min = 0.1,
                        max = 3,
                        step = 0.1,
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      selectInput(
                        ns("vline_1_color"),
                        "Color",
                        choices = list(
                          "Red" = "red",
                          "Blue" = "blue",
                          "Green" = "green",
                          "Purple" = "purple",
                          "Orange" = "orange",
                          "Black" = "black"
                        ),
                        selected = "red",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      div(style = "margin-top: 25px;",
                        checkboxInput(ns("vline_1_legend"), "Legend", value = FALSE)
                      )
                    )
                  )
                )
              ),
              
              # Vertical Line 2
              checkboxInput(ns("enable_vline_2"), "Vertical Line 2"),
              conditionalPanel(
                condition = paste0("input['", ns("enable_vline_2"), "']"),
                div(class = "ms-3 border-start ps-2",
                  textInput(
                    ns("vline_2_name"),
                    "Legend Name",
                    value = "V-Line 2",
                    placeholder = "Name for legend...",
                    width = "100%"
                  ),
                  numericInput(
                    ns("vline_2"),
                    "X Position",
                    value = 0,
                    width = "100%"
                  ),
                  div(class = "row g-1",
                    div(class = "col-3",
                      selectInput(
                        ns("vline_2_linetype"),
                        "Type",
                        choices = list(
                          "Dashed" = "dashed",
                          "Solid" = "solid",
                          "Dotted" = "dotted",
                          "Dot-dash" = "dotdash"
                        ),
                        selected = "dashed",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      numericInput(
                        ns("vline_2_linewidth"),
                        "Width",
                        value = 1,
                        min = 0.1,
                        max = 3,
                        step = 0.1,
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      selectInput(
                        ns("vline_2_color"),
                        "Color",
                        choices = list(
                          "Blue" = "blue",
                          "Red" = "red",
                          "Green" = "green",
                          "Purple" = "purple",
                          "Orange" = "orange",
                          "Black" = "black"
                        ),
                        selected = "blue",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      div(style = "margin-top: 25px;",
                        checkboxInput(ns("vline_2_legend"), "Legend", value = FALSE)
                      )
                    )
                  )
                )
              ),
              
              # Vertical Line 3
              checkboxInput(ns("enable_vline_3"), "Vertical Line 3"),
              conditionalPanel(
                condition = paste0("input['", ns("enable_vline_3"), "']"),
                div(class = "ms-3 border-start ps-2",
                  textInput(
                    ns("vline_3_name"),
                    "Legend Name",
                    value = "V-Line 3",
                    placeholder = "Name for legend...",
                    width = "100%"
                  ),
                  numericInput(
                    ns("vline_3"),
                    "X Position",
                    value = 0,
                    width = "100%"
                  ),
                  div(class = "row g-1",
                    div(class = "col-3",
                      selectInput(
                        ns("vline_3_linetype"),
                        "Type",
                        choices = list(
                          "Dashed" = "dashed",
                          "Solid" = "solid",
                          "Dotted" = "dotted",
                          "Dot-dash" = "dotdash"
                        ),
                        selected = "dashed",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      numericInput(
                        ns("vline_3_linewidth"),
                        "Width",
                        value = 1,
                        min = 0.1,
                        max = 3,
                        step = 0.1,
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      selectInput(
                        ns("vline_3_color"),
                        "Color",
                        choices = list(
                          "Purple" = "purple",
                          "Red" = "red",
                          "Blue" = "blue",
                          "Green" = "green",
                          "Orange" = "orange",
                          "Black" = "black"
                        ),
                        selected = "purple",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      div(style = "margin-top: 25px;",
                        checkboxInput(ns("vline_3_legend"), "Legend", value = FALSE)
                      )
                    )
                  )
                )
              ),
              
              # Vertical Line 4
              checkboxInput(ns("enable_vline_4"), "Vertical Line 4"),
              conditionalPanel(
                condition = paste0("input['", ns("enable_vline_4"), "']"),
                div(class = "ms-3 border-start ps-2",
                  textInput(
                    ns("vline_4_name"),
                    "Legend Name",
                    value = "V-Line 4",
                    placeholder = "Name for legend...",
                    width = "100%"
                  ),
                  numericInput(
                    ns("vline_4"),
                    "X Position",
                    value = 0,
                    width = "100%"
                  ),
                  div(class = "row g-1",
                    div(class = "col-3",
                      selectInput(
                        ns("vline_4_linetype"),
                        "Type",
                        choices = list(
                          "Dashed" = "dashed",
                          "Solid" = "solid",
                          "Dotted" = "dotted",
                          "Dot-dash" = "dotdash"
                        ),
                        selected = "dashed",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      numericInput(
                        ns("vline_4_linewidth"),
                        "Width",
                        value = 1,
                        min = 0.1,
                        max = 3,
                        step = 0.1,
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      selectInput(
                        ns("vline_4_color"),
                        "Color",
                        choices = list(
                          "Orange" = "orange",
                          "Red" = "red",
                          "Blue" = "blue",
                          "Green" = "green",
                          "Purple" = "purple",
                          "Black" = "black"
                        ),
                        selected = "orange",
                        width = "100%"
                      )
                    ),
                    div(class = "col-3",
                      div(style = "margin-top: 25px;",
                        checkboxInput(ns("vline_4_legend"), "Legend", value = FALSE)
                      )
                    )
                  )
                )
              )
            ),
            
            # Download Options
            div(class = "mt-3",
              h6("Download Options", class = "text-muted mb-2"),
              
              # Format Selection (dynamic based on plot type)
              selectInput(
                ns("download_format"),
                "Format",
                choices = list(
                  "Auto (recommended)" = "auto",
                  "PNG (high quality)" = "png", 
                  "JPEG (smaller size)" = "jpeg",
                  "PDF (vector graphics)" = "pdf",
                  "SVG (web vector)" = "svg",
                  "HTML (interactive)" = "html",
                  "JSON (data only)" = "json"
                ),
                selected = "auto",
                width = "100%"
              ) |> tooltip("Choose output format - Auto selects best format for plot type"),
              
              # Size Options
              div(class = "row g-2 mb-2",
                div(class = "col-6",
                  numericInput(
                    ns("download_width"),
                    "Width",
                    value = 12,
                    min = 3,
                    max = 24,
                    step = 1,
                    width = "100%"
                  ) |> tooltip("Width in inches for static plots")
                ),
                div(class = "col-6",
                  numericInput(
                    ns("download_height"), 
                    "Height",
                    value = 8,
                    min = 2,
                    max = 16,
                    step = 1,
                    width = "100%"
                  ) |> tooltip("Height in inches for static plots")
                )
              ),
              
              # DPI/Quality Options
              div(class = "row g-2 mb-3",
                div(class = "col-6",
                  selectInput(
                    ns("download_dpi"),
                    "Quality",
                    choices = list(
                      "Web (150 DPI)" = 150,
                      "Print (300 DPI)" = 300,
                      "High (600 DPI)" = 600
                    ),
                    selected = 300,
                    width = "100%"
                  ) |> tooltip("Resolution for raster formats (PNG/JPEG)")
                ),
                div(class = "col-6",
                  selectInput(
                    ns("download_preset"),
                    "Preset",
                    choices = list(
                      "Custom" = "custom",
                      "Presentation" = "presentation", # 16:9, 300 DPI
                      "Publication" = "publication",   # 7x5, 600 DPI
                      "Poster" = "poster",             # 12x8, 300 DPI
                      "Web" = "web"                    # 10x6, 150 DPI
                    ),
                    selected = "custom",
                    width = "100%"
                  ) |> tooltip("Quick size presets for common use cases")
                )
              ),
              
              # Download Button
              downloadButton(
                ns("download_output"), 
                "Download Plot",
                icon = icon("download"),
                class = "btn-outline-primary w-100"
              ) |> tooltip("Download the current plot with selected options")
            )
          ),
          
          ), # Close single accordion
          
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
                  "Data Processing",
                  icon = icon("cogs"),
                  aceEditor_pre(ns("r_code_plot_data_processing"), value = ggplot_data_processing_template)
                ),
                nav_panel(
                  "Base Setup",
                  icon = icon("layer-group"),
                  aceEditor_pre(ns("r_code_plot_base_setup"), value = ggplot_base_setup_template)
                ),
                nav_panel(
                  "Themes & Style",
                  icon = icon("palette"),
                  aceEditor_pre(ns("r_code_plot_themes_styling"), value = ggplot_themes_styling_template)
                ),
                nav_panel(
                  "Lines & Overlays",
                  icon = icon("chart-line"),
                  aceEditor_pre(ns("r_code_plot_statistical_overlays"), value = ggplot_statistical_overlays_template)
                ),
                nav_panel(
                  "Grid & Axes",
                  icon = icon("border-all"),
                  aceEditor_pre(ns("r_code_plot_grid_axes"), value = ggplot_grid_axes_template)
                ),
                nav_panel(
                  "Faceting & Final",
                  icon = icon("th"),
                  aceEditor_pre(ns("r_code_plot_faceting_final"), value = ggplot_faceting_final_template)
                ),
                nav_panel(
                  "Combined Static",
                  icon = icon("image"),
                  aceEditor_pre(ns("r_code_plot_static"), value = ggplot_static_template)
                ),
                nav_panel(
                  "Interactive", 
                  icon = icon("chart-area"),
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
              uiOutput(ns("plot_output"))
            )
        )
      )
  )
    ) # Close navset_card_pill
  ) # Close module-container div
}
