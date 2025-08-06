# --- Server Module for Plotter ---
server_plotter <- function(id, combined_data_reactive, main_session_input = NULL) { # Added main_session_input
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Tab renaming functionality for plotter (same as importer)
    plot_rename_trigger <- reactiveVal(NULL)
    
    # Auto-apply plot name when input changes (no button needed)
    observeEvent(input$plot_name, {
      req(input$plot_name)
      new_name <- trimws(input$plot_name)
      
      cat("=== PLOT NAME AUTO-APPLY ===\n")
      cat("Module ID:", id, "\n")
      cat("New name:", new_name, "\n")
      
      if (new_name == "") {
        return() # Don't apply empty names
      }
      
      # Set the reactive value to trigger parent's observer
      current_title <- id # Use the module ID as current title
      plot_rename_trigger(list(
        moduleId = id,
        newName = new_name,
        currentTitle = current_title,
        timestamp = Sys.time() # Ensure trigger fires even with same name
      ))
      
      cat("Plot rename trigger set automatically\n")
      showNotification(paste("Plot renamed to:", new_name), type = "message", duration = 2)
    }, ignoreInit = TRUE) # Only trigger on user changes, not initial load

    # --- Plot-specific Data Processing ---
    df_plot_data_processed <- reactiveVal()
    r_code_plot_process_reactive <- debounce(reactive(input$r_code_plot_process), 500)
    ace_server_functions(ns("r_code_plot_process"))
    
    # Multiple ace editors for different plot components
    r_code_plot_text_reactive <- debounce(reactive(input$r_code_plot_text), 500)
    r_code_plot_data_processing_reactive <- debounce(reactive(input$r_code_plot_data_processing), 500)
    r_code_plot_base_setup_reactive <- debounce(reactive(input$r_code_plot_base_setup), 500)
    r_code_plot_themes_styling_reactive <- debounce(reactive(input$r_code_plot_themes_styling), 500)
    r_code_plot_statistical_overlays_reactive <- debounce(reactive(input$r_code_plot_statistical_overlays), 500)
    r_code_plot_grid_axes_reactive <- debounce(reactive(input$r_code_plot_grid_axes), 500)
    r_code_plot_faceting_final_reactive <- debounce(reactive(input$r_code_plot_faceting_final), 500)
    r_code_plot_static_reactive <- debounce(reactive(input$r_code_plot_static), 500)
    r_code_plot_interactive_reactive <- debounce(reactive(input$r_code_plot_interactive), 500)
    r_code_plot_table_reactive <- debounce(reactive(input$r_code_plot_table), 500)
    r_code_plot_final_reactive <- debounce(reactive(input$r_code_plot_final), 500)
    
    # Ace server functions for each editor
    ace_server_functions(ns("r_code_plot_text"))
    ace_server_functions(ns("r_code_plot_data_processing"))
    ace_server_functions(ns("r_code_plot_base_setup"))
    ace_server_functions(ns("r_code_plot_themes_styling"))
    ace_server_functions(ns("r_code_plot_statistical_overlays"))
    ace_server_functions(ns("r_code_plot_grid_axes"))
    ace_server_functions(ns("r_code_plot_faceting_final"))
    ace_server_functions(ns("r_code_plot_static"))
    ace_server_functions(ns("r_code_plot_interactive"))
    ace_server_functions(ns("r_code_plot_table"))
    ace_server_functions(ns("r_code_plot_final"))

    # Update source filter checkboxes when combined data changes
    observe({
      req(combined_data_reactive())
      
      combined_df <- combined_data_reactive()
      if (!is.null(combined_df) && nrow(combined_df) > 0 && 'source_importer_id' %in% names(combined_df)) {
        # Get unique source IDs
        unique_sources <- unique(combined_df$source_importer_id)
        
        if (length(unique_sources) > 0) {
          # Create mapping from module IDs to friendly names
          source_labels <- sapply(unique_sources, function(source_id) {
            # Convert data_import_module_1 to "Data Import 1"
            if (grepl("^data_import_module_", source_id)) {
              tab_number <- gsub("data_import_module_", "", source_id)
              paste("Data Import", tab_number)
            } else {
              # Fallback for other naming patterns
              source_id
            }
          })
          
          # Generate checkboxes
          checkbox_list <- lapply(seq_along(unique_sources), function(i) {
            source_id <- unique_sources[i]
            label <- source_labels[i]
            
            div(class = "form-check form-check-inline",
              tags$input(
                type = "checkbox",
                class = "form-check-input",
                id = session$ns(paste0("source_", source_id)),
                value = source_id,
                checked = "checked" # Default to all sources selected
              ),
              tags$label(
                class = "form-check-label small",
                `for` = session$ns(paste0("source_", source_id)),
                label
              )
            )
          })
          
          # Update the UI
          output$source_filter_checkboxes <- renderUI({
            tagList(checkbox_list)
          })
        }
      } else {
        # No data available
        output$source_filter_checkboxes <- renderUI({
          p(class = "text-muted small", "No data sources available yet")
        })
      }
    })

    # Process data when button is clicked
    observeEvent(input$data_render, {
      req(combined_data_reactive())
      
      spsComps::shinyCatch({
        # Get the combined data
        combined_df <- data.table::copy(combined_data_reactive())
        
        if (is.null(combined_df) || nrow(combined_df) == 0) {
          showNotification("No data available to process", type = "warning")
          return()
        }
        
        # Apply source filtering
        if ('source_importer_id' %in% names(combined_df)) {
          # Get all unique sources
          unique_sources <- unique(combined_df$source_importer_id)
          
          # Check which sources are selected
          selected_sources <- c()
          for (source_id in unique_sources) {
            checkbox_id <- paste0("source_", source_id)
            if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
              selected_sources <- c(selected_sources, source_id)
            }
          }
          
          # Filter data by selected sources
          if (length(selected_sources) > 0) {
            combined_df <- combined_df[combined_df$source_importer_id %in% selected_sources, ]
          } else {
            # If no sources selected, show warning and use all data
            showNotification("No sources selected - using all data", type = "warning")
          }
        }
        
        # Apply sampling if specified
        if (!is.null(input$sample_n_plot) && input$sample_n_plot > 0 && input$sample_n_plot < 1) {
          n_sample <- floor(nrow(combined_df) * input$sample_n_plot)
          if (n_sample > 0) {
            combined_df <- combined_df[sample(nrow(combined_df), n_sample), ]
          }
        }
        
        # Environment for plot data processing
        env <- new.env(parent = .GlobalEnv)
        env$df <- as.data.table(combined_df) # Start with filtered and sampled data
        env$input <- input # Expose module's input for filtering etc.
        env$filter_in <- filter_in
        env$filter_out <- filter_out
        env$rname <- rname
        env$str_extract <- stringr::str_extract
        env$sample <- sample # base R sample
        
        # Add code sharing functions
        env$get_plotter_code <- get_plotter_code
        env$get_importer_code <- get_importer_code
        env$main_session_input <- main_session_input
        
        processed_df <- eval(parse(text = r_code_plot_process_reactive()), envir = env)
        df_plot_data_processed(processed_df)
        
        # Update aesthetics choices based on processed data columns
        if (!is.null(processed_df) && ncol(processed_df) > 0) {
          column_choices <- c("None" = "null", setNames(names(processed_df), names(processed_df)))
          
          updateSelectInput(session, "plot_color", choices = column_choices, selected = if("series" %in% names(processed_df)) "series" else "null")
          updateSelectInput(session, "plot_linetype", choices = column_choices, selected = "null")
        }
        
        showNotification("Plot-specific data processing complete.", type = "message")
      })
    })

    # Display tables for plot data
    server_data_table_display(
      "plot_data_table_input_data",
      reactive(combined_data_reactive()) # Show the raw input to this plotter
    )
    server_data_table_display(
      "plot_data_table_input_data_summary",
      reactive(combined_data_reactive()) # Show summary of raw input data
    )
    server_data_table_display(
      "plot_data_table_modified_data",
      reactive(df_plot_data_processed()) # Show data after plot-specific processing
    )
    server_data_table_display(
      "plot_data_table_modified_data_summary",
      reactive(df_plot_data_processed()) # Summary of processed data
    )

    # --- Plot Rendering Logic ---
    plot_object_reactive <- reactiveVal(NULL)
    
    observeEvent(input$plot_render, {
      spsComps::shinyCatch({
        req(df_plot_data_processed()) # Requires data processed for *this* plot
        
        # Environment for plot generation
        env <- new.env(parent = .GlobalEnv)
        env$df <- df_plot_data_processed()
        env$input <- input # Expose module's input for titles, labels, etc.
        
        # Check if ggplot2 can load properly
        ggplot2_available <- tryCatch({
          # Test if we can actually load ggplot2 and its dependencies
          library(ggplot2, quietly = TRUE)
          # Test a simple ggplot operation
          test_plot <- ggplot(data.frame(x=1, y=1), aes(x=x, y=y)) + geom_point()
          TRUE
        }, error = function(e) {
          cat("ggplot2 loading failed:", e$message, "\n")
          FALSE
        })
        
        if (ggplot2_available) {
          # Add ggplot2 functions - full version
          env$ggplot <- ggplot2::ggplot
          env$geom_line <- ggplot2::geom_line
          env$geom_point <- ggplot2::geom_point
          env$geom_ribbon <- ggplot2::geom_ribbon
          env$aes <- ggplot2::aes
          env$labs <- ggplot2::labs
          env$ggtitle <- ggplot2::ggtitle # Deprecated, use labs(title=)
          env$xlab <- ggplot2::xlab # Deprecated
          env$ylab <- ggplot2::ylab # Deprecated
          env$theme_bw <- ggplot2::theme_bw
          env$facet_wrap <- ggplot2::facet_wrap
          env$ggplot2_available <- TRUE
        } else {
          # Create fallback functions that use base R plots or plotly
          env$ggplot2_available <- FALSE
          env$ggplot <- function(data = NULL, mapping = NULL, ...) {
            warning("ggplot2 not available - use plotly functions instead")
            return(list(data = data, mapping = mapping, type = "ggplot_fallback"))
          }
          env$geom_line <- function(...) {
            warning("ggplot2 not available - use plotly::add_lines() instead")
            return(list(type = "geom_fallback"))
          }
          env$geom_point <- function(...) {
            warning("ggplot2 not available - use plotly::add_markers() instead")
            return(list(type = "geom_fallback"))
          }
          env$geom_ribbon <- function(...) {
            warning("ggplot2 not available - use plotly::add_ribbons() instead")
            return(list(type = "geom_fallback"))
          }
          env$aes <- function(...) {
            warning("ggplot2 not available - define aesthetics directly in plotly")
            return(list(type = "aes_fallback"))
          }
          env$labs <- function(...) {
            warning("ggplot2 not available - use plotly::layout() instead")
            return(list(type = "labs_fallback"))
          }
          env$ggtitle <- function(...) {
            warning("ggplot2 not available - use plotly::layout(title = ...) instead")
            return(list(type = "title_fallback"))
          }
          env$xlab <- function(...) {
            warning("ggplot2 not available - use plotly::layout(xaxis = list(title = ...)) instead")
            return(list(type = "xlab_fallback"))
          }
          env$ylab <- function(...) {
            warning("ggplot2 not available - use plotly::layout(yaxis = list(title = ...)) instead")
            return(list(type = "ylab_fallback"))
          }
          env$theme_bw <- function(...) {
            warning("ggplot2 not available - use plotly styling instead")
            return(list(type = "theme_fallback"))
          }
          env$facet_wrap <- function(...) {
            warning("ggplot2 not available - create subplots with plotly::subplot() instead")
            return(list(type = "facet_fallback"))
          }
        }
        
        # Add scattermore with safe loading
        tryCatch({
          env$geom_scattermore <- scattermore::geom_scattermore
        }, error = function(e) {
          env$geom_scattermore <- function(...) {
            warning("scattermore not available - use plotly::add_markers() for large datasets")
            return(list(type = "scattermore_fallback"))
          }
        })
        
        env$str_sub <- stringr::str_sub
        env$ggplotly <- plotly::ggplotly
        env$plot_ly <- plotly::plot_ly
        env$add_trace <- plotly::add_trace
        env$add_lines <- plotly::add_lines
        env$add_markers <- plotly::add_markers
        env$add_ribbons <- plotly::add_ribbons
        env$layout <- plotly::layout
        env$renderPlot <- shiny::renderPlot # For static ggplot
        env$renderPlotly <- plotly::renderPlotly # For plotly/ggplotly
        env$renderPrint <- shiny::renderPrint # For text output
        env$renderUI <- shiny::renderUI # For flexible output
        env$datatable <- DT::datatable # For table output
        env$tagList <- shiny::tagList # For multiple UI elements
        env$div <- shiny::div # For HTML containers
        env$h3 <- shiny::h3 # For headings
        env$h4 <- shiny::h4 # For headings
        env$p <- shiny::p # For paragraphs
        env$copy <- data.table::copy # For data.table copying
        env$setDT <- data.table::setDT # For converting to data.table
        
        # Add code sharing functions
        env$get_plotter_code <- get_plotter_code
        env$get_importer_code <- get_importer_code
        env$main_session_input <- main_session_input
        
        # Add modular template variables for the combined static template to use
        env$ggplot_data_processing_template <- ggplot_data_processing_template
        env$ggplot_base_setup_template <- ggplot_base_setup_template
        env$ggplot_themes_styling_template <- ggplot_themes_styling_template
        env$ggplot_statistical_overlays_template <- ggplot_statistical_overlays_template
        env$ggplot_grid_axes_template <- ggplot_grid_axes_template
        env$ggplot_faceting_final_template <- ggplot_faceting_final_template
        
        # IMPORTANT: Override with user's ace editor inputs when available
        # This allows users to edit the modular templates in real-time
        if (!is.null(r_code_plot_data_processing_reactive()) && nzchar(r_code_plot_data_processing_reactive())) {
          env$ggplot_data_processing_template <- r_code_plot_data_processing_reactive()
        }
        if (!is.null(r_code_plot_base_setup_reactive()) && nzchar(r_code_plot_base_setup_reactive())) {
          env$ggplot_base_setup_template <- r_code_plot_base_setup_reactive()
        }
        if (!is.null(r_code_plot_themes_styling_reactive()) && nzchar(r_code_plot_themes_styling_reactive())) {
          env$ggplot_themes_styling_template <- r_code_plot_themes_styling_reactive()
        }
        if (!is.null(r_code_plot_statistical_overlays_reactive()) && nzchar(r_code_plot_statistical_overlays_reactive())) {
          env$ggplot_statistical_overlays_template <- r_code_plot_statistical_overlays_reactive()
        }
        if (!is.null(r_code_plot_grid_axes_reactive()) && nzchar(r_code_plot_grid_axes_reactive())) {
          env$ggplot_grid_axes_template <- r_code_plot_grid_axes_reactive()
        }
        if (!is.null(r_code_plot_faceting_final_reactive()) && nzchar(r_code_plot_faceting_final_reactive())) {
          env$ggplot_faceting_final_template <- r_code_plot_faceting_final_reactive()
        }
        
        # Combine all the code sections
        combined_code <- paste(
          "# Text code section:",
          paste("text_code <- '", gsub("'", "\\\\'", r_code_plot_text_reactive()), "'", sep=""),
          "",
          "# Static code section:",
          paste("static_code <- '", gsub("'", "\\\\'", r_code_plot_static_reactive()), "'", sep=""),
          "",
          "# Interactive code section:",
          paste("interactive_code <- '", gsub("'", "\\\\'", r_code_plot_interactive_reactive()), "'", sep=""),
          "",
          "# Table code section:",
          paste("table_code <- '", gsub("'", "\\\\'", r_code_plot_table_reactive()), "'", sep=""),
          "",
          "# Final conditional code:",
          r_code_plot_final_reactive(),
          sep = "\n"
        )
        
        plot_result <- eval(parse(text = combined_code), envir = env)
        plot_object_reactive(plot_result)
        showNotification("Plot generated successfully.", type = "message")
      })
    })

    # Flexible plot output using renderUI
    output$plot_output <- renderUI({
      req(plot_object_reactive())
      
      plot_obj <- plot_object_reactive()
      
      # Handle different types of output flexibly
      if (inherits(plot_obj, "htmlwidget")) {
        # Plotly or other HTML widgets
        plot_obj
      } else if (inherits(plot_obj, "ggplot")) {
        # ggplot objects - render as plot
        renderPlot({
          plot_obj
        }, height = 600)
      } else if (inherits(plot_obj, "datatables")) {
        # DataTable objects
        plot_obj
      } else if (is.list(plot_obj) && all(sapply(plot_obj, function(x) inherits(x, "shiny.tag") || inherits(x, "shiny.tag.list")))) {
        # List of UI elements
        tagList(plot_obj)
      } else if (inherits(plot_obj, "shiny.render.function")) {
        # Already a render function (renderPrint, etc.)
        plot_obj
      } else {
        # Fallback: try to render as print output
        renderPrint({
          plot_obj
        })
      }
    })

    # Download handler for plots
    output$download_output <- downloadHandler(
      filename = function() {
        plot_name <- if (!is.null(input$plot_name) && nzchar(input$plot_name)) {
          gsub("[^a-zA-Z0-9_.-]", "_", input$plot_name)
        } else {
          id
        }
        
        if (input$plot_type == "dynamic") {
          paste0(plot_name, "_", Sys.Date(), ".html")
        } else if (input$plot_type == "static") {
          paste0(plot_name, "_", Sys.Date(), ".png")
        } else {
          paste0(plot_name, "_", Sys.Date(), ".txt")
        }
      },
      content = function(file) {
        req(plot_object_reactive())
        
        plot_obj <- plot_object_reactive()
        
        if (input$plot_type == "dynamic" && inherits(plot_obj, "htmlwidget")) {
          htmlwidgets::saveWidget(plot_obj, file, selfcontained = TRUE)
        } else if (input$plot_type == "static" && inherits(plot_obj, "ggplot")) {
          ggsave(file, plot_obj, device = "png", width = 12, height = 8, dpi = 300)
        } else {
          # Fallback: save as text
          writeLines(capture.output(print(plot_obj)), file)
        }
      }
    )

    # Return the plot reactive and rename trigger for parent access
    list(
      plot = reactive(plot_object_reactive()),
      plot_rename_trigger = plot_rename_trigger
    )
  })
}