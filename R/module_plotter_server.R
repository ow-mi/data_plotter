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

    # --- Plot Rendering Logic ---
    plot_object_reactive <- reactiveVal(NULL)

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
      cat("=== PLOT RENDER CLICKED ===\n")
      cat("Module ID:", id, "\n")
      
      # Use spsComps::shinyCatch for Shiny-specific error handling
      spsComps::shinyCatch(
        tryCatch({
        req(df_plot_data_processed()) # Requires data processed for *this* plot
        
        # Use withProgress for built-in progress indicator
        withProgress(message = 'Generating plot...', value = 0, {
          

          
          incProgress(0.1, detail = "Setting up environment...")
          
          # Environment for plot generation
          env <- new.env(parent = .GlobalEnv)
          env$df <- df_plot_data_processed()
          env$input <- input # Expose module's input for titles, labels, etc.
          
          # Add simple progress function to environment
          env$incProgress <- function(amount = 0, detail = NULL) {
            incProgress(amount, detail = detail)
          }
        
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
        env$string_eval <- string_eval  # For dynamic string evaluation in captions
        
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
        
          incProgress(0.2, detail = "Preparing plot templates...")
          

          
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
          
          incProgress(0.3, detail = "Executing plot code...")
          
          # Execute plot generation
          plot_result <- eval(parse(text = combined_code), envir = env)
          
          incProgress(0.4, detail = "Finalizing plot...")
          
          # Update reactive with result
          if (!is.null(plot_result)) {
            plot_object_reactive(plot_result)
            incProgress(0.5, detail = "Plot complete!")
            showNotification("Plot generated successfully.", type = "message")
          }
          
        }) # End withProgress
      })
      ) # Close spsComps::shinyCatch
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
        # ggplot objects - render as plot with dynamic height
        renderPlot({
          plot_obj
        }, height = "auto", width = "auto")
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

    # Preset size observer
    observeEvent(input$download_preset, {
      if (!is.null(input$download_preset) && input$download_preset != "custom") {
        preset_settings <- switch(input$download_preset,
          "presentation" = list(width = 16, height = 9, dpi = 300),  # 16:9 aspect ratio
          "publication" = list(width = 7, height = 5, dpi = 600),   # Standard journal size
          "poster" = list(width = 12, height = 8, dpi = 300),       # Poster size
          "web" = list(width = 10, height = 6, dpi = 150),          # Web optimized
          list(width = 12, height = 8, dpi = 300) # Default
        )
        
        updateNumericInput(session, "download_width", value = preset_settings$width)
        updateNumericInput(session, "download_height", value = preset_settings$height)
        updateSelectInput(session, "download_dpi", selected = preset_settings$dpi)
      }
    })
    
    # Download handler for plots with enhanced options
    output$download_output <- downloadHandler(
      filename = function() {
        req(plot_object_reactive())
        
        plot_name <- if (!is.null(input$plot_name) && nzchar(input$plot_name)) {
          gsub("[^a-zA-Z0-9_.-]", "_", input$plot_name)
        } else {
          id
        }
        
        # Determine format
        format <- if (!is.null(input$download_format) && input$download_format != "auto") {
          input$download_format
        } else {
          # Auto-detect based on plot type
          plot_obj <- plot_object_reactive()
          if (inherits(plot_obj, "htmlwidget")) {
            "html"
          } else if (inherits(plot_obj, "ggplot")) {
            "png"
          } else if (inherits(plot_obj, "datatables")) {
            "html"
          } else {
            "txt"
          }
        }
        
        # Add dimensions to filename for static formats
        size_suffix <- if (format %in% c("png", "jpeg", "pdf", "svg")) {
          width <- if (!is.null(input$download_width)) input$download_width else 12
          height <- if (!is.null(input$download_height)) input$download_height else 8
          dpi <- if (!is.null(input$download_dpi)) input$download_dpi else 300
          paste0("_", width, "x", height, "_", dpi, "dpi")
        } else {
          ""
        }
        
        paste0(plot_name, size_suffix, "_", Sys.Date(), ".", format)
      },
      content = function(file) {
        req(plot_object_reactive())
        
        plot_obj <- plot_object_reactive()
        
        # Get download settings
        format <- if (!is.null(input$download_format) && input$download_format != "auto") {
          input$download_format
        } else {
          # Auto-detect
          if (inherits(plot_obj, "htmlwidget")) "html"
          else if (inherits(plot_obj, "ggplot")) "png"
          else if (inherits(plot_obj, "datatables")) "html"
          else "txt"
        }
        
        width <- if (!is.null(input$download_width)) input$download_width else 12
        height <- if (!is.null(input$download_height)) input$download_height else 8
        dpi <- if (!is.null(input$download_dpi)) as.numeric(input$download_dpi) else 300
        
        # Save based on format and plot type
        tryCatch({
          if (format == "html" && inherits(plot_obj, "htmlwidget")) {
            # Interactive plots as HTML
            htmlwidgets::saveWidget(plot_obj, file, selfcontained = TRUE)
            
          } else if (format == "html" && inherits(plot_obj, "datatables")) {
            # DataTable as HTML
            htmlwidgets::saveWidget(plot_obj, file, selfcontained = TRUE)
            
          } else if (format %in% c("png", "jpeg", "pdf", "svg") && inherits(plot_obj, "ggplot")) {
            # Static plots in various formats
            ggplot2::ggsave(
              filename = file, 
              plot = plot_obj, 
              device = format,
              width = width, 
              height = height, 
              dpi = dpi,
              bg = "white" # Ensure white background
            )
            
          } else if (format == "json") {
            # Extract data from plot
            if (inherits(plot_obj, "ggplot")) {
              # Get data from ggplot
              plot_data <- plot_obj$data
              if (!is.null(plot_data)) {
                jsonlite::write_json(plot_data, file, pretty = TRUE)
              } else {
                writeLines('{"error": "No data available in plot object"}', file)
              }
            } else if (inherits(plot_obj, "htmlwidget")) {
              # Try to extract data from plotly
              if ("plotly" %in% class(plot_obj)) {
                plot_data <- plot_obj$x$data
                jsonlite::write_json(plot_data, file, pretty = TRUE)
              } else {
                writeLines('{"error": "Data extraction not supported for this widget type"}', file)
              }
            } else {
              writeLines('{"error": "JSON export not supported for this plot type"}', file)
            }
            
          } else if (format == "html" && inherits(plot_obj, "ggplot")) {
            # Convert ggplot to interactive HTML via plotly
            if (requireNamespace("plotly", quietly = TRUE)) {
              interactive_plot <- plotly::ggplotly(plot_obj)
              htmlwidgets::saveWidget(interactive_plot, file, selfcontained = TRUE)
            } else {
              stop("plotly package required to save ggplot as HTML")
            }
            
          } else {
            # Fallback: save as text
            writeLines(capture.output(print(plot_obj)), file)
          }
          
          # Show success notification
          showNotification(paste("Plot downloaded as", format, "format"), type = "message")
          
        }, error = function(e) {
          showNotification(paste("Download error:", e$message), type = "error")
          # Fallback: save as text
          writeLines(capture.output(print(plot_obj)), file)
        })
      }
    )

    # Return the plot reactive and rename trigger for parent access
    list(
      plot = reactive(plot_object_reactive()),
      plot_rename_trigger = plot_rename_trigger
    )
  })
}