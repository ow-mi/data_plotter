server_data_combiner <- function(id, list_of_importer_df_reactives) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for filtering
    filter_applied <- reactiveVal(FALSE)
    filtered_data <- reactiveVal(NULL)
    
    combined_data_from_all_importers <- reactive({
      
      get_valid_df <- function(df_reactive_fn) {
        if (is.function(df_reactive_fn)) {
          df <- df_reactive_fn() # Call the reactive function
          if (!is.null(df) && (is.data.frame(df) || is.data.table(df)) && nrow(df) > 0) {
            return(as.data.table(df)) # Ensure it's a data.table
          }
        }
        return(NULL)
      }

      # Get the current list of reactive functions
      # list_of_importer_df_reactives can be either a reactive or a static list
      current_reactives <- if (is.function(list_of_importer_df_reactives)) {
        list_of_importer_df_reactives() # Call the reactive to get the list
      } else {
        list_of_importer_df_reactives # Use the static list directly
      }
      
      # Handle empty list case
      if (length(current_reactives) == 0) {
        return(NULL)
      }

      # Evaluate each reactive in the list
      evaluated_dfs_list <- lapply(current_reactives, get_valid_df)
      
      # Assign names based on the position in the original list (e.g., "importer_1", "importer_2")
      # These names will be used for the 'idcol' if they are preserved through Filter.
      # Use existing names if available, otherwise create default names
      if (is.null(names(current_reactives))) {
        names(evaluated_dfs_list) <- paste0("importer_", seq_along(evaluated_dfs_list))
      } else {
        names(evaluated_dfs_list) <- names(current_reactives)
      }
      
      # Filter out any NULLs that resulted from importers with no data or errors
      valid_dfs_to_combine <- Filter(Negate(is.null), evaluated_dfs_list)

      if (length(valid_dfs_to_combine) > 0) {
        # rbindlist will use the names of the list elements for the idcol
        tryCatch({
          # Ensure all data.tables have consistent column types
          valid_dfs_to_combine <- lapply(valid_dfs_to_combine, function(dt) {
            # Convert to data.table if not already
            dt <- as.data.table(dt)
            
            # Ensure consistent column types for common columns
            if ("timestamp" %in% names(dt)) {
              dt$timestamp <- as.POSIXct(dt$timestamp)
            }
            if ("value" %in% names(dt)) {
              dt$value <- as.numeric(dt$value)
            }
            if ("series" %in% names(dt)) {
              dt$series <- as.character(dt$series)
            }
            if ("file_name_source" %in% names(dt)) {
              dt$file_name_source <- as.character(dt$file_name_source)
            }
            
            return(dt)
          })
          
          combined_df <- data.table::rbindlist(
            valid_dfs_to_combine, 
            use.names = TRUE, 
            fill = TRUE, 
            idcol = "source_importer_id"
          )
          
          # Apply filters if they are active
          if (filter_applied()) {
            return(filtered_data())
          } else {
            return(combined_df)
          }
          
        }, error = function(e) {
          warning("Error in rbindlist: ", e$message, ". Trying with ignore.attr=TRUE")
          # Fallback: try with ignore.attr=TRUE to handle class mismatches
          tryCatch({
            combined_df <- data.table::rbindlist(
              valid_dfs_to_combine, 
              use.names = TRUE, 
              fill = TRUE, 
              idcol = "source_importer_id",
              ignore.attr = TRUE  # This handles class attribute mismatches
            )
            
            if (filter_applied()) {
              return(filtered_data())
            } else {
              return(combined_df)
            }
          }, error = function(e2) {
            warning("Even fallback rbindlist failed: ", e2$message)
            return(NULL)
          })
        })
      } else {
        NULL # Or return an empty data.table: data.table()
      }
    })
    
    # Update source filter checkboxes when combined data changes
    observe({
      base_data <- combined_data_from_all_importers()
      
      if (!is.null(base_data) && nrow(base_data) > 0 && 'source_importer_id' %in% names(base_data)) {
        # Get unique source IDs
        unique_sources <- unique(base_data$source_importer_id)
        
        if (length(unique_sources) > 0) {
          # Create mapping from module IDs to friendly names
          source_labels <- sapply(unique_sources, function(source_id) {
            # Convert data_import_module_1 to "Data Import 1"
            if (grepl("^data_import_module_", source_id)) {
              tab_number <- gsub("data_import_module_", "", source_id)
              paste("Data Import", tab_number)
            } else if (grepl("^importer_", source_id)) {
              tab_number <- gsub("importer_", "", source_id)
              paste("Importer", tab_number)
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
    
    # Apply filters to combined data
    observeEvent(input$apply_combiner_filters, {
      base_data <- combined_data_from_all_importers()
      
      if (is.null(base_data)) {
        showNotification("No data available to filter", type = "warning")
        return()
      }
      
      filtered_df <- data.table::copy(base_data)
      
      tryCatch({
        # Apply source filtering using checkboxes
        if ('source_importer_id' %in% names(filtered_df)) {
          # Get all unique sources
          unique_sources <- unique(filtered_df$source_importer_id)
          
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
            filtered_df <- filtered_df[source_importer_id %in% selected_sources]
          } else {
            # If no sources selected, show warning and use all data
            showNotification("No sources selected - using all data", type = "warning")
          }
        }
        
        # Apply series filtering - Include Series
        if (!is.null(input$combiner_filter_in_series) && input$combiner_filter_in_series != "" && "series" %in% names(filtered_df)) {
          filtered_df <- filter_in(filtered_df, 'series', input$combiner_filter_in_series)
        }
        
        # Apply series filtering - Exclude Series
        if (!is.null(input$combiner_filter_out_series) && input$combiner_filter_out_series != "" && "series" %in% names(filtered_df)) {
          filtered_df <- filter_out(filtered_df, 'series', input$combiner_filter_out_series)
        }
        
        # Apply file filtering - Include Files
        if (!is.null(input$combiner_filter_in_files) && input$combiner_filter_in_files != "" && "file_name_source" %in% names(filtered_df)) {
          filtered_df <- filter_in(filtered_df, 'file_name_source', input$combiner_filter_in_files)
        }
        
        # Apply file filtering - Exclude Files
        if (!is.null(input$combiner_filter_out_files) && input$combiner_filter_out_files != "" && "file_name_source" %in% names(filtered_df)) {
          filtered_df <- filter_out(filtered_df, 'file_name_source', input$combiner_filter_out_files)
        }
        
        # Apply timestamp filtering
        if ('timestamp' %in% names(filtered_df)) {
          # Start time filtering
          if (!is.null(input$combiner_filter_start_enabled) && input$combiner_filter_start_enabled) {
            if (!is.null(input$combiner_filter_start_date) && !is.null(input$combiner_filter_start_time)) {
              start_datetime <- as.POSIXct(paste(
                as.character(input$combiner_filter_start_date), 
                format(input$combiner_filter_start_time, '%H:%M:%S')
              ))
              
              if (!is.na(start_datetime)) {
                filtered_df <- filtered_df[timestamp >= start_datetime]
              }
            }
          }
          
          # End time filtering
          if (!is.null(input$combiner_filter_end_enabled) && input$combiner_filter_end_enabled) {
            if (!is.null(input$combiner_filter_end_date) && !is.null(input$combiner_filter_end_time)) {
              end_datetime <- as.POSIXct(paste(
                as.character(input$combiner_filter_end_date), 
                format(input$combiner_filter_end_time, '%H:%M:%S')
              ))
              
              if (!is.na(end_datetime)) {
                filtered_df <- filtered_df[timestamp <= end_datetime]
              }
            }
          }
        }
        
        filtered_data(filtered_df)
        filter_applied(TRUE)
        
        showNotification(paste("Filters applied. Showing", nrow(filtered_df), "of", nrow(base_data), "rows"), type = "message")
        
      }, error = function(e) {
        showNotification(paste("Filter error:", e$message), type = "error")
        return()
      })
    })
    
    # Clear filters
    observeEvent(input$clear_combiner_filters, {
      filter_applied(FALSE)
      filtered_data(NULL)
      
      # Clear input fields
      updateTextInput(session, "combiner_filter_in_series", value = "")
      updateTextInput(session, "combiner_filter_out_series", value = "")
      updateTextInput(session, "combiner_filter_in_files", value = "")
      updateTextInput(session, "combiner_filter_out_files", value = "")
      
      # Clear timestamp filtering
      updateCheckboxInput(session, "combiner_filter_start_enabled", value = FALSE)
      updateCheckboxInput(session, "combiner_filter_end_enabled", value = FALSE)
      updateDateInput(session, "combiner_filter_start_date", value = Sys.Date())
      updateDateInput(session, "combiner_filter_end_date", value = Sys.Date())
      
      # Reset all source checkboxes to checked
      base_data <- combined_data_from_all_importers()
      if (!is.null(base_data) && 'source_importer_id' %in% names(base_data)) {
        unique_sources <- unique(base_data$source_importer_id)
        for (source_id in unique_sources) {
          checkbox_id <- paste0("source_", source_id)
          updateCheckboxInput(session, checkbox_id, value = TRUE)
        }
      }
      
      showNotification("Filters cleared - showing all data", type = "message")
    })
    
    # Clear all data
    observeEvent(input$clear_combined_data, {
      # This is a more complex operation that would need to clear data from all importers
      # For now, we'll show a confirmation and clear filters
      showModal(modalDialog(
        title = "Clear All Data",
        "This will clear all processed data from all import modules. Are you sure?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_clear_data", "Yes, Clear All", class = "btn-danger")
        )
      ))
    })
    
    # Status display
    output$combiner_status <- renderText({
      data <- combined_data_from_all_importers()
      
      if (is.null(data)) {
        "Status: No data available"
      } else {
        total_rows <- nrow(data)
        unique_sources <- length(unique(data$source_importer_id))
        unique_files <- if ("file_name_source" %in% names(data)) {
          length(unique(data$file_name_source))
        } else {
          "N/A"
        }
        
        filter_status <- if (filter_applied()) "Filtered" else "All data"
        
        paste0(
          "Status: ", filter_status, "\n",
          "Rows: ", format(total_rows, big.mark = ","), "\n",
          "Sources: ", unique_sources, "\n",
          "Files: ", unique_files
        )
      }
    })

    server_data_table_display(
      "combined_data_sample",
      reactive(combined_data_from_all_importers())
    )
    server_data_table_display(
      "combined_data_summary",
      reactive(combined_data_from_all_importers())
    )
    
    # Additional data table displays for new tabs
    server_data_table_display(
      "combined_data_file_info",
      reactive(combined_data_from_all_importers())
    )
    server_data_table_display(
      "combined_data_quality",
      reactive(combined_data_from_all_importers())
    )
    server_data_table_display(
      "combined_data_columns",
      reactive(combined_data_from_all_importers())
    )
    server_data_table_display(
      "combined_data_log",
      reactive(combined_data_from_all_importers())
    )

    list(
      df = reactive(combined_data_from_all_importers())
    )
  })
}