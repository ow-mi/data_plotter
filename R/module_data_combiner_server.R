server_data_combiner <- function(id, list_of_importer_df_reactives, main_session_input = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for processed data
    processed_data <- reactiveVal(NULL)
    processing_status <- reactiveVal("No processing applied")
    
    # Helper function to create processing environment
    create_processing_environment <- function(raw_data, input_obj, main_session_input) {
      env <- new.env(parent = globalenv())
      env$df <- raw_data
      env$input <- input_obj
      env$data.table <- data.table
      env$setDT <- data.table::setDT
      env$copy <- data.table::copy
      env$filter_in <- filter_in
      env$filter_out <- filter_out
      env$rname <- rname
      env$string_eval <- string_eval
      if (!is.null(main_session_input)) {
        env$main_session_input <- main_session_input
      }
      return(env)
    }
    
    # Helper function to parse batch filters and expand code blocks
    parse_batch_filters <- function(filter_string, df_for_eval) {
      if (is.null(filter_string) || trimws(filter_string) == "") {
        return(list(""))
      }
      
      # Split by semicolon first to get individual filter runs
      filter_runs <- strsplit(filter_string, ";")[[1]]
      filter_runs <- trimws(filter_runs)
      filter_runs <- filter_runs[filter_runs != ""]
      
      if (length(filter_runs) <= 1) {
        # Check if we have code blocks that need expansion
        return(expand_code_blocks(filter_string, df_for_eval))
      }
      
      # Multiple runs - expand each one and create combinations
      expanded_runs <- list()
      for (run in filter_runs) {
        expanded <- expand_code_blocks(run, df_for_eval)
        expanded_runs <- c(expanded_runs, expanded)
      }
      
      return(expanded_runs)
    }
    
    # Helper function to expand code blocks in filter strings
    expand_code_blocks <- function(filter_string, df_for_eval) {
      if (is.null(filter_string) || trimws(filter_string) == "") {
        return(list(""))
      }
      
      # Create evaluation environment
      eval_env <- new.env(parent = globalenv())
      eval_env$df <- df_for_eval
      
      # Find all quoted code blocks
      quoted_pattern <- '["\']([^"\']*)["\']'
      matches <- gregexpr(quoted_pattern, filter_string, perl = TRUE)
      match_data <- regmatches(filter_string, matches)[[1]]
      
      if (length(match_data) == 0) {
        # No code blocks, return as is
        return(list(filter_string))
      }
      
      # Process each quoted code block and expand
      expanded_combinations <- list(filter_string)
      
      for (quoted_code in match_data) {
        # Remove quotes to get the actual code
        code <- gsub('^["\']|["\']$', '', quoted_code)
        
        tryCatch({
          # Evaluate the code
          eval_result <- eval(parse(text = code), envir = eval_env)
          
          if (is.vector(eval_result) && length(eval_result) > 1) {
            # Expand combinations for multiple values
            new_combinations <- list()
            for (combination in expanded_combinations) {
              for (value in eval_result) {
                new_combo <- gsub(pattern = fixed(quoted_code), 
                                replacement = as.character(value), 
                                x = combination, fixed = TRUE)
                new_combinations <- c(new_combinations, list(new_combo))
              }
            }
            expanded_combinations <- new_combinations
          } else {
            # Single value, just replace
            for (i in seq_along(expanded_combinations)) {
              expanded_combinations[[i]] <- gsub(pattern = fixed(quoted_code), 
                                                replacement = as.character(eval_result), 
                                                x = expanded_combinations[[i]], fixed = TRUE)
            }
          }
          
        }, error = function(e) {
          # If evaluation fails, leave the code block as is
          warning(paste("Failed to evaluate code block:", code, "Error:", e$message))
        })
      }
      
      return(expanded_combinations)
    }
    
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
      
      # Debug: Log information about available reactives
      cat("DEBUG: current_reactives length:", length(current_reactives), "\n")
      if (length(current_reactives) > 0) {
        for (i in seq_along(current_reactives)) {
          reactive_fn <- current_reactives[[i]]
          if (is.function(reactive_fn)) {
            df_result <- reactive_fn()
            cat("DEBUG: Reactive", i, "- is.null:", is.null(df_result), 
                "- nrow:", if (!is.null(df_result)) nrow(df_result) else "N/A", "\n")
          } else {
            cat("DEBUG: Reactive", i, "- not a function\n")
          }
        }
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
          
          # Return the combined data frame
          return(combined_df)
          
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
            
            return(combined_df)
          }, error = function(e2) {
            warning("Even fallback rbindlist failed: ", e2$message)
            return(NULL)
          })
        })
      } else {
        NULL # Or return an empty data.table: data.table()
      }
    })
    
    # Process data when "Apply Filters" button is pressed
    observeEvent(input$apply_combiner_filters, {
      # Get the raw combined data
      raw_data <- combined_data_from_all_importers()
      
      # Get the processing code from ace editor
      processing_code <- input$r_code_combined_data_processing
      
      # Get the custom filter for batch processing
      custom_filter <- input$custom_filter
      
      # Debug information
      if (is.null(raw_data)) {
        showNotification("Debug: raw_data is NULL. Check if importers have processed data.", type = "warning", duration = 10)
        processing_status("No data available - raw_data is NULL")
        return()
      } else if (nrow(raw_data) == 0) {
        showNotification(paste("Debug: raw_data exists but has 0 rows. Columns:", paste(names(raw_data), collapse = ", ")), type = "warning", duration = 10)
        processing_status("No data available - raw_data has 0 rows")
        return()
      } else {
        showNotification(paste("Debug: Found", nrow(raw_data), "rows,", ncol(raw_data), "columns"), type = "message", duration = 5)
      }
      
      if (is.null(processing_code) || trimws(processing_code) == "") {
        showNotification("No processing code provided", type = "warning")
        processing_status("No code provided")
        return()
      }
      
      # Show processing notification
      showNotification("Processing data with ace editor code...", type = "message", duration = 3)
      processing_status("Processing...")
      
      tryCatch({
        # Handle batch runs by splitting semicolon-separated entries
        filter_runs <- parse_batch_filters(custom_filter, raw_data)
        
        if (length(filter_runs) <= 1) {
          # Single run - process normally
          env <- create_processing_environment(raw_data, input, main_session_input)
          processed_df <- eval(parse(text = processing_code), envir = env)
          
          if (!is.null(processed_df) && (is.data.frame(processed_df) || is.data.table(processed_df))) {
            setDT(processed_df)
            processed_data(processed_df)
            processing_status(paste("Processed successfully -", format(nrow(processed_df), big.mark = ","), "rows"))
            showNotification(paste("Processing complete:", format(nrow(processed_df), big.mark = ","), "rows"), type = "message")
          } else {
            warning("Processing code did not return a valid data frame")
            processed_data(raw_data)
            processing_status("Code returned invalid result - using raw data")
            showNotification("Processing code did not return valid data. Using raw data.", type = "warning")
          }
        } else {
          # Multiple runs - batch processing
          batch_results <- list()
          
          for (i in seq_along(filter_runs)) {
            # Create modified input for this run
            modified_input <- as.list(input)
            modified_input$custom_filter <- filter_runs[[i]]
            
            env <- create_processing_environment(raw_data, modified_input, main_session_input)
            run_result <- eval(parse(text = processing_code), envir = env)
            
            if (!is.null(run_result) && (is.data.frame(run_result) || is.data.table(run_result))) {
              setDT(run_result)
              run_result[, batch_run := i]  # Add batch run identifier
              batch_results[[i]] <- run_result
            }
          }
          
          # Combine all batch results
          if (length(batch_results) > 0) {
            combined_batch_df <- rbindlist(batch_results, use.names = TRUE, fill = TRUE)
            processed_data(combined_batch_df)
            processing_status(paste("Batch processed successfully -", length(filter_runs), "runs,", format(nrow(combined_batch_df), big.mark = ","), "total rows"))
            showNotification(paste("Batch processing complete:", length(filter_runs), "runs,", format(nrow(combined_batch_df), big.mark = ","), "rows"), type = "message")
          } else {
            processed_data(raw_data)
            processing_status("Batch processing failed - using raw data")
            showNotification("Batch processing failed. Using raw data.", type = "warning")
          }
        }
        
      }, error = function(e) {
        warning("Error in processing code: ", e$message)
        processed_data(raw_data)
        processing_status(paste("Error:", e$message))
        showNotification(paste("Processing error:", e$message), type = "error")
      })
    })
    
    # Manual data check button
    observeEvent(input$refresh_data_check, {
      # Force refresh of the reactive to see debug output
      raw_data <- combined_data_from_all_importers()
      
      # Get current reactives info
      current_reactives <- if (is.function(list_of_importer_df_reactives)) {
        list_of_importer_df_reactives()
      } else {
        list_of_importer_df_reactives
      }
      
      # Show detailed info
      if (length(current_reactives) == 0) {
        showNotification("No importer modules found. Create Data Import tabs first.", type = "warning", duration = 10)
      } else {
        msg <- paste("Found", length(current_reactives), "importer(s).")
        if (is.null(raw_data)) {
          msg <- paste(msg, "But combined data is NULL. Check console for debug info.")
        } else if (nrow(raw_data) == 0) {
          msg <- paste(msg, "Combined data has 0 rows.")
        } else {
          msg <- paste(msg, "Combined data has", nrow(raw_data), "rows.")
        }
        showNotification(msg, type = "message", duration = 10)
      }
    })
    
    # Reactive to return the current data (processed if available, otherwise raw)
    current_display_data <- reactive({
      processed <- processed_data()
      if (!is.null(processed)) {
        return(processed)
      } else {
        return(combined_data_from_all_importers())
      }
    })
    

    

    

    
    # Status display
    output$combiner_status <- renderText({
      raw_data <- combined_data_from_all_importers()
      processed <- processed_data()
      status <- processing_status()
      
      if (is.null(raw_data)) {
        paste0(
          "Status: ", status, "\n",
          "Raw data: NULL\n",
          "Tip: Process data in Data Import tabs first"
        )
      } else if (nrow(raw_data) == 0) {
        paste0(
          "Status: ", status, "\n",
          "Raw data: 0 rows (", ncol(raw_data), " columns)\n",
          "Tip: Process data in Data Import tabs first"
        )
      } else {
        raw_rows <- nrow(raw_data)
        processed_rows <- if (!is.null(processed)) nrow(processed) else 0
        
        unique_sources <- length(unique(raw_data$source_importer_id))
        unique_files <- if ("file_name_source" %in% names(raw_data)) {
          length(unique(raw_data$file_name_source))
        } else {
          "N/A"
        }
        
        paste0(
          "Status: ", status, "\n",
          "Raw rows: ", format(raw_rows, big.mark = ","), "\n",
          if (processed_rows > 0) paste0("Processed rows: ", format(processed_rows, big.mark = ","), "\n") else "",
          "Sources: ", unique_sources, "\n",
          "Files: ", unique_files
        )
      }
    })

    server_data_table_display(
      "combined_data_sample",
      current_display_data
    )
    server_data_table_display(
      "combined_data_summary",
      current_display_data
    )
    
    # Additional data table displays for new tabs
    server_data_table_display(
      "combined_data_file_info",
      current_display_data
    )
    server_data_table_display(
      "combined_data_quality",
      current_display_data
    )
    server_data_table_display(
      "combined_data_columns",
      current_display_data
    )
    server_data_table_display(
      "combined_data_log",
      current_display_data
    )

    list(
      df = current_display_data
    )
  })
}