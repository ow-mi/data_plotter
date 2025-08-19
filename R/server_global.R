#' Data Plotter Server Function
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @export
server_global <- function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024^2) # 1GB
  
  # Initialize dark mode - only call this when session is available
  tryCatch({
    toggle_dark_mode(mode = "dark")
  }, error = function(e) {
    # Ignore errors if called outside of Shiny session
    cat("Note: toggle_dark_mode skipped (not in Shiny session)\n")
  })

  # JSON-Based Template Save/Load Logic
  safe_input_value <- function(val) {
    if (is.null(val)) {
      return(NULL)
    } else if (is.list(val) && length(val) > 1) {
      return(as.character(val))
    } else if (inherits(val, c("Date", "POSIXct", "POSIXlt"))) {
      return(as.character(val))
    } else if (is.logical(val) || is.numeric(val)) {
      return(val)
    } else {
      return(as.character(val))
    }
  }

  # Initialize download handler when session is available
  tryCatch({
    output$download_template <- downloadHandler(
      filename = function() {
        req(input$template_file_name)
        paste0(tools::file_path_sans_ext(input$template_file_name), ".json")
      },
      content = function(file) {
        tryCatch({
          all_inputs_list <- reactiveValuesToList(input)
          
          cat("=== JSON TEMPLATE SAVE ===\n")
          cat("Total inputs captured:", length(all_inputs_list), "\n")
          
          # Get current module counts
          current_importer_count <- isolate(importer_counter())
          current_plotter_count <- isolate(plotter_counter())
          
          # Split inputs by category
          general_inputs <- list()
          importer_inputs <- list()
          plotter_inputs <- list()
          ace_inputs <- list()
          
          for (input_name in names(all_inputs_list)) {
            val <- safe_input_value(all_inputs_list[[input_name]])
            
            if (grepl("r_code", input_name)) {
              ace_inputs[[input_name]] <- val
            } else if (grepl("data_import_module_[0-9]+", input_name)) {
              importer_inputs[[input_name]] <- val
            } else if (grepl("plotter_[0-9]+", input_name)) {
              plotter_inputs[[input_name]] <- val
            } else {
              general_inputs[[input_name]] <- val
            }
          }
          
          # Create structured template
          template_data <- list(
            metadata = list(
              template_version = "2.0",
              created_date = as.character(Sys.time()),
              importer_count = current_importer_count,
              plotter_count = current_plotter_count,
              total_inputs = length(all_inputs_list)
            ),
            modules = list(
              importers = current_importer_count,
              plotters = current_plotter_count
            ),
            inputs = list(
              general = general_inputs,
              importers = importer_inputs,
              plotters = plotter_inputs,
              ace_editors = ace_inputs
            )
          )
          
          # Write JSON file
          jsonlite::write_json(template_data, file, pretty = TRUE, auto_unbox = TRUE)
          
          cat("Saved:", length(general_inputs), "general,", length(importer_inputs), "importer,", 
              length(plotter_inputs), "plotter,", length(ace_inputs), "ace inputs\n")
          
          showNotification(paste("Template saved:", current_importer_count, "importers,", 
                                 current_plotter_count, "plotters"), type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error saving template:", e$message), type = "error", duration = 10)
          # Create minimal JSON file to prevent download failure
          minimal_template <- list(
            metadata = list(template_version = "2.0", error = e$message),
            inputs = list()
          )
          jsonlite::write_json(minimal_template, file, pretty = TRUE, auto_unbox = TRUE)
        })
      }
    )
  }, error = function(e) {
    # Ignore errors if called outside of Shiny session
    cat("Note: downloadHandler skipped (not in Shiny session)\n")
  })

  observeEvent(input$template_upload, {
    req(input$template_upload)
    tryCatch({
      # Load JSON template
      template_data <- jsonlite::fromJSON(input$template_upload$datapath, simplifyVector = FALSE)
      
      cat("=== JSON TEMPLATE LOAD ===\n")
      cat("Template version:", if(!is.null(template_data$metadata$template_version)) template_data$metadata$template_version else "unknown", "\n")
      
      # Validate template structure
      if (is.null(template_data$metadata) || is.null(template_data$inputs)) {
        showNotification("Invalid JSON template file structure", type = "error", duration = 10)
        return()
      }
      
      # Extract module requirements
      importer_count_needed <- if(!is.null(template_data$modules$importers)) as.integer(template_data$modules$importers) else 1
      plotter_count_needed <- if(!is.null(template_data$modules$plotters)) as.integer(template_data$modules$plotters) else 0
      
      # Ensure valid counts
      importer_count_needed <- max(1, importer_count_needed)
      plotter_count_needed <- max(0, plotter_count_needed)
      
      cat("Modules needed - Importers:", importer_count_needed, "Plotters:", plotter_count_needed, "\n")
      
      # Get current counts
      current_importer_count <- isolate(importer_counter())
      current_plotter_count <- isolate(plotter_counter())
      
      cat("Current modules - Importers:", current_importer_count, "Plotters:", current_plotter_count, "\n")
      
      # STEP 1: Create required importer modules
      if (current_importer_count < importer_count_needed) {
        tabs_to_create <- importer_count_needed - current_importer_count
        cat("Creating", tabs_to_create, "additional importer tabs\n")
        
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
          
          # Initialize server module
          importer_module_output <- server_data_import(import_id, global_files)
          current_instances <- importer_instances()
          current_instances[[import_id]] <- importer_module_output
          importer_instances(current_instances)
          
          # Observe tab rename trigger from this importer module
          observeEvent(importer_module_output$tab_rename_trigger(), {
            req(importer_module_output$tab_rename_trigger())
            
            rename_data <- importer_module_output$tab_rename_trigger()
            cat("Importer tab rename triggered:", rename_data$newName, "\n")
            
            # Send message to JavaScript to update the tab
            session$sendCustomMessage("updateNavTabTitle", list(
              moduleId = rename_data$moduleId,
              newTitle = rename_data$newName,
              oldTitle = rename_data$currentTitle
            ))
          })
          
          cat("Created importer:", import_id, "\n")
        }
      }
      
      # STEP 2: Create required plotter modules
      if (current_plotter_count < plotter_count_needed) {
        tabs_to_create <- plotter_count_needed - current_plotter_count
        cat("Creating", tabs_to_create, "additional plotter tabs\n")
        
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
          
          # Initialize server module
          plotter_module_output <- server_plotter(plot_id, data_combiner$df, input)
          current_instances <- plotter_instances()
          current_instances[[plot_id]] <- plotter_module_output
          plotter_instances(current_instances)
          
          # Observe plot rename trigger from this plotter module
          observeEvent(plotter_module_output$plot_rename_trigger(), {
            req(plotter_module_output$plot_rename_trigger())
            
            rename_data <- plotter_module_output$plot_rename_trigger()
            cat("Plotter tab rename triggered:", rename_data$newName, "\n")
            
            # Send message to JavaScript to update the tab
            session$sendCustomMessage("updateNavTabTitle", list(
              moduleId = rename_data$moduleId,
              newTitle = rename_data$newName,
              oldTitle = rename_data$currentTitle
            ))
          })
          
          cat("Created plotter:", plot_id, "\n")
        }
      }
      
      # STEP 3: Apply inputs in order (modules are now ready)
      cat("Applying template inputs...\n")
      
      # Helper function to safely update inputs
      safe_update_input <- function(input_id, value) {
        if (is.null(value)) return(FALSE)
        
        tryCatch({
          current_val <- isolate(input[[input_id]])
          
          if (is.null(current_val)) {
            cat("  Input", input_id, "not found, skipping\n")
            return(FALSE)
          }
          
          # Convert value based on current input type
          if (is.logical(current_val)) {
            updateCheckboxInput(session, input_id, value = as.logical(value))
          } else if (is.numeric(current_val)) {
            updateNumericInput(session, input_id, value = as.numeric(value))
          } else if (inherits(current_val, "Date")) {
            updateDateInput(session, input_id, value = as.Date(value))
          } else if (is.character(value) && length(value) > 1) {
            # Multiple values for selectize
            updateSelectizeInput(session, input_id, selected = value)
          } else {
            # Try select first, then text
            tryCatch({
              updateSelectInput(session, input_id, selected = as.character(value))
            }, error = function(e) {
              updateTextInput(session, input_id, value = as.character(value))
            })
          }
          
          cat("  Updated", input_id, "=", paste(value, collapse=","), "\n")
          return(TRUE)
        }, error = function(e) {
          cat("  Failed to update", input_id, ":", e$message, "\n")
          return(FALSE)
        })
      }
      
      # Apply general inputs first
      if (!is.null(template_data$inputs$general)) {
        cat("Applying", length(template_data$inputs$general), "general inputs\n")
        for (input_id in names(template_data$inputs$general)) {
          safe_update_input(input_id, template_data$inputs$general[[input_id]])
        }
      }
      
      # Apply importer inputs
      if (!is.null(template_data$inputs$importers)) {
        cat("Applying", length(template_data$inputs$importers), "importer inputs\n")
        for (input_id in names(template_data$inputs$importers)) {
          safe_update_input(input_id, template_data$inputs$importers[[input_id]])
        }
      }
      
      # Apply ace editor inputs
      if (!is.null(template_data$inputs$ace_editors)) {
        cat("Applying", length(template_data$inputs$ace_editors), "ace editor inputs\n")
        for (ace_id in names(template_data$inputs$ace_editors)) {
          ace_value <- template_data$inputs$ace_editors[[ace_id]]
          if (!is.null(ace_value) && nzchar(ace_value)) {
            tryCatch({
              updateAceEditor(session, ace_id, value = as.character(ace_value))
              cat("  Updated ace editor:", ace_id, "\n")
            }, error = function(e) {
              cat("  Failed to update ace editor", ace_id, ":", e$message, "\n")
            })
          }
        }
      }
      
      # Apply plotter inputs (these should work now that modules exist)
      if (!is.null(template_data$inputs$plotters)) {
        cat("Applying", length(template_data$inputs$plotters), "plotter inputs\n")
        successful_count <- 0
        
        for (input_id in names(template_data$inputs$plotters)) {
          if (safe_update_input(input_id, template_data$inputs$plotters[[input_id]])) {
            successful_count <- successful_count + 1
          }
        }
        
        cat("Successfully applied", successful_count, "out of", length(template_data$inputs$plotters), "plotter inputs\n")
      }
      
      showNotification(paste("Template loaded successfully:", importer_count_needed, "importers,", 
                             plotter_count_needed, "plotters"), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Failed to load template:", e$message), type = "error", duration = 10)
      cat("Template load error:", e$message, "\n")
    })
  })

  # Global File Management for Input Data Tab
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
    }
    
    global_files(current_files)
    showNotification(paste("Uploaded", nrow(new_files), "individual file(s)"), type = "message")
  })
  
  # Reactive values to track batched folder upload
  folder_upload_state <- reactiveValues(
    total_files_expected = 0,
    files_processed = 0,
    files_skipped = 0,
    files_filtered = 0,
    upload_in_progress = FALSE,
    batches_received = 0,
    total_batches = 0
  )
  
  # Batched folder upload handler - receives file data from JavaScript in chunks
  observeEvent(input$global_folder_upload_files_batch, {
    req(input$global_folder_upload_files_batch)
    
    batch_data <- input$global_folder_upload_files_batch
    batch_number <- batch_data$batchNumber
    is_last_batch <- batch_data$isLastBatch
    total_batches <- batch_data$totalBatches
    
    cat("=== FOLDER UPLOAD BATCH", batch_number + 1, "of", total_batches, "RECEIVED ===\n")
    cat("Number of files in this batch:", length(batch_data$files), "\n")
    cat("Is last batch:", is_last_batch, "\n")
    
    # Initialize or update upload state
    if (batch_number == 0) {
      folder_upload_state$upload_in_progress <- TRUE
      folder_upload_state$total_batches <- total_batches
      folder_upload_state$batches_received <- 0
      cat("Starting new folder upload session\n")
    }
    
    folder_upload_state$batches_received <- folder_upload_state$batches_received + 1
    
    current_files <- global_files()
    batch_files_processed <- 0
    batch_files_skipped <- 0
    
    # Process each file in this batch (simplified, synchronous approach)
    for (file_info in batch_data$files) {
      file_name <- file_info$name
      data_url <- file_info$dataURL
      original_path <- file_info$originalPath
      
      cat("Processing folder file:", file_name, "(original:", original_path, ")\n")
      
      # Check if file already exists
      if (file_name %in% names(current_files) && !input$overwrite_files) {
        cat("File already exists, skipping:", file_name, "\n")
        batch_files_skipped <- batch_files_skipped + 1
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
        
        batch_files_processed <- batch_files_processed + 1
        cat("Successfully processed:", file_name, "\n")
        
      }, error = function(e) {
        cat("Error processing file", file_name, ":", e$message, "\n")
        showNotification(
          paste("Error processing", file_name, ":", e$message),
          type = "error"
        )
      })
    }
    
    # Update cumulative counters
    folder_upload_state$files_processed <- folder_upload_state$files_processed + batch_files_processed
    folder_upload_state$files_skipped <- folder_upload_state$files_skipped + batch_files_skipped
    
    global_files(current_files)
    
    # Show progress notification only for final batch
    if (is_last_batch) {
      # Final batch - show summary
      folder_upload_state$upload_in_progress <- FALSE
      
      summary_msg <- paste("Folder upload complete:", 
                           folder_upload_state$files_processed, "files processed")
      if (folder_upload_state$files_skipped > 0) {
        summary_msg <- paste0(summary_msg, ", ", folder_upload_state$files_skipped, 
                             " files skipped (already exist)")
      }
      
      showNotification(summary_msg, type = "message", duration = 5)
      cat("Folder upload complete:", folder_upload_state$files_processed, "processed,", 
          folder_upload_state$files_skipped, "skipped across", total_batches, "batches\n")
    }
    # No intermediate progress notifications to reduce session overhead
    
    cat("Batch", batch_number + 1, "complete:", batch_files_processed, "processed,", 
        batch_files_skipped, "skipped\n")
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
  tryCatch({
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
  }, error = function(e) {
    cat("Note: global_file_list output skipped (not in Shiny session)\n")
  })
  
  # File statistics
  tryCatch({
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
  }, error = function(e) {
    cat("Note: file_stats output skipped (not in Shiny session)\n")
  })

  # Dynamic Plotter Management
  plotter_instances <- reactiveVal(list())
  plotter_counter <- reactiveVal(0)

  # Dynamic Data Import Management
  importer_instances <- reactiveVal(list())
  importer_counter <- reactiveVal(0)
  
  # Store custom tab names for importers
  importer_tab_names <- reactiveVal(list())
  # Also store current displayed titles to help with repeated renames
  importer_current_titles <- reactiveVal(list())

  # Data Import and Combination Logic
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
  
  # Initialize data combiner module
  data_combiner <- tryCatch({
    server_data_combiner(
      "combiner", 
      list_of_df_reactives_for_combiner
    )
  }, error = function(e) {
    cat("Note: data_combiner module skipped (not in Shiny session)\n")
    # Return a dummy reactive for testing
    reactive({ NULL })
  })

  # Dynamic Data Import Creation
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
    
    # Observe tab rename trigger from this importer module
    observeEvent(importer_module_output$tab_rename_trigger(), {
      req(importer_module_output$tab_rename_trigger())
      
      rename_data <- importer_module_output$tab_rename_trigger()
      cat("Manual importer tab rename triggered:", rename_data$newName, "\n")
      
      # Send message to JavaScript to update the tab
      session$sendCustomMessage("updateNavTabTitle", list(
        moduleId = rename_data$moduleId,
        newTitle = rename_data$newName,
        oldTitle = rename_data$currentTitle
      ))
    })
    
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
      
      # Observe tab rename trigger from this default importer module
      observeEvent(importer_module_output$tab_rename_trigger(), {
        req(importer_module_output$tab_rename_trigger())
        
        rename_data <- importer_module_output$tab_rename_trigger()
        cat("Default importer tab rename triggered:", rename_data$newName, "\n")
        
        # Send message to JavaScript to update the tab
        session$sendCustomMessage("updateNavTabTitle", list(
          moduleId = rename_data$moduleId,
          newTitle = rename_data$newName,
          oldTitle = rename_data$currentTitle
        ))
      })
    }
  }, once = TRUE)

  # Dynamic Plotter Creation
  observeEvent(input$insert_plot, {
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
    
    # Observe plot rename trigger from this plotter module
    observeEvent(plotter_module_output$plot_rename_trigger(), {
      req(plotter_module_output$plot_rename_trigger())
      
      rename_data <- plotter_module_output$plot_rename_trigger()
      cat("Manual plotter tab rename triggered:", rename_data$newName, "\n")
      
      # Send message to JavaScript to update the tab
      session$sendCustomMessage("updateNavTabTitle", list(
        moduleId = rename_data$moduleId,
        newTitle = rename_data$newName,
        oldTitle = rename_data$currentTitle
      ))
    })
    
    showNotification(paste("Added plotter tab:", plot_id), type = "message")
  })

  # --- Automation Functionality ---
  
  # Automation: Upload Folder
  observeEvent(input$automation_upload_folder, {
    showNotification("Opening folder upload dialog...", type = "message")
    
    # Send custom message to JavaScript to trigger folder upload
    session$sendCustomMessage("triggerFolderUpload", list(
      message = "Automation triggered folder upload"
    ))
  })
  
  # Automation: Process All Data
  observeEvent(input$automation_process_data, {
    current_importers <- importer_instances()
    
    if (length(current_importers) == 0) {
      showNotification("No data import tabs found. Please create at least one import tab first.", type = "warning")
      return()
    }
    
    showNotification(paste("Starting batch processing for", length(current_importers), "importer(s)..."), type = "message")
    
    # Trigger data processing for each importer
    processed_count <- 0
    for (importer_id in names(current_importers)) {
      tryCatch({
        # Debug: Print importer information
        cat("DEBUG: Processing importer_id:", importer_id, "\n")
        
        # Trigger processing by setting the input value directly
        input_id <- paste0(importer_id, "-combine_data")
        cat("DEBUG: Constructed input_id:", input_id, "\n")
        
        # Try using updateActionButton instead of sendInputMessage
        session$sendCustomMessage("triggerButton", list(
          buttonId = input_id,
          timestamp = as.numeric(Sys.time())
        ))
        
        processed_count <- processed_count + 1
        cat("Triggered processing for importer:", importer_id, "\n")
      }, error = function(e) {
        cat("Error triggering processing for importer", importer_id, ":", e$message, "\n")
      })
    }
    
    showNotification(paste("Triggered processing for", processed_count, "importer(s). Check individual tabs for progress."), 
                     type = "message", duration = 5)
  })
  
  # Automation: Generate All Plots
  observeEvent(input$automation_generate_plots, {
    current_plotters <- plotter_instances()
    
    if (length(current_plotters) == 0) {
      showNotification("No plotter tabs found. Please create at least one plotter tab first.", type = "warning")
      return()
    }
    
    showNotification(paste("Starting batch plot generation for", length(current_plotters), "plotter(s)..."), type = "message")
    
    # Trigger data processing and plot generation for each plotter
    processed_count <- 0
    for (plotter_id in names(current_plotters)) {
      tryCatch({
        # Debug: Print plotter information
        cat("DEBUG: Processing plotter_id:", plotter_id, "\n")
        
        # First trigger data processing for this plotter
        data_process_id <- paste0(plotter_id, "-data_process_plot")
        cat("DEBUG: Constructed data_process_id:", data_process_id, "\n")
        
        session$sendCustomMessage("triggerButton", list(
          buttonId = data_process_id,
          timestamp = as.numeric(Sys.time())
        ))
        
        # Then trigger plot generation (with slight delay)
        plot_render_id <- paste0(plotter_id, "-plot_render")
        cat("DEBUG: Constructed plot_render_id:", plot_render_id, "\n")
        
        session$sendCustomMessage("triggerButton", list(
          buttonId = plot_render_id,
          timestamp = as.numeric(Sys.time()) + 0.1
        ))
        
        processed_count <- processed_count + 1
        cat("Triggered plot generation for plotter:", plotter_id, "\n")
      }, error = function(e) {
        cat("Error triggering plot generation for plotter", plotter_id, ":", e$message, "\n")
      })
    }
    
    showNotification(paste("Triggered plot generation for", processed_count, "plotter(s). Check individual tabs for progress."), 
                     type = "message", duration = 5)
  })
  
  # Helper & Downloader R Code Execution
  ace_server_functions("helper_input")
  observeEvent(input$helper_input, {
    output$helper_output <- renderUI({
      spsComps::shinyCatch({
        eval(
          parse(text = input$helper_input), 
          envir = new.env(parent = globalenv())
        )
      })
    })
  }, ignoreInit = FALSE, ignoreNULL = FALSE)

  ace_server_functions("downloader_input")
  observeEvent(input$downloader_input, {
    output$downloader_output <- renderUI({
      spsComps::shinyCatch({
        # Prepare the list of plot reactives for the downloader code
        plots_to_download_map <- lapply(plotter_instances(), function(instance) {
          if (!is.null(instance) && "plot" %in% names(instance) && 
              is.function(instance$plot)) {
            return(instance$plot)
          }
          return(NULL)
        })
        plots_to_download_map <- Filter(Negate(is.null), plots_to_download_map)

        # Environment for downloader code
        downloader_env <- new.env(parent = globalenv())
        downloader_env$dynamic_plots_map <- plots_to_download_map

        eval(parse(text = input$downloader_input), envir = downloader_env)
      })
    })
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
}