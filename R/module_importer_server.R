# --- Server Module for Data Importer ---
server_data_import <- function(id, global_files_reactive = NULL) { # Accept global files
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # For constructing observeEvent selectors if needed, though not used here

    # Tab renaming functionality - use reactive values instead of custom messages
    tab_rename_trigger <- reactiveVal(NULL)
    
    # Auto-apply tab name when input changes (no button needed)
    observeEvent(input$tab_name, {
      req(input$tab_name)
      new_name <- trimws(input$tab_name)
      
      cat("=== TAB NAME AUTO-APPLY ===\n")
      cat("Module ID:", id, "\n")
      cat("New name:", new_name, "\n")
      
      if (new_name == "") {
        return() # Don't apply empty names
      }
      
      # Set the reactive value to trigger parent's observer
      current_title <- paste("Import", substr(id, nchar("data_import_module_") + 1, nchar(id)))
      tab_rename_trigger(list(
        moduleId = id,
        newName = new_name,
        currentTitle = current_title,
        timestamp = Sys.time() # Ensure trigger fires even with same name
      ))
      
      cat("Tab rename trigger set automatically\n")
      showNotification(paste("Tab renamed to:", new_name), type = "message", duration = 2)
    }, ignoreInit = TRUE) # Only trigger on user changes, not initial load
    
    # Handle adding filename extraction columns
    observeEvent(input$add_extract_column, {
      req(input$extract_column_name, input$extract_pattern)
      
      column_name <- trimws(input$extract_column_name)
      pattern <- trimws(input$extract_pattern)
      
      if (column_name == "" || pattern == "") {
        showNotification("Both column name and pattern are required.", type = "warning")
        return()
      }
      
      # Check if column name already exists
      current_extractions <- filename_extractions()
      if (column_name %in% current_extractions$column_name) {
        showNotification(paste("Column", column_name, "already exists. Please use a different name."), type = "warning")
        return()
      }
      
      # Test the regex pattern
      tryCatch({
        test_string <- "Thermal performance-MWC B10 Box21-Charging1-Box data-20250331_072751362__MWCU_Offboard_Temp.csv"
        str_extract(test_string, pattern)
      }, error = function(e) {
        showNotification(paste("Invalid regex pattern:", e$message), type = "error")
        return()
      })
      
      # Add new extraction rule
      new_extraction <- data.frame(
        column_name = column_name,
        pattern = pattern,
        stringsAsFactors = FALSE
      )
      updated_extractions <- rbind(current_extractions, new_extraction)
      filename_extractions(updated_extractions)
      
      # Clear inputs
      updateTextInput(session, "extract_column_name", value = "")
      updateTextInput(session, "extract_pattern", value = "")
      
      showNotification(paste("Added column extraction:", column_name), type = "message")
    })
    
    # Display current extractions
    output$extraction_display <- renderUI({
      extractions <- filename_extractions()
      if (nrow(extractions) == 0) {
        return(div(class = "text-muted small", "No extractions defined"))
      }
      
      extract_items <- lapply(1:nrow(extractions), function(i) {
        div(class = "d-flex justify-content-between align-items-center mb-1 p-1 border rounded",
          div(class = "small",
            strong(extractions$column_name[i]), " → ", 
            code(extractions$pattern[i], style = "font-size: 0.8em;")
          ),
          actionButton(
            ns(paste0("remove_extract_", i)),
            label = "",
            icon = icon("times"),
            class = "btn-sm btn-outline-danger p-1",
            style = "font-size: 0.7em; padding: 2px 6px !important;",
            onclick = paste0("Shiny.setInputValue('", ns("remove_extract"), "', ", i, ");")
          )
        )
      })
      
      do.call(tagList, extract_items)
    })
    
    # Handle removing extractions
    observeEvent(input$remove_extract, {
      req(input$remove_extract)
      extractions <- filename_extractions()
      if (input$remove_extract <= nrow(extractions)) {
        removed_name <- extractions$column_name[input$remove_extract]
        updated_extractions <- extractions[-input$remove_extract, , drop = FALSE]
        filename_extractions(updated_extractions)
        showNotification(paste("Removed column extraction:", removed_name), type = "message")
      }
    })

    df_list_uploaded <- reactiveVal(data.table(name = character(0), path = character(0), size = numeric(0)))
    
    # Store filename extraction rules
    filename_extractions <- reactiveVal(data.frame(
      column_name = character(0),
      pattern = character(0),
      stringsAsFactors = FALSE
    ))



    # Update df_list_uploaded to use all available files from Input Data tab
    # Then apply include/exclude filters to determine the actual working set
    observe({
      if (!is.null(global_files_reactive)) {
        global_files <- global_files_reactive()
        
        # Build file info from all available files
        all_files_info <- data.table(
          name = character(0),
          path = character(0), 
          size = numeric(0)
        )
        
        for (file_name in names(global_files)) {
          file_info <- global_files[[file_name]]
          all_files_info <- rbind(all_files_info, data.table(
            name = file_info$name,
            path = file_info$path,
            size = file_info$size
          ))
        }
        
        df_list_uploaded(all_files_info)
        if (nrow(all_files_info) > 0) {
          showNotification(paste(nrow(all_files_info), "file(s) available - use filters to narrow selection."), duration = 2, type = "message")
        }
      }
    })

    df_list_filtered <- reactive({
      df <- df_list_uploaded()
      if (nrow(df) == 0) return(df) # Return empty DT if no files

      if (!is.null(input$filter_in_files_1) && input$filter_in_files_1 != "") {
        df <- filter_in(df, "name", input$filter_in_files_1)
      }
      if (!is.null(input$filter_out_files_1) && input$filter_out_files_1 != "") {
        df <- filter_out(df, "name", input$filter_out_files_1)
      }
      
      # Update file selector choices
      updateSelectInput(session, "input_preview_file_selector",
        choices = df$name,
        selected = if (nrow(df) > 0) df$name[1] else NULL
      )
      df
    })

    server_data_table_display(
      "data_table_input_file_list",
      reactive(df_list_filtered())
    )

    df_preview_pre_processed <- reactiveVal()
    df_preview_post_processed <- reactiveVal() # For the single file post-process preview

    # Debounced reactive for Ace editor code
    r_code_pre_process_reactive <- debounce(reactive(input$r_code_pre_process), 500)
    r_code_post_process_reactive <- debounce(reactive(input$r_code_post_process), 500)

    ace_server_functions(ns("r_code_pre_process")) # ns() for module IDs
    ace_server_functions(ns("r_code_post_process"))

    # Function to pre-process a single file (for preview)
    pre_process_single_file <- function(file_path, file_name, code, n_every, skip_rows) {
      spsComps::shinyCatch({
        # Environment for eval: pass necessary inputs directly
        env <- new.env(parent = .GlobalEnv) # Safer environment
        env$file_path <- file_path
        env$file_name <- file_name
        env$n_every <- n_every # Use a different name to avoid conflict with base::n
        env$skip_rows <- skip_rows # Use a different name
        env$showNotification <- showNotification # Make shiny's showNotification available
        env$fread <- data.table::fread
        env$read_excel <- readxl::read_excel
        env$read_fst <- fst::read_fst
        env$read_parquet <- nanoparquet::read_parquet
        env$file_ext <- tools::file_ext
        env$setDT <- data.table::setDT
        env$as.data.table <- data.table::as.data.table
        
        eval(parse(text = code), envir = env)
      })
    }
    
    # Function to post-process a single data frame (for preview)
    post_process_single_df <- function(df_to_process, code, filter_in_val, filter_out_val, rename_val, date_fmt_val, extractions_val = NULL) {
        spsComps::shinyCatch({
            env <- new.env(parent = .GlobalEnv)
            env$df <- as.data.table(df_to_process) # Ensure it's a data.table
            env$showNotification <- showNotification
            env$setDT <- data.table::setDT
            env$setnames <- data.table::setnames
            env$melt <- data.table::melt
            env$parse_date_time <- lubridate::parse_date_time
            env$ymd_hms <- lubridate::ymd_hms # if used in code
            env$na.omit <- stats::na.omit
            env$filter_in <- filter_in # Make helper available
            env$filter_out <- filter_out # Make helper available
            env$rname <- rname # Make helper available
            env$extract_from_filename <- extract_from_filename # Make helper available
            env$str_extract <- stringr::str_extract
            env$stringi <- stringi::stri_extract_first_regex # if used

            # Make specific input values available for the post-processing script
            env$input_filter_in_1 <- filter_in_val
            env$input_filter_out_1 <- filter_out_val
            env$input_rename_1 <- rename_val
            env$input_date_format <- date_fmt_val
            env$input_filename_extractions <- extractions_val
            
            eval(parse(text = code), envir = env)
        })
    }


    observe({ # For pre-processing preview
      file_name_selected <- input$input_preview_file_selector
      code_pre <- r_code_pre_process_reactive()
      
      req(file_name_selected, code_pre, nrow(df_list_filtered()) > 0)
      
      file_info_selected <- df_list_filtered()[name == file_name_selected]
      if (nrow(file_info_selected) == 0) {
        showNotification("Selected file not found in the filtered list.", type = "warning")
        df_preview_pre_processed(NULL)
        return()
      }

      df <- pre_process_single_file(
        file_info_selected$path,
        file_info_selected$name,
        code_pre,
        input$file_skip_every_nth,
        input$skip_n_rows_start
      )
      df_preview_pre_processed(df)
    }) |> bindEvent(list(input$reload_data, input$input_preview_file_selector, r_code_pre_process_reactive(), input$file_skip_every_nth, input$skip_n_rows_start), ignoreNULL = TRUE)


    observe({ # For post-processing preview
        req(df_preview_pre_processed())
        code_post <- r_code_post_process_reactive()
        req(code_post)

        df <- post_process_single_df(
            df_preview_pre_processed(), 
            code_post,
            input$filter_in_1,
            input$filter_out_1,
            input$rename_1,
            input$date_format,
            filename_extractions()
        )
        df_preview_post_processed(df)
    }) |> bindEvent(list(df_preview_pre_processed(), r_code_post_process_reactive(), input$filter_in_1, input$filter_out_1, input$rename_1, input$date_format, filename_extractions()), ignoreNULL = TRUE)


    # Display tables for preview
    server_data_table_display("data_table_input_data", reactive(df_preview_pre_processed()))
    server_data_table_display("data_table_input_data_repeat", reactive(df_preview_pre_processed()))
    server_data_table_display("data_table_input_data_sum", reactive(df_preview_pre_processed()))
    server_data_table_display("data_table_modified_data", reactive(df_preview_post_processed()))
    server_data_table_display("data_table_modified_data_sum", reactive(df_preview_post_processed()))
    server_data_table_display("data_table_skim", reactive(df_preview_post_processed()))

    # --- Main Data Processing and Combination Logic for this Importer ---
    combined_data_for_this_importer <- reactiveVal()

    observeEvent(input$combine_data, {
      showNotification("Starting to process and combine all files for this importer...", type = "message", duration = 5)
      
      files_to_process <- df_list_filtered()
      if (nrow(files_to_process) == 0) {
        showNotification("No files to process.", type = "warning")
        combined_data_for_this_importer(NULL)
        return()
      }

      # Isolate all necessary inputs at the beginning of the combine process
      # to avoid issues with reactive changes during the potentially long map2 call.
      isolated_r_code_pre_process <- isolate(input$r_code_pre_process)
      isolated_r_code_post_process <- isolate(input$r_code_post_process)
      isolated_file_skip_every_nth <- isolate(input$file_skip_every_nth)
      isolated_skip_n_rows_start <- isolate(input$skip_n_rows_start)
      isolated_filter_in_1 <- isolate(input$filter_in_1)
      isolated_filter_out_1 <- isolate(input$filter_out_1)
      isolated_rename_1 <- isolate(input$rename_1)
      isolated_date_format <- isolate(input$date_format)
      isolated_filename_extractions <- isolate(filename_extractions())

      # Use future_map2 for parallel processing if files are independent
      # Requires careful handling of environments and function calls
      # For simplicity, using purrr::map2 here. For true parallelism, future::plan(multisession) is set.
      # Ensure functions pre_process_single_file and post_process_single_df are self-contained or pass all dependencies.
      
      progress <- shiny::Progress$new(session, min = 0, max = nrow(files_to_process))
      progress$set(message = "Processing files...", value = 0)
      on.exit(progress$close())

      results_list <- tryCatch({
        purrr::map2(files_to_process$path, files_to_process$name, function(.x_path, .y_name) {
          progress$inc(1, detail = paste("Processing", basename(.y_name)))
          
          # Step 1: Pre-process
          df_pre <- pre_process_single_file(
            file_path = .x_path,
            file_name = .y_name,
            code = isolated_r_code_pre_process,
            n_every = isolated_file_skip_every_nth,
            skip_rows = isolated_skip_n_rows_start
          )
          
          if (is.null(df_pre) || !is.data.frame(df_pre) || nrow(df_pre) == 0) {
            return(NULL) # Skip this file if pre-processing fails or yields no data
          }
          
          # Step 2: Post-process
          df_post <- post_process_single_df(
            df_to_process = df_pre,
            code = isolated_r_code_post_process,
            filter_in_val = isolated_filter_in_1,
            filter_out_val = isolated_filter_out_1,
            rename_val = isolated_rename_1,
            date_fmt_val = isolated_date_format,
            extractions_val = isolated_filename_extractions
          )
          return(df_post)
        })
      }, error = function(e) {
        showNotification(paste("Error during batch processing:", e$message), type = "error", duration = 10)
        return(list()) # Return empty list on error
      })
      
      valid_results <- Filter(Negate(is.null), results_list)
      
      if (length(valid_results) > 0) {
        final_combined_dt <- data.table::rbindlist(valid_results, use.names = TRUE, fill = TRUE)
        combined_data_for_this_importer(final_combined_dt)
        showNotification(paste("Successfully processed and combined", length(valid_results), "files."), type = "message")
      } else {
        combined_data_for_this_importer(NULL)
        showNotification("No data resulted from processing the files.", type = "warning")
      }
    })

    # Return the combined data for this importer module
    list(
      df = reactive(combined_data_for_this_importer()),
      tab_rename_trigger = tab_rename_trigger
    )
  })
}
