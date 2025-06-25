# functions.R
library(shiny)
library(bslib)
library(stringr)
library(data.table)
library(jsonlite) # For fromJSON in aceEditor_pre
library(shinyAce) # For aceEditor and server functions

# --- Custom UI Helper Functions ---
p <- function(...) { # Concatenate all arguments with paste0
  paste0(...)
}

text_input_tip <- function(inputId, label, value = "", tip = "Tooltip text", placeholder = "") {
  shiny::textInput(inputId, label, value, placeholder = placeholder) |>
    bslib::tooltip(tip, placement = "right")
}

numeric_input_tip <- function(inputId, label, value, min = 0, max = 1, step = 0.1, tip = "Tooltip text") {
  shiny::numericInput(inputId, label, value = value, min = min, max = max, step = step) |>
    bslib::tooltip(tip, placement = "right")
}

action_input_tip <- function(inputId, label, tip = "Tooltip text", ...) {
  shiny::actionButton(inputId, label, ...) |>
    bslib::tooltip(tip, placement = "right")
}

# --- Data Manipulation Functions ---
filter_in <- function(df, col_to_effect, input_text) {
  if (!is.data.table(df)) setDT(df)
  if (is.null(input_text) || input_text == "") return(df)
  
  # Split by comma, allowing for optional spaces
  patterns <- str_trim(str_split(input_text, ",\\s*")[[1]])
  patterns <- patterns[patterns != ""] # Remove empty strings
  if (length(patterns) == 0) return(df)
  
  # Create a single regex pattern: (pattern1|pattern2|...)
  regex_pattern <- paste(patterns, collapse = "|")
  
  # Use str_detect with the regex pattern
  # Ensure the column exists and get its values
  if (!col_to_effect %in% names(df)) {
    warning(paste("Column", col_to_effect, "not found in filter_in."))
    return(df)
  }
  
  # data.table filtering
  df[str_detect(get(col_to_effect), regex_pattern)]
}

filter_out <- function(df, col_to_effect, input_text) {
  if (!is.data.table(df)) setDT(df)
  if (is.null(input_text) || input_text == "") return(df)
  
  patterns <- str_trim(str_split(input_text, ",\\s*")[[1]])
  patterns <- patterns[patterns != ""]
  if (length(patterns) == 0) return(df)
  
  regex_pattern <- paste(patterns, collapse = "|")
  
  if (!col_to_effect %in% names(df)) {
    warning(paste("Column", col_to_effect, "not found in filter_out."))
    return(df)
  }
  
  df[!str_detect(get(col_to_effect), regex_pattern)]
}

rname <- function(df, col_to_effect, input_text) {
  if (!is.data.table(df)) setDT(df)
  if (is.null(input_text) || input_text == "" || !grepl(",", input_text)) {
    # Expecting pairs like "old1,new1,old2,new2"
    if (input_text != "") warning("Rename input format seems incorrect. Expected 'old1,new1,old2,new2'.")
    return(df)
  }
  
  rename_parts <- str_trim(str_split(input_text, ",\\s*")[[1]])
  
  # Ensure we have an even number of parts for old/new pairs
  if (length(rename_parts) %% 2 != 0) {
    warning("Rename pairs are uneven. Last part ignored.")
    rename_parts <- rename_parts[-length(rename_parts)] # Remove last if uneven
  }
  if (length(rename_parts) == 0) return(df)

  # Create named vector for str_replace_all: c(old1 = new1, old2 = new2)
  old_names <- rename_parts[seq(1, length(rename_parts), by = 2)]
  new_names <- rename_parts[seq(2, length(rename_parts), by = 2)]
  
  # Ensure no empty old_names which can cause issues with str_replace_all
  valid_indices <- old_names != ""
  old_names <- old_names[valid_indices]
  new_names <- new_names[valid_indices]

  if (length(old_names) == 0) return(df)
  
  rename_vector <- setNames(new_names, old_names)

  if (!col_to_effect %in% names(df)) {
    warning(paste("Column", col_to_effect, "not found in rname."))
    return(df)
  }
  
  # data.table in-place modification
  df[, (col_to_effect) := str_replace_all(get(col_to_effect), rename_vector)]
  df
}


# --- Ace Editor Setup ---

# Generate function lists for Ace Editor autocompletion (run this once to create the JSON)
generate_function_lists <- function() {
  tryCatch({
    tidyverse_packages <- c("ggplot2", "dplyr", "tidyr", "readr", "purrr", "tibble", "stringr", "forcats", "data.table", "lubridate")
    get_functions_from_package <- function(pkg) {
      if (requireNamespace(pkg, quietly = TRUE)) {
        ns <- getNamespace(pkg)
        # Filter out non-function objects and internal-looking functions
        funcs <- Filter(function(x) is.function(ns[[x]]) && !startsWith(x, "."), ls(ns, all.names = FALSE))
        if (length(funcs) > 0) {
          data.frame(
            name = funcs,
            meta = pkg, # Use 'meta' as per shinyAce documentation for category
            value = funcs, # 'value' is what's inserted
            score = 100, # Arbitrary score
            stringsAsFactors = FALSE
          )
        } else {
          NULL
        }
      } else {
        NULL
      }
    }
    
    all_pkg_functions_list <- lapply(tidyverse_packages, get_functions_from_package)
    all_pkg_functions_df <- do.call(rbind, all_pkg_functions_list)
    
    # Add base R functions (a selection, not exhaustive)
    base_r_funcs <- c("c", "list", "data.frame", "matrix", "vector", "seq", "rep", "lapply", "sapply", "tapply", "mean", "sum", "min", "max", "sd", "median", "quantile", "summary", "head", "tail", "subset", "transform", "with", "within", "if", "else", "for", "while", "function", "return", "print", "cat", "paste", "paste0", "sprintf", "plot", "hist", "lines", "points", "text", "legend", "par", "options", "Sys.Date", "Sys.time", "as.Date", "as.POSIXct")
    base_df <- data.frame(
        name = base_r_funcs,
        meta = "base R",
        value = base_r_funcs,
        score = 100,
        stringsAsFactors = FALSE
    )
    
    combined_functions <- rbind(all_pkg_functions_df, base_df)
    combined_functions <- combined_functions[!duplicated(combined_functions$name), ] # Remove duplicates

    # if (!dir.exists("www")) dir.create("www") # shinyAce looks in www by default for JSON
    # jsonlite::write_json(combined_functions, "www/r_functions_autocomplete.json", pretty = TRUE)
    # message("Generated www/r_functions_autocomplete.json for Ace autocompletion.")
  # }, error = function(e) {
    # message(paste("Error generating autocomplete list:", e$message))
  })
}

# Call it once if the file doesn't exist or needs updating
if (!file.exists("www/r_functions_autocomplete.json")) {
  generate_function_lists()
}


aceEditor_pre <- function(inputId, value, mode = "r", theme = "gruvbox", minLines = 6, maxLines = Inf, fontSize = 13) {
  # Load annotations from the generated JSON file
  # shinyAce expects a list of lists/data.frames for autoCompletion
  # The structure should be list(completerName1 = list_of_terms, completerName2 = df_of_terms)
  # For static completer, it expects a data.frame with 'name', 'value', 'score', 'meta'
  
  # Try to load the custom completer list
  # custom_completers <- list()
  # if (file.exists("www/r_functions_autocomplete.json")) {
  #     custom_completers$static <- jsonlite::fromJSON("www/r_functions_autocomplete.json")
  # } else {
  #     warning("www/r_functions_autocomplete.json not found. Ace editor will have basic autocompletion.")
  # }

  aceEditor(
    inputId,
    value = value,
    mode = mode,
    theme = theme,
    minLines = minLines,
    maxLines = maxLines,
    fontSize = fontSize,
    height = "300px",  # Fixed height instead of auto
    autoScrollEditorIntoView = TRUE,
    wordWrap = TRUE,
    showPrintMargin = FALSE,
    highlightActiveLine = TRUE
    # Note: Some autocompletion arguments may not be supported in older shinyAce versions
    # debounce = 500, # ms delay before value updates
    # autoCompleters = c("static", "text", "rlang"), # Include "static" for our custom list
    # customAutoCompleters = if (length(custom_completers) > 0) custom_completers else NULL
  )
}

ace_server_functions <- function(ace_input_name) {
  # These functions are generally for more advanced Ace features or if issues with default setup.
  # aceAutocomplete(ace_input_name) # Often handled by enableLiveAutocompletion
  # aceTooltip(ace_input_name)
  # aceAnnotate(ace_input_name) # For adding error/warning markers
}

# --- Server Module for Data Table Display ---
server_data_table_display <- function(id, input_data_reactive) {
  moduleServer(id, function(input, output, session) {
    processed_data <- reactiveVal()

    # Debounce the R code input to avoid rapid re-evaluations
    debounced_code_input <- debounce(reactive(input$code_input), 1000)

    observe({ # Unified observer for code changes or data changes
      current_code <- debounced_code_input()
      current_data <- input_data_reactive()
      
      # Don't req(current_data) here - our code templates handle NULL data gracefully
      
      # Only proceed if code is non-empty, otherwise might show raw data
      # Or, if code is empty, could default to showing current_data directly as DT
      if (is.null(current_code) || current_code == "") {
          # Default behavior if no code: render data as a basic DT table
          if (!is.null(current_data) && (is.data.frame(current_data) || is.data.table(current_data))) {
            processed_data(datatable(current_data, options = list(scrollX = TRUE, pageLength = 5, lengthMenu = c(5, 10, 25, 50)), rownames = FALSE, filter = 'top', class = 'compact stripe hover'))
          } else {
            # Handle NULL data case even when no custom code
            processed_data(data.frame(Message = "No data available") |> datatable(options = list(dom = 't', ordering = FALSE), rownames = FALSE, class = 'compact'))
          }
          return()
      }

      spsComps::shinyCatch({
        # Ensure 'df' is available in the eval environment
        env <- new.env(parent = .GlobalEnv) # Safer environment
        env$df <- current_data
        # Make essential functions available for data table display code
        env$datatable <- DT::datatable
        env$renderPrint <- shiny::renderPrint
        env$skim <- skimr::skim  # Use fully qualified name
        env$setDT <- data.table::setDT
        env$data.frame <- data.frame
        env$is.data.table <- data.table::is.data.table
        env$is.data.frame <- is.data.frame
        env$is.null <- is.null
        env$nrow <- nrow
        env$ncol <- ncol
        env$names <- names
        env$paste <- paste
        env$c <- c
        env$library <- library  # Allow library calls in eval code
        env$require <- require   # Allow require calls in eval code
        
        evaluated_output <- eval(parse(text = current_code), envir = env)
        processed_data(evaluated_output)
      })
    }) |> bindEvent(list(debounced_code_input(), input_data_reactive(), input$apply_code), ignoreNULL = FALSE, ignoreInit = FALSE)


    output$data_table <- renderUI({
      req(processed_data()) # Ensure processed_data is available
      tagList(processed_data()) # Wrap in tagList in case it's multiple UI elements
    })
  })
}


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

    df_list_uploaded <- reactiveVal(data.table(name = character(0), path = character(0), size = numeric(0)))



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
    post_process_single_df <- function(df_to_process, code, filter_in_val, filter_out_val, rename_val, date_fmt_val) {
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
            env$str_extract <- stringr::str_extract
            env$stringi <- stringi::stri_extract_first_regex # if used

            # Make specific input values available for the post-processing script
            env$input_filter_in_1 <- filter_in_val
            env$input_filter_out_1 <- filter_out_val
            env$input_rename_1 <- rename_val
            env$input_date_format <- date_fmt_val
            
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
            input$date_format
        )
        df_preview_post_processed(df)
    }) |> bindEvent(list(df_preview_pre_processed(), r_code_post_process_reactive(), input$filter_in_1, input$filter_out_1, input$rename_1, input$date_format), ignoreNULL = TRUE)


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
            date_fmt_val = isolated_date_format
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

# --- Server Module for Data Combiner (combines outputs of multiple importers) ---
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
    
    # Apply filters to combined data
    observeEvent(input$apply_combiner_filters, {
      base_data <- combined_data_from_all_importers()
      
      if (is.null(base_data)) {
        showNotification("No data available to filter", type = "warning")
        return()
      }
      
      filtered_df <- data.table::copy(base_data)
      
      # Apply source filter
      if (!is.null(input$combiner_filter_source) && input$combiner_filter_source != "") {
        source_pattern <- input$combiner_filter_source
        tryCatch({
          filtered_df <- filtered_df[grepl(source_pattern, source_importer_id, perl = TRUE)]
        }, error = function(e) {
          showNotification(paste("Source filter error:", e$message), type = "error")
          return()
        })
      }
      
      # Apply file filter
      if (!is.null(input$combiner_filter_files) && input$combiner_filter_files != "" && "file_name_source" %in% names(filtered_df)) {
        file_pattern <- input$combiner_filter_files
        tryCatch({
          filtered_df <- filtered_df[grepl(file_pattern, file_name_source, perl = TRUE)]
        }, error = function(e) {
          showNotification(paste("File filter error:", e$message), type = "error")
          return()
        })
      }
      
      filtered_data(filtered_df)
      filter_applied(TRUE)
      
      showNotification(paste("Filters applied. Showing", nrow(filtered_df), "of", nrow(base_data), "rows"), type = "message")
    })
    
    # Clear filters
    observeEvent(input$clear_combiner_filters, {
      filter_applied(FALSE)
      filtered_data(NULL)
      
      # Clear input fields
      updateTextInput(session, "combiner_filter_source", value = "")
      updateTextInput(session, "combiner_filter_files", value = "")
      
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
    r_code_plot_static_reactive <- debounce(reactive(input$r_code_plot_static), 500)
    r_code_plot_interactive_reactive <- debounce(reactive(input$r_code_plot_interactive), 500)
    r_code_plot_table_reactive <- debounce(reactive(input$r_code_plot_table), 500)
    r_code_plot_final_reactive <- debounce(reactive(input$r_code_plot_final), 500)
    
    # Ace server functions for each editor
    ace_server_functions(ns("r_code_plot_text"))
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
        env$ggplot <- ggplot2::ggplot
        env$geom_line <- ggplot2::geom_line
        env$geom_point <- ggplot2::geom_point
        env$geom_ribbon <- ggplot2::geom_ribbon
        env$geom_scattermore <- scattermore::geom_scattermore
        env$aes <- ggplot2::aes
        env$labs <- ggplot2::labs
        env$ggtitle <- ggplot2::ggtitle # Deprecated, use labs(title=)
        env$xlab <- ggplot2::xlab # Deprecated
        env$ylab <- ggplot2::ylab # Deprecated
        env$theme_bw <- ggplot2::theme_bw
        env$facet_wrap <- ggplot2::facet_wrap
        env$str_sub <- stringr::str_sub
        env$ggplotly <- plotly::ggplotly
        env$plot_ly <- plotly::plot_ly
        env$add_trace <- plotly::add_trace
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

# --- Helper Functions ---

# Code sharing helper function for plotters and importers
get_plotter_code <- function(plotter_id, code_type = "static", main_session_input = NULL) {
  # Helper function to access code from other plotters
  # plotter_id: e.g., "plotter_1", "plotter_2", etc.
  # code_type: "text", "static", "interactive", "table", "final", "process"
  # main_session_input: The main session's input object
  
  if (is.null(main_session_input)) {
    return("# Error: get_plotter_code() requires main_session_input\n# Usage: get_plotter_code('plotter_1', 'static', main_session_input)")
  }
  
  # Build the namespaced input ID for the ace editor
  if (code_type == "process") {
    input_id <- paste0(plotter_id, "-r_code_plot_process")
  } else {
    input_id <- paste0(plotter_id, "-r_code_plot_", code_type)
  }
  
  # Get the code from the other plotter's ace editor
  code <- main_session_input[[input_id]]
  
  if (is.null(code) || trimws(code) == "") {
    return(paste0("# No code found in ", plotter_id, " for '", code_type, "' plot type\n# Available types: 'text', 'static', 'interactive', 'table', 'final', 'process'"))
  }
  
  return(code)
}

get_importer_code <- function(importer_id, code_type = "pre", main_session_input = NULL) {
  # Helper function to access code from other importers
  # importer_id: e.g., "data_import_module_1", "data_import_module_2", etc.
  # code_type: "pre" or "post"
  # main_session_input: The main session's input object
  
  if (is.null(main_session_input)) {
    return("# Error: get_importer_code() requires main_session_input\n# Usage: get_importer_code('data_import_module_1', 'pre', main_session_input)")
  }
  
  # Build the namespaced input ID for the ace editor
  if (code_type == "pre") {
    input_id <- paste0(importer_id, "-r_code_pre_process")
  } else if (code_type == "post") {
    input_id <- paste0(importer_id, "-r_code_post_process")
  } else {
    return(paste0("# Invalid code_type: ", code_type, "\n# Available types: 'pre', 'post'"))
  }
  
  # Get the code from the other importer's ace editor
  code <- main_session_input[[input_id]]
  
  if (is.null(code) || trimws(code) == "") {
    return(paste0("# No code found in ", importer_id, " for '", code_type, "' processing\n# Available types: 'pre', 'post'"))
  }
  
  return(code)
}
