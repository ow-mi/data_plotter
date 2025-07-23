# functions.R
library(shiny)
library(bslib)
library(stringr)
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


aceEditor_pre <- function(inputId, value, mode = "r", theme = "gruvbox", minLines = 6, maxLines = 25, fontSize = 13) {
  aceEditor(
    inputId,
    value = value,
    mode = mode,
    theme = theme,
    minLines = minLines,
    maxLines = maxLines,
    fontSize = fontSize,
    height = "250px",  # Reduced height for better performance
    autoScrollEditorIntoView = FALSE,  # Disable auto-scroll for performance
    wordWrap = TRUE,
    showPrintMargin = FALSE,
    highlightActiveLine = FALSE,  # Disable for performance
    debounce = 750  # Increase debounce to reduce processing
  )
}

ace_server_functions <- function(ace_input_name) {
  # These functions are generally for more advanced Ace features or if issues with default setup.
  # aceAutocomplete(ace_input_name) # Often handled by enableLiveAutocompletion
  # aceTooltip(ace_input_name)
  # aceAnnotate(ace_input_name) # For adding error/warning markers
}

# --- Server Module for Data Table Display ---


# --- Server Module for Data Combiner (combines outputs of multiple importers) ---



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
