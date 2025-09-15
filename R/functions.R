# functions.R
# Note: Required libraries are loaded in app.R

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

# Extract values from filename using regex patterns
extract_from_filename <- function(df, filename_col = "file_name_source", extractions_df) {
  if (!is.data.table(df)) setDT(df)
  
  # Check if filename column exists
  if (!filename_col %in% names(df)) {
    warning(paste("Column", filename_col, "not found for filename extraction."))
    return(df)
  }
  
  # If no extractions defined, return original data
  if (is.null(extractions_df) || nrow(extractions_df) == 0) {
    return(df)
  }
  
  # Apply each extraction rule
  for (i in 1:nrow(extractions_df)) {
    column_name <- extractions_df$column_name[i]
    pattern <- extractions_df$pattern[i]
    
    tryCatch({
      # Check if pattern has capturing groups (parentheses)
      if (grepl("\\(.*\\)", pattern)) {
        # Use str_match to extract capturing groups
        matches <- str_match(df[[filename_col]], pattern)
        # If there are capturing groups, use the first one, otherwise use the full match
        if (ncol(matches) > 1) {
          df[, (column_name) := matches[, 2]]  # Second column is first capturing group
        } else {
          df[, (column_name) := matches[, 1]]  # Full match if no capturing groups
        }
      } else {
        # Use str_extract for patterns without capturing groups
        df[, (column_name) := str_extract(get(filename_col), pattern)]
      }
    }, error = function(e) {
      warning(paste("Error applying pattern", pattern, "for column", column_name, ":", e$message))
    })
  }
  
  df
}

# Evaluate R code within strings for dynamic text generation
string_eval <- function(text, env = parent.frame(), safe_mode = FALSE, max_length = 1000) {
  if (is.null(text) || is.na(text) || text == "") {
    return(text)
  }

  # Improved regex pattern that handles escaped quotes better
  # Matches quoted strings but avoids matching escaped quotes within
  pattern <- '(["\'])((?:\\\\.|(?!(?<!\\\\)\\1).)*?)\\1'

  # Extract all quoted sections with better handling
  matches <- gregexpr(pattern, text, perl = TRUE)
  match_data <- regmatches(text, matches)[[1]]

  if (length(match_data) == 0) {
    # No quoted code found, return original text
    return(text)
  }

  # Process each quoted section
  result_text <- text
  processed_codes <- list() # Track processed code to avoid duplicate processing

  for (i in seq_along(match_data)) {
    quoted_code <- match_data[i]

    # Skip if we've already processed this exact code
    if (quoted_code %in% names(processed_codes)) {
      next
    }

    # Extract the content between quotes, handling escaped characters
    code_match <- regexec(pattern, quoted_code, perl = TRUE)
    code <- regmatches(quoted_code, code_match)[[1]][3] # Get the captured group

    # Unescape quotes within the code
    code <- gsub('\\\\(["\'])', '\\1', code)

    # Skip empty code
    if (is.null(code) || code == "") {
      next
    }

    tryCatch({
      # Security check in safe mode
      if (safe_mode) {
        # Block potentially dangerous operations
        dangerous_patterns <- c(
          "system\\s*\\(",
          "shell\\s*\\(",
          "eval\\s*\\(",
          "parse\\s*\\(",
          "source\\s*\\(",
          "file\\.remove\\s*\\(",
          "unlink\\s*\\(",
          "setwd\\s*\\(",
          "\\.Call\\s*\\(",
          "\\.External\\s*\\(",
          "\\.C\\s*\\(",
          "\\.Fortran\\s*\\("
        )

        if (any(sapply(dangerous_patterns, function(p) grepl(p, code, ignore.case = TRUE)))) {
          stop("Potentially unsafe code detected")
        }
      }

      # Evaluate the code in the provided environment
      eval_result <- eval(parse(text = code), envir = env)

      # Improved result conversion with better type handling
      result_str <- format_eval_result(eval_result, max_length)

      # Replace the quoted code with the result in the text
      result_text <- gsub(
        pattern = fixed(quoted_code),
        replacement = result_str,
        x = result_text,
        fixed = TRUE
      )

      # Cache the result to avoid reprocessing identical code
      processed_codes[[quoted_code]] <- result_str

    }, error = function(e) {
      # More informative error message
      error_msg <- sprintf("[Error evaluating '%s': %s]", substr(code, 1, 50), e$message)

      # If error message is too long, truncate it
      if (nchar(error_msg) > 200) {
        error_msg <- paste0(substr(error_msg, 1, 197), "...]")
      }

      result_text <- gsub(
        pattern = fixed(quoted_code),
        replacement = error_msg,
        x = result_text,
        fixed = TRUE
      )

      # Cache the error result too
      processed_codes[[quoted_code]] <- error_msg
    })
  }

  return(result_text)
}

# Helper function to format evaluation results
format_eval_result <- function(result, max_length = 1000) {
  if (is.null(result)) {
    return("NULL")
  }

  if (is.na(result) && length(result) == 1) {
    return("NA")
  }

  # Handle different data types
  if (is.factor(result)) {
    result <- as.character(result)
  } else if (is.logical(result)) {
    result <- ifelse(result, "TRUE", "FALSE")
  } else if (is.list(result)) {
    # Convert list to a readable format
    if (length(result) == 0) {
      return("list()")
    } else {
      items <- sapply(result, function(x) {
        if (is.null(x)) "NULL" else as.character(x)
      })
      return(sprintf("list(%s)", paste(items, collapse = ", ")))
    }
  } else if (is.data.frame(result)) {
    # For data frames, show dimensions
    return(sprintf("data.frame(%d rows, %d cols)", nrow(result), ncol(result)))
  } else if (is.matrix(result)) {
    return(sprintf("matrix(%d x %d)", nrow(result), ncol(result)))
  }

  # Convert to character
  if (is.vector(result) && length(result) > 1) {
    # Join multiple values with commas, but limit total length
    result_str <- paste(result, collapse = ", ")

    if (nchar(result_str) > max_length) {
      # Truncate and add indicator
      result_str <- paste0(substr(result_str, 1, max_length - 3), "...")
    }

    return(result_str)
  } else {
    result_str <- as.character(result)

    # Handle very long single results
    if (nchar(result_str) > max_length) {
      result_str <- paste0(substr(result_str, 1, max_length - 3), "...")
    }

    return(result_str)
  }
}

# Safe version for user inputs (blocks system calls and file operations)
string_eval_safe <- function(text, env = parent.frame(), max_length = 1000) {
  string_eval(text, env, safe_mode = TRUE, max_length = max_length)
}

# Version that preserves original quotes and formatting
string_eval_preserve <- function(text, env = parent.frame(), safe_mode = FALSE) {
  if (is.null(text) || is.na(text) || text == "") {
    return(text)
  }

  # Pattern that captures quotes, content, and closing quote separately
  pattern <- '(["\'])((?:\\\\.|(?!(?<!\\\\)\\1).)*?)\\1'

  matches <- gregexpr(pattern, text, perl = TRUE)
  match_data <- regmatches(text, matches)[[1]]

  if (length(match_data) == 0) {
    return(text)
  }

  result_text <- text

  for (quoted_code in match_data) {
    # Extract quote type and content
    code_match <- regexec(pattern, quoted_code, perl = TRUE)
    parts <- regmatches(quoted_code, code_match)[[1]]

    if (length(parts) >= 4) {
      quote_type <- parts[2]
      code <- parts[3]

      # Unescape quotes within the code
      code <- gsub('\\\\(["\'])', '\\1', code)

      if (code != "") {
        tryCatch({
          if (safe_mode) {
            # Same security checks as before
            dangerous_patterns <- c(
              "system\\s*\\(", "shell\\s*\\(", "eval\\s*\\(",
              "parse\\s*\\(", "source\\s*\\(", "file\\.remove\\s*\\(",
              "unlink\\s*\\(", "setwd\\s*\\("
            )

            if (any(sapply(dangerous_patterns, function(p) grepl(p, code, ignore.case = TRUE)))) {
              stop("Potentially unsafe code detected")
            }
          }

          eval_result <- eval(parse(text = code), envir = env)
          result_str <- format_eval_result(eval_result)

          # Replace with same quote type
          replacement <- paste0(quote_type, result_str, quote_type)
          result_text <- gsub(fixed(quoted_code), replacement, result_text, fixed = TRUE)

        }, error = function(e) {
          error_msg <- sprintf("Error: %s", e$message)
          replacement <- paste0(quote_type, error_msg, quote_type)
          result_text <- gsub(fixed(quoted_code), replacement, result_text, fixed = TRUE)
        })
      }
    }
  }

  return(result_text)
}

# Test and demonstration functions for string_eval
test_string_eval <- function() {
  # Create a test environment with some variables
  test_env <- new.env()
  test_env$x <- 42
  test_env$name <- "Alice"
  test_env$scores <- c(85, 92, 78, 96)
  test_env$my_list <- list(a = 1, b = "hello", c = TRUE)
  test_env$my_df <- data.frame(id = 1:3, value = c("A", "B", "C"))

  cat("=== Testing string_eval improvements ===\n\n")

  # Test 1: Basic variable evaluation
  text1 <- "Hello 'name', your score is 'x' points!"
  result1 <- string_eval(text1, test_env)
  cat("Test 1 - Basic variables:\n")
  cat("Input: ", text1, "\n")
  cat("Output:", result1, "\n\n")

  # Test 2: Vector handling
  text2 <- "Your scores are: 'paste(scores, collapse=\", \")'"
  result2 <- string_eval(text2, test_env)
  cat("Test 2 - Vector handling:\n")
  cat("Input: ", text2, "\n")
  cat("Output:", result2, "\n\n")

  # Test 3: List handling
  text3 <- "List contents: 'my_list'"
  result3 <- string_eval(text3, test_env)
  cat("Test 3 - List handling:\n")
  cat("Input: ", text3, "\n")
  cat("Output:", result3, "\n\n")

  # Test 4: Data frame handling
  text4 <- "Data frame info: 'my_df'"
  result4 <- string_eval(text4, test_env)
  cat("Test 4 - Data frame handling:\n")
  cat("Input: ", text4, "\n")
  cat("Output:", result4, "\n\n")

  # Test 5: Escaped quotes
  text5 <- "Result: 'paste(\"Hello\", \"World\", sep=\" \")'"
  result5 <- string_eval(text5, test_env)
  cat("Test 5 - Escaped quotes:\n")
  cat("Input: ", text5, "\n")
  cat("Output:", result5, "\n\n")

  # Test 6: Error handling
  text6 <- "This will error: 'nonexistent_var + 1'"
  result6 <- string_eval(text6, test_env)
  cat("Test 6 - Error handling:\n")
  cat("Input: ", text6, "\n")
  cat("Output:", result6, "\n\n")

  # Test 7: Safe mode (blocks dangerous operations)
  text7 <- "This is blocked: 'system(\"echo hello\")'"
  result7 <- string_eval_safe(text7, test_env)
  cat("Test 7 - Safe mode:\n")
  cat("Input: ", text7, "\n")
  cat("Output:", result7, "\n\n")

  # Test 8: Mixed quotes
  text8 <- 'Single quotes: \'x\' and double quotes: "name"'
  result8 <- string_eval(text8, test_env)
  cat("Test 8 - Mixed quotes:\n")
  cat("Input: ", text8, "\n")
  cat("Output:", result8, "\n\n")

  cat("=== All tests completed ===\n")
}

# Shiny integration helper
create_eval_text_input <- function(inputId, label, value = "", tip = "Use single or double quotes around R code to evaluate it", ...) {
  text_input_tip(
    inputId = inputId,
    label = label,
    value = value,
    tip = tip,
    ...
  )
}

# Function to process text input with evaluation
process_text_input <- function(text_input, env = parent.frame(), safe_mode = TRUE) {
  if (safe_mode) {
    string_eval_safe(text_input, env)
  } else {
    string_eval(text_input, env)
  }
}

# === Enhanced Filtering System Helper Functions ===

# Helper function for automation: create filter strings programmatically
create_filter_string <- function(..., separator = " | ") {
  filters <- list(...)
  filter_parts <- c()
  
  for (name in names(filters)) {
    values <- filters[[name]]
    if (length(values) > 0) {
      # Convert vector to semicolon-separated string
      values_str <- paste(values, collapse = ";")
      filter_parts <- c(filter_parts, paste0(name, ":", values_str))
    }
  }
  
  paste(filter_parts, collapse = separator)
}

# Helper function for automation: create dynamic filter with code evaluation
create_dynamic_filter <- function(..., separator = " | ") {
  filters <- list(...)
  filter_parts <- c()
  
  for (name in names(filters)) {
    value <- filters[[name]]
    if (length(value) == 1 && is.character(value)) {
      # If it's R code, wrap in quotes for string_eval
      if (grepl("^[a-zA-Z_\\[\\(]", value) && grepl("[\\]\\)\\,]", value)) {
        filter_parts <- c(filter_parts, paste0(name, ':"', value, '"'))
      } else {
        filter_parts <- c(filter_parts, paste0(name, ":", value))
      }
    } else {
      # Multiple values, join with semicolon
      values_str <- paste(value, collapse = ";")
      filter_parts <- c(filter_parts, paste0(name, ":", values_str))
    }
  }
  
  paste(filter_parts, collapse = separator)
}

# Updated automation functions for new filtering system
create_custom_filter <- function(..., separator = " | ") {
  filters <- list(...)
  filter_parts <- c()
  
  for (name in names(filters)) {
    values <- filters[[name]]
    if (length(values) > 0) {
      if (length(values) == 1 && is.character(values) && grepl("^[a-zA-Z_\\[\\(]", values)) {
        # Single R code expression, wrap in quotes
        filter_parts <- c(filter_parts, paste0(name, ':"', values, '"'))
      } else {
        # Multiple values or simple values, join with commas
        values_str <- paste(values, collapse = ",")
        filter_parts <- c(filter_parts, paste0(name, ":", values_str))
      }
    }
  }
  
  paste(filter_parts, collapse = separator)
}

# Create batch filters with semicolon separation
create_batch_filters <- function(...) {
  filter_combinations <- list(...)
  batch_parts <- c()
  
  for (combo in filter_combinations) {
    if (is.list(combo)) {
      # Named list of column:values
      combo_str <- create_custom_filter(!!!combo)
      batch_parts <- c(batch_parts, combo_str)
    } else if (is.character(combo)) {
      # Already formatted string
      batch_parts <- c(batch_parts, combo)
    }
  }
  
  paste(batch_parts, collapse = " ; ")
}

# Automation function for updating combined data filter
update_combined_data_filter <- function(session, filter_string) {
  # This function updates the custom_filter input in the combined data panel
  # 
  # Parameters:
  # - session: The Shiny session object
  # - filter_string: The filter string to set
  # 
  # Example usage in a Shiny server:
  # update_combined_data_filter(session, "dut:27,28 | series:temp.*")
  # update_combined_data_filter(session, create_custom_filter(dut = c(27, 28), series = "temp.*"))
  # update_combined_data_filter(session, create_batch_filters(
  #   list(dut = 27, series = 14),
  #   list(dut = 28, series = 15)
  # ))
  
  updateTextAreaInput(session, "combiner-custom_filter", value = filter_string)
}

# Helper function for creating automation filter strings
automation_filter <- function(...) {
  # Simple wrapper that creates filter strings for automation
  # This is the main function automation scripts should use
  # 
  # Example usage:
  # filter1 <- automation_filter(dut = c(27, 28), series = "temp.*")
  # filter2 <- automation_filter(
  #   list(dut = 27, series = 14),
  #   list(dut = 28, series = 15)
  # )
  
  args <- list(...)
  
  # Check if we have multiple list arguments (batch mode)
  if (length(args) > 1 && all(sapply(args, is.list))) {
    return(create_batch_filters(...))
  } else {
    return(create_custom_filter(...))
  }
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


aceEditor_pre_ <- function(inputId, value, mode = "r", theme = "gruvbox", minLines = 6, maxLines = 50, fontSize = 13, height = "auto") {
  aceEditor(
    inputId,
    value = value,
    mode = mode,
    theme = theme,
    minLines = minLines,
    maxLines = maxLines,
    fontSize = fontSize,
    height = if(height == "auto") "calc(100vh - 300px)" else height,  # Dynamic height for sidebars
    autoScrollEditorIntoView = FALSE,  # Disable auto-scroll for performance
    wordWrap = TRUE,
    showPrintMargin = FALSE,
    highlightActiveLine = FALSE,  # Disable for performance
    debounce = 750  # Increase debounce to reduce processing
  )
}

# Enhanced Ace editor with fullscreen capability
aceEditor_pre <- function(inputId, value, mode = "r", theme = "gruvbox", minLines = 6, maxLines = 50, fontSize = 13, height = "auto", showFullscreenButton = TRUE) {

  # A unique ID for the main wrapper div that contains the button and editor
  wrapperId <- paste0(inputId, "_wrapper")

  # Create the fullscreen button. It will be part of the editor_container.
  fullscreen_button <- if (showFullscreenButton) {
    actionButton(
      paste0(inputId, "_fullscreen_btn"),
      icon = icon("expand"),
      label = "Fullscreen",
      class = "btn-sm btn-outline-secondary",
      # Style to position it at the top-right of the wrapper
      style = "position: absolute; top: 5px; right: 5px; z-index: 10;"
    )
  } else {
    NULL
  }

  # Create the editor container with a unique ID and relative positioning.
  # This is the element our JavaScript will make fullscreen.
  editor_container <- div(
    id = wrapperId,
    style = "position: relative;",
    fullscreen_button,
    aceEditor(
      inputId,
      value = value,
      mode = mode,
      theme = theme,
      minLines = minLines,
      maxLines = maxLines,
      fontSize = fontSize,
      # Use a calculated height for auto, which works better in Shiny layouts
      height = if(height == "auto") "calc(100vh - 300px)" else height,
      autoScrollEditorIntoView = FALSE,
      wordWrap = TRUE,
      showPrintMargin = FALSE,
      highlightActiveLine = TRUE, # It's a nice feature to have
      debounce = 750
    )
  )

  # CSS for the fullscreen state and the new close button
  fullscreen_css <- tags$style(HTML(sprintf("
    /* This class is applied to the wrapper div (%s) */
    .ace-editor-fullscreen {
      position: fixed !important;
      top: 0 !important;
      left: 0 !important;
      width: 100vw !important;
      height: 100vh !important;
      z-index: 9999 !important;
      /* Use a background that matches the default theme */
      background: #272822 !important;
      padding: 10px;
      box-sizing: border-box;
    }

    /* Ensure the ace editor instance fills the new fullscreen wrapper */
    .ace-editor-fullscreen .shiny-ace-container,
    .ace-editor-fullscreen .shiny-ace-container .ace_editor {
      height: 100%% !important;
      width: 100%% !important;
    }

    /* Hide the original fullscreen button when in fullscreen mode */
    .ace-editor-fullscreen > #%s_fullscreen_btn {
      display: none;
    }

    /* Style for the new 'Exit' button we add with JavaScript */
    .fullscreen-close-btn {
      position: absolute;
      top: 15px;
      right: 15px;
      z-index: 10000;
      background: #f92672; /* A color that fits the gruvbox theme */
      color: white;
      border: 1px solid #fff;
      border-radius: 4px;
      padding: 8px 12px;
      cursor: pointer;
      font-family: sans-serif;
      font-size: 14px;
    }
    .fullscreen-close-btn:hover {
      background: #c7205b;
    }
  ", paste0("#", wrapperId), inputId)))

  # JavaScript to handle the fullscreen logic
  fullscreen_js <- tags$script(HTML(sprintf("
    $(document).ready(function() {
      // Use a short delay to ensure all elements are rendered
      setTimeout(function() {
        var editorId = '%s';
        var wrapperId = '%s_wrapper';
        var wrapper = $('#' + wrapperId);

        // Check if the wrapper element exists
        if (wrapper.length === 0) {
          console.error('Ace Editor wrapper not found:', wrapperId);
          return;
        }

        var fullscreenBtn = $('#' + editorId + '_fullscreen_btn');
        var isFullscreen = false;
        var originalParent = null; // To store where the editor came from

        function enterFullscreen() {
          if (isFullscreen) return;
          console.log('Entering fullscreen for:', editorId);

          originalParent = wrapper.parent(); // Save original location
          $('body').append(wrapper); // Move wrapper to body for fullscreen
          wrapper.addClass('ace-editor-fullscreen');

          // Add a dedicated close button inside the wrapper
          var closeBtn = $('<button type=\"button\" class=\"fullscreen-close-btn\"><i class=\"fas fa-times\"></i> Exit</button>');
          wrapper.append(closeBtn);
          closeBtn.on('click', exitFullscreen); // Attach event handler

          // Add a namespaced resize event listener to the window
          // This ensures the editor resizes when the browser window does.
          $(window).on('resize.aceFullscreen-' + editorId, function() {
            ace.edit(editorId).resize();
          });

          ace.edit(editorId).resize(); // Tell Ace to resize to its new container
          isFullscreen = true;
        }

        function exitFullscreen() {
          if (!isFullscreen) return;
          console.log('Exiting fullscreen for:', editorId);

          // Remove the specific namespaced event listener to avoid memory leaks
          $(window).off('resize.aceFullscreen-' + editorId);

          // Move the editor back to its original parent container
          if (originalParent && originalParent.length) {
            originalParent.append(wrapper);
          } else {
            // Fallback if original parent is lost
            wrapper.remove();
          }
          
          wrapper.removeClass('ace-editor-fullscreen');
          wrapper.find('.fullscreen-close-btn').remove(); // Clean up close button

          ace.edit(editorId).resize(); // Resize back to normal
          isFullscreen = false;
        }

        // Attach the main click handler to the original 'Fullscreen' button
        fullscreenBtn.on('click', function(e) {
          e.preventDefault(); // Prevent any default button behavior
          if (!isFullscreen) {
            enterFullscreen();
          } else {
            exitFullscreen();
          }
        });

        // Add Escape key functionality to exit fullscreen mode
        $(document).on('keydown', function(e) {
          if (e.key === 'Escape' && isFullscreen) {
            exitFullscreen();
          }
        });

      }, 200); // End of setTimeout
    });
  ", inputId, inputId)))

  # Return the complete UI widget as a tagList
  tagList(
    fullscreen_css,
    fullscreen_js,
    editor_container
  )
}


# Alternative: Simple fullscreen button without overlay
aceEditor_simple_fullscreen <- function(inputId, value, mode = "r", theme = "gruvbox", minLines = 6, maxLines = 50, fontSize = 13, height = "auto") {
  
  # Create the fullscreen button
  fullscreen_button <- actionButton(
    paste0(inputId, "_fullscreen_btn"),
    icon = icon("expand"),
    label = "Fullscreen",
    class = "btn-sm btn-outline-secondary mb-2",
    style = "float: right;"
  )
  
  # Create the editor
  editor <- aceEditor(
    inputId,
    value = value,
    mode = mode,
    theme = theme,
    minLines = minLines,
    maxLines = maxLines,
    fontSize = fontSize,
    height = if(height == "auto") "calc(100vh - 300px)" else height,
    autoScrollEditorIntoView = FALSE,
    wordWrap = TRUE,
    showPrintMargin = FALSE,
    highlightActiveLine = FALSE,
    debounce = 750
  )
  
  # Add JavaScript for simple fullscreen toggle
  fullscreen_js <- tags$script(HTML(sprintf("
    $(document).ready(function() {
      var editorId = '%s';
      var fullscreenBtn = $('#' + editorId + '_fullscreen_btn');
      var editorContainer = $('#' + editorId).closest('.shiny-ace-container');
      var isFullscreen = false;
      
      // Wait for Ace editor to be ready
      var checkEditor = setInterval(function() {
        if (typeof ace !== 'undefined' && ace.edit(editorId)) {
          clearInterval(checkEditor);
          setupFullscreen();
        }
      }, 100);
      
      function setupFullscreen() {
        fullscreenBtn.on('click', function() {
          console.log('Simple fullscreen button clicked for:', editorId);
          if (!isFullscreen) {
            // Enter fullscreen
            editorContainer.css({
              'position': 'fixed',
              'top': '0',
              'left': '0',
              'width': '100vw',
              'height': '100vh',
              'z-index': '9999',
              'background': 'white',
              'border': 'none'
            });
            
            $('#' + editorId).css('height', 'calc(100vh - 60px)');
            
            // Update button
            fullscreenBtn.find('i').removeClass('fa-expand').addClass('fa-compress');
            fullscreenBtn.find('span').text('Exit Fullscreen');
            
            isFullscreen = true;
            console.log('Entered simple fullscreen mode');
            
          } else {
            // Exit fullscreen
            editorContainer.css({
              'position': '',
              'top': '',
              'left': '',
              'width': '',
              'height': '',
              'z-index': '',
              'background': '',
              'border': ''
            });
            
            $('#' + editorId).css('height', '');
            
            // Update button
            fullscreenBtn.find('i').removeClass('fa-compress').addClass('fa-expand');
            fullscreenBtn.find('span').text('Fullscreen');
            
            isFullscreen = false;
            console.log('Exited simple fullscreen mode');
          }
          
          // Resize editor
          var editor = ace.edit(editorId);
          if (editor) {
            editor.resize();
          }
        });
        
        // Handle escape key
        $(document).on('keydown', function(e) {
          if (e.key === 'Escape' && isFullscreen) {
            fullscreenBtn.click();
          }
        });
      }
    });
  ", inputId)))
  
  # Return the complete widget
  tagList(
    fullscreen_js,
    div(
      fullscreen_button,
      editor
    )
  )
}

ace_server_functions <- function(ace_input_name) {
  # These functions are generally for more advanced Ace features or if issues with default setup.
  aceAutocomplete(ace_input_name) # Often handled by enableLiveAutocompletion
  aceTooltip(ace_input_name)
  aceAnnotate(ace_input_name) # For adding error/warning markers
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


# Custom downloadButton to prevent default browser download behavior
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Load template files from inst folder
load_templates <- function() {
  # Get the path to the inst/code_template directory
  template_dir <- system.file("code_template", package = "dataPlotter")
  
  if (template_dir == "") {
    stop("Template directory not found. Make sure the package is properly installed.")
  }
  
  # Load all template files
  template_files <- list.files(template_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  
  # Load each template file
  for (file in template_files) {
    source(file, local = FALSE)
  }
  
  cat("Loaded", length(template_files), "template files from", template_dir, "\n")
}

# Load templates in the correct order (mimicking load_templates.R)
load_templates_ordered <- function() {
  template_dir <- system.file("code_template", package = "dataPlotter")
  
  if (template_dir == "") {
    stop("Template directory not found. Make sure the package is properly installed.")
  }
  
  # Define the order in which templates should be loaded
  template_order <- c(
    "data_import_preprocessing",
    "data_import_postprocessing", 
    "plot_data_processing",
    "text_output",
    "table_output",
    "interactive_plot",
    "static_plot_modules/data_processing",
    "static_plot_modules/base_setup",
    "static_plot_modules/themes_styling",
    "static_plot_modules/statistical_overlays",
    "static_plot_modules/grid_axes",
    "static_plot_modules/faceting_final",
    "static_plot",
    "final_conditional",
    "combined_template",
    "data_table_display",
    "combined_data_summary",
    "combined_data_sample",
    "helper_code",
    "downloader_code"
  )
  
  # Load templates in the specified order
  for (template in template_order) {
    file_path <- file.path(template_dir, paste0(template, ".R"))
    if (file.exists(file_path)) {
      source(file_path, local = FALSE)
      cat("Loaded:", template, "\n")
    } else {
      warning("Template file not found:", file_path)
    }
  }
  
  cat("All templates loaded successfully from", template_dir, "\n")
}

# Alternative: Load specific template files
load_specific_templates <- function(template_names = NULL) {
  template_dir <- system.file("code_template", package = "dataPlotter")
  
  if (template_dir == "") {
    stop("Template directory not found.")
  }
  
  if (is.null(template_names)) {
    # Load all templates
    load_templates()
    return()
  }
  
  # Load specific templates
  for (name in template_names) {
    file_path <- file.path(template_dir, paste0(name, ".R"))
    if (file.exists(file_path)) {
      source(file_path, local = FALSE)
      cat("Loaded template:", name, "\n")
    } else {
      warning("Template file not found:", file_path)
    }
  }
}

# Function to get template content as character
get_template_content <- function(template_name) {
  template_dir <- system.file("code_template", package = "dataPlotter")
  file_path <- file.path(template_dir, paste0(template_name, ".R"))
  
  if (!file.exists(file_path)) {
    stop("Template file not found:", file_path)
  }
  
  readLines(file_path, warn = FALSE)
}

# Function to list available templates
list_templates <- function() {
  template_dir <- system.file("code_template", package = "dataPlotter")
  
  if (template_dir == "") {
    return(character(0))
  }
  
  files <- list.files(template_dir, pattern = "\\.R$", recursive = TRUE)
  gsub("\\.R$", "", files)
}

# Utility functions for working with package files
get_package_file_path <- function(file_path) {
  # Get the full path to a file in the package
  system.file(file_path, package = "dataPlotter")
}

read_package_file <- function(file_path) {
  # Read a file from the package
  full_path <- get_package_file_path(file_path)
  if (full_path == "") {
    stop("File not found in package:", file_path)
  }
  readLines(full_path, warn = FALSE)
}

list_package_files <- function(directory = "") {
  # List all files in a package directory
  package_dir <- system.file(directory, package = "dataPlotter")
  if (package_dir == "") {
    return(character(0))
  }
  list.files(package_dir, recursive = TRUE)
}

# Function to check if a file exists in the package
package_file_exists <- function(file_path) {
  full_path <- get_package_file_path(file_path)
  full_path != "" && file.exists(full_path)
}

# Function to get package data directory
get_package_data_dir <- function() {
  system.file("data", package = "dataPlotter")
}

# Function to get package extdata directory
get_package_extdata_dir <- function() {
  system.file("extdata", package = "dataPlotter")
}

# === Batch Download System ===

#' Create a temporary directory for batch downloads
create_batch_download_dir <- function() {
  # Create a unique temp directory for this batch
  batch_id <- paste0("batch_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", substr(as.character(runif(1)), 3, 8))
  temp_dir <- file.path(tempdir(), batch_id)

  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }

  return(temp_dir)
}

#' Generate plot file for batch download
generate_plot_file <- function(plot_obj, filename, format = "png", width = 12, height = 8, dpi = 300) {
  tryCatch({
    if (format == "html" && inherits(plot_obj, "htmlwidget")) {
      # Interactive plots as HTML
      htmlwidgets::saveWidget(plot_obj, filename, selfcontained = TRUE)

    } else if (format == "html" && inherits(plot_obj, "datatables")) {
      # DataTable as HTML
      htmlwidgets::saveWidget(plot_obj, filename, selfcontained = TRUE)

    } else if (format %in% c("png", "jpeg", "pdf", "svg") && inherits(plot_obj, "ggplot")) {
      # Static plots in various formats
      ggplot2::ggsave(
        filename = filename,
        plot = plot_obj,
        device = format,
        width = width,
        height = height,
        dpi = dpi,
        bg = "white"
      )

    } else if (format == "json") {
      # Extract data from plot
      if (inherits(plot_obj, "ggplot")) {
        plot_data <- plot_obj$data
        if (!is.null(plot_data)) {
          jsonlite::write_json(plot_data, filename, pretty = TRUE)
        } else {
          writeLines('{"error": "No data available in plot object"}', filename)
        }
      } else if (inherits(plot_obj, "htmlwidget")) {
        if ("plotly" %in% class(plot_obj)) {
          plot_data <- plot_obj$x$data
          jsonlite::write_json(plot_data, filename, pretty = TRUE)
        } else {
          writeLines('{"error": "Data extraction not supported for this widget type"}', filename)
        }
      } else {
        writeLines('{"error": "Unsupported plot type for data extraction"}', filename)
      }

    } else if (format == "txt") {
      # Save as text representation
      plot_text <- capture.output(print(plot_obj))
      writeLines(plot_text, filename)

    } else {
      stop("Unsupported format: ", format)
    }

    return(TRUE)

  }, error = function(e) {
    warning("Error generating plot file ", filename, ": ", e$message)
    return(FALSE)
  })
}

#' Generate filename for batch download
generate_batch_filename <- function(plotter_id, title = NULL, caption = NULL, format = "png", timestamp = TRUE) {
  # Base filename from plotter ID
  base_name <- plotter_id

  # Add title if available
  if (!is.null(title) && nzchar(title)) {
    title_clean <- gsub("[^a-zA-Z0-9 ]", "", title)
    title_words <- strsplit(title_clean, "\\s+")[[1]]
    title_short <- paste(head(title_words, 3), collapse = "_")
    if (nzchar(title_short)) {
      base_name <- paste0(base_name, "_", title_short)
    }
  }

  # Add caption if available
  if (!is.null(caption) && nzchar(caption)) {
    caption_clean <- gsub("[^a-zA-Z0-9 ]", "", caption)
    caption_words <- strsplit(caption_clean, "\\s+")[[1]]
    caption_short <- paste(head(caption_words, 2), collapse = "_")
    if (nzchar(caption_short)) {
      base_name <- paste0(base_name, "_", caption_short)
    }
  }

  # Add timestamp
  if (timestamp) {
    timestamp_str <- format(Sys.time(), "%H%M%S")
    base_name <- paste0(base_name, "_", timestamp_str)
  }

  # Add extension
  paste0(base_name, ".", format)
}

#' Process a single plotter for batch download
process_plotter_for_batch <- function(plotter_id, temp_dir, session, progress_callback = NULL) {
  tryCatch({
    # Update progress if callback provided
    if (!is.null(progress_callback)) {
      progress_callback(plotter_id, "starting", "Preparing plot data...")
    }

    # Get plot object and data from the plotter module
    # This requires access to the plotter's reactive values
    plot_obj <- NULL
    plot_data <- NULL

    # We need to access the plotter's internal state
    # This will require some restructuring of how plotter data is accessed

    if (!is.null(progress_callback)) {
      progress_callback(plotter_id, "generating", "Generating plot...")
    }

    # For now, we'll create a placeholder structure
    # In a real implementation, this would access the actual plotter data

    success <- TRUE
    message <- "Plot processed successfully"

    if (!is.null(progress_callback)) {
      progress_callback(plotter_id, "complete", message)
    }

    return(list(
      plotter_id = plotter_id,
      success = success,
      message = message,
      filename = NULL
    ))

  }, error = function(e) {
    error_msg <- paste("Error processing plotter", plotter_id, ":", e$message)

    if (!is.null(progress_callback)) {
      progress_callback(plotter_id, "error", error_msg)
    }

    return(list(
      plotter_id = plotter_id,
      success = FALSE,
      message = error_msg,
      filename = NULL
    ))
  })
}

#' Create ZIP archive from batch download directory
create_batch_zip <- function(temp_dir, zip_filename) {
  tryCatch({
    # Get all files in the temp directory
    files_to_zip <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)

    if (length(files_to_zip) == 0) {
      warning("No files to zip in directory: ", temp_dir)
      return(FALSE)
    }

    # Create ZIP archive
    utils::zip(zip_filename, files_to_zip, extras = "-j")  # -j to store just filenames

    return(TRUE)

  }, error = function(e) {
    warning("Error creating ZIP archive: ", e$message)
    return(FALSE)
  })
}

#' Clean up temporary batch download files
cleanup_batch_files <- function(temp_dir) {
  tryCatch({
    if (dir.exists(temp_dir)) {
      unlink(temp_dir, recursive = TRUE)
    }
    return(TRUE)
  }, error = function(e) {
    warning("Error cleaning up temp directory ", temp_dir, ": ", e$message)
    return(FALSE)
  })
}

# Simple test function for debugging fullscreen
aceEditor_test_fullscreen <- function(inputId, value = "# Test editor\n# Click the fullscreen button to test", height = "300px") {
  
  # Create a simple fullscreen button
  fullscreen_button <- actionButton(
    paste0(inputId, "_test_fullscreen_btn"),
    icon = icon("expand"),
    label = "Test Fullscreen",
    class = "btn-sm btn-primary mb-2"
  )
  
  # Create the editor
  editor <- aceEditor(
    inputId,
    value = value,
    mode = "r",
    theme = "gruvbox",
    height = height,
    fontSize = 13
  )
  
  # Simple JavaScript for testing
  test_js <- tags$script(HTML(sprintf("
    $(document).ready(function() {
      console.log('Test fullscreen setup for:', '%s');
      
      var testBtn = $('#' + '%s' + '_test_fullscreen_btn');
      var editorContainer = $('#' + '%s').closest('.shiny-ace-container');
      
      testBtn.on('click', function() {
        console.log('Test button clicked!');
        alert('Button clicked! Editor ID: ' + '%s');
        
        // Simple test - just change background color
        editorContainer.css('background-color', 'yellow');
        setTimeout(function() {
          editorContainer.css('background-color', '');
        }, 2000);
      });
    });
  ", inputId, inputId, inputId, inputId)))
  
  # Return the test widget
  tagList(
    test_js,
    div(
      fullscreen_button,
      editor
    )
  )
}