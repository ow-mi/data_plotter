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
string_eval <- function(text, env = parent.frame()) {
  if (is.null(text) || is.na(text) || text == "") {
    return(text)
  }
  
  # Find patterns like "code" or 'code' (quoted R code to evaluate)
  pattern <- '["\'](.*?)["\']'
  
  # Extract all quoted sections
  matches <- gregexpr(pattern, text, perl = TRUE)
  match_data <- regmatches(text, matches)[[1]]
  
  if (length(match_data) == 0) {
    # No quoted code found, return original text
    return(text)
  }
  
  # Process each quoted section
  result_text <- text
  for (quoted_code in match_data) {
    # Remove quotes to get the actual code
    code <- gsub('^["\']|["\']$', '', quoted_code)
    
    tryCatch({
      # Evaluate the code in the provided environment
      eval_result <- eval(parse(text = code), envir = env)
      
      # Convert result to character and handle vectors
      if (is.vector(eval_result) && length(eval_result) > 1) {
        # Join multiple values with commas
        result_str <- paste(eval_result, collapse = ", ")
      } else {
        result_str <- as.character(eval_result)
      }
      
      # Replace the quoted code with the result in the text
      result_text <- gsub(pattern = fixed(quoted_code), replacement = result_str, x = result_text, fixed = TRUE)
      
    }, error = function(e) {
      # If evaluation fails, replace with error message
      error_msg <- paste0("[Error: ", e$message, "]")
      result_text <<- gsub(pattern = fixed(quoted_code), replacement = error_msg, x = result_text, fixed = TRUE)
    })
  }
  
  return(result_text)
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