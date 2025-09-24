#' ACE Editor Module Functions
#'
#' @import shiny
#' @importFrom shinyAce aceEditor
#' @importFrom shiny actionButton icon tagList tags div HTML
#' @noRd

#' Generate function lists for Ace Editor autocompletion (run this once to create the JSON)
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
  }, error = function(e) {
    message(paste("Error generating autocomplete list:", e$message))
  })
}

#' Call it once if the file doesn't exist or needs updating
#' This function is called at package load time to generate autocomplete data
.onLoad <- function(libname, pkgname) {
  if (!file.exists("www/r_functions_autocomplete.json")) {
    generate_function_lists()
  }
}

#' Pre-configured ACE editor with sensible defaults
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

#' Enhanced ACE editor with fullscreen capability
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

#' Alternative: Simple fullscreen button without overlay
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

#' ACE server functions for advanced features
ace_server_functions <- function(ace_input_name) {
  # These functions are generally for more advanced Ace features or if issues with default setup.
  aceAutocomplete(ace_input_name) # Often handled by enableLiveAutocompletion
  aceTooltip(ace_input_name)
  aceAnnotate(ace_input_name) # For adding error/warning markers
}