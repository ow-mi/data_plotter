#' Code Sharing Module Functions
#'
#' @importFrom stringr str_trim
#' @noRd

#' Code sharing helper function for plotters and importers
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

  if (is.null(code) || str_trim(code) == "") {
    return(paste0("# No code found in ", plotter_id, " for '", code_type, "' plot type\n# Available types: 'text', 'static', 'interactive', 'table', 'final', 'process'"))
  }

  return(code)
}

#' Helper function to access code from other importers
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

  if (is.null(code) || str_trim(code) == "") {
    return(paste0("# No code found in ", importer_id, " for '", code_type, "' processing\n# Available types: 'pre', 'post'"))
  }

  return(code)
}