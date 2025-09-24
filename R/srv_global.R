#' Global Server Function
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @import shiny
#' @importFrom shiny reactiveValuesToList isolate downloadHandler
#' @importFrom jsonlite write_json
#' @importFrom tools file_path_sans_ext
#' @noRd
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

  # Template download handler
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
        cat("Error saving template:", e$message, "\n")
        showNotification(paste("Error saving template:", e$message), type = "error")
      })
    }
  )

  # Call module server functions here
  # Example: mod_importer_server("data_import_module_1", data = reactive_data)
  # These will be called from the UI components

  # Cheatsheet helper module
  srv_cheatsheets("cheatsheets")
}