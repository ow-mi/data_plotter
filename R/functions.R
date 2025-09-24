# functions.R
# Essential functions that need to be loaded at startup
# Other functions have been moved to their respective modules:
# - UI functions: R/utils_ui.R
# - Data functions: R/utils_data.R
# - String evaluation: R/utils_string_eval.R
# - ACE Editor: R/app_ace_editor.R
# - Template loading: R/app_template_loader.R
# - Code sharing: R/app_code_sharing.R
# - Batch download: R/app_batch_download.R
# - Test functions: R/app_test_functions.R

# === Enhanced Filtering System Helper Functions ===

#' Helper function for automation: create filter strings programmatically
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

#' Helper function for automation: create dynamic filter with code evaluation
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

#' Updated automation functions for new filtering system
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

#' Create batch filters with semicolon separation
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

#' Automation function for updating combined data filter
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

#' Helper function for creating automation filter strings
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
