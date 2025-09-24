#' Template Loader Module Functions
#'
#' @importFrom jsonlite write_json
#' @noRd

#' Load template files from inst folder
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

#' Load templates in the correct order (mimicking load_templates.R)
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

#' Alternative: Load specific template files
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

#' Function to get template content as character
get_template_content <- function(template_name) {
  template_dir <- system.file("code_template", package = "dataPlotter")
  file_path <- file.path(template_dir, paste0(template_name, ".R"))

  if (!file.exists(file_path)) {
    stop("Template file not found:", file_path)
  }

  readLines(file_path, warn = FALSE)
}

#' Function to list available templates
list_templates <- function() {
  template_dir <- system.file("code_template", package = "dataPlotter")

  if (template_dir == "") {
    return(character(0))
  }

  files <- list.files(template_dir, pattern = "\\.R$", recursive = TRUE)
  gsub("\\.R$", "", files)
}

#' Utility functions for working with package files
get_package_file_path <- function(file_path) {
  # Get the full path to a file in the package
  system.file(file_path, package = "dataPlotter")
}

#' Read a file from the package
read_package_file <- function(file_path) {
  # Read a file from the package
  full_path <- get_package_file_path(file_path)
  if (full_path == "") {
    stop("File not found in package:", file_path)
  }
  readLines(full_path, warn = FALSE)
}

#' List all files in a package directory
list_package_files <- function(directory = "") {
  # List all files in a package directory
  package_dir <- system.file(directory, package = "dataPlotter")
  if (package_dir == "") {
    return(character(0))
  }
  list.files(package_dir, recursive = TRUE)
}

#' Function to check if a file exists in the package
package_file_exists <- function(file_path) {
  full_path <- get_package_file_path(file_path)
  full_path != "" && file.exists(full_path)
}

#' Function to get package data directory
get_package_data_dir <- function() {
  system.file("data", package = "dataPlotter")
}

#' Function to get package extdata directory
get_package_extdata_dir <- function() {
  system.file("extdata", package = "dataPlotter")
}