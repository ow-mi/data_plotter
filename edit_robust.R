# Robust shinylive export script with complete dependency handling

export_name <- "shinylive_export"

# Function to get all dependencies recursively
get_all_dependencies <- function(packages) {
  all_deps <- character(0)
  
  for (pkg in packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      # Get direct dependencies
      deps <- tools::package_dependencies(pkg, recursive = TRUE)[[1]]
      if (!is.null(deps)) {
        all_deps <- c(all_deps, deps)
      }
    }
  }
  
  # Remove duplicates and base packages
  base_packages <- c("base", "compiler", "datasets", "graphics", "grDevices", 
                     "grid", "methods", "parallel", "splines", "stats", "stats4", 
                     "tcltk", "tools", "translations", "utils")
  
  unique_deps <- unique(c(packages, all_deps))
  unique_deps[!unique_deps %in% base_packages]
}

# List of all packages used in your app
main_packages <- c(
  "shiny", "shinyTime", "shinyjqui", "bslib", "shinyAce", "shinyFiles",
  "stringr", "data.table", "DT", "R.utils", "ggplot2", "plotly", 
  "lubridate", "jsonlite", "fasttime", "readxl", "fst", "nanoparquet",
  "scattermore", "purrr", "promises", "future", "rstudioapi", "skimr",
  "htmlwidgets", "spsComps", "base64enc"
)

# Add known missing dependencies manually
additional_deps <- c(
  "munsell", "scales", "colorspace", "farver", "labeling", "lifecycle",
  "RColorBrewer", "viridisLite", "isoband", "mgcv", "MASS", "tibble",
  "gtable", "withr", "digest", "rlang", "cli", "glue", "fansi", "utf8",
  "pillar", "pkgconfig", "vctrs", "crayon", "ellipsis"
)

cat("Calculating complete dependency list...\n")
all_packages <- get_all_dependencies(main_packages)
all_packages <- unique(c(all_packages, additional_deps))

cat("Total packages to include:", length(all_packages), "\n")
cat("Packages:", paste(head(all_packages, 10), collapse = ", "), "...\n")

# Remove existing export directory
if (dir.exists(export_name)) {
  cat("Removing existing export directory...\n")
  unlink(export_name, recursive = TRUE)
}

# Install/update shinylive
if (!requireNamespace("shinylive", quietly = TRUE)) {
  cat("Installing shinylive...\n")
  install.packages("shinylive", repos = "https://cran.r-project.org")
}

library(shinylive)

# Create a custom app.R for shinylive with better error handling
app_content <- '
# Simplified app.R for shinylive with better error handling

# Load packages with error handling
safe_library <- function(pkg) {
  result <- tryCatch({
    library(pkg, character.only = TRUE)
    TRUE
  }, error = function(e) {
    cat("Warning: Could not load package", pkg, ":", e$message, "\\n")
    FALSE
  })
  return(result)
}

# Core packages (load these first)
required_packages <- c("shiny", "bslib")
for (pkg in required_packages) {
  if (!safe_library(pkg)) {
    stop("Critical package ", pkg, " could not be loaded")
  }
}

# Optional packages (continue even if these fail)
optional_packages <- c(
  "shinyTime", "shinyjqui", "shinyAce", "shinyFiles", "stringr",
  "data.table", "DT", "ggplot2", "plotly", "lubridate", "jsonlite",
  "fasttime", "readxl", "fst", "nanoparquet", "scattermore", "purrr",
  "htmlwidgets", "spsComps"
)

for (pkg in optional_packages) {
  safe_library(pkg)
}

# Simplified future setup for shinylive
if (requireNamespace("future", quietly = TRUE)) {
  tryCatch({
    library(future)
    # Use synchronous plan for shinylive compatibility
    plan(sequential)
  }, error = function(e) {
    cat("Warning: Future setup failed, continuing without parallel processing\\n")
  })
}

# Source files with error handling
source_safe <- function(file) {
  if (file.exists(file)) {
    tryCatch({
      source(file)
      cat("Loaded:", file, "\\n")
    }, error = function(e) {
      cat("Warning: Could not load", file, ":", e$message, "\\n")
    })
  } else {
    cat("Warning: File not found:", file, "\\n")
  }
}

source_safe("./functions.R")
source_safe("./r_code_.R")

# Source module files
module_files <- c(
  "./modules/module_data_table_ui.R",
  "./modules/module_data_table_server.R", 
  "./modules/module_data_combiner_ui.R",
  "./modules/module_data_combiner_server.R",
  "./modules/module_importer_ui.R",
  "./modules/module_importer_server.R",
  "./modules/module_plotter_ui.R",
  "./modules/module_plotter_server.R"
)

for (file in module_files) {
  source_safe(file)
}
'

# Get the rest of the original app.R content (UI and server)
original_app <- readLines("app.R")
start_ui <- grep("# UI Definition", original_app)
if (length(start_ui) > 0) {
  ui_and_server <- paste(original_app[start_ui[1]:length(original_app)], collapse = "\n")
  app_content <- paste(app_content, ui_and_server, sep = "\n")
}

# Write the modified app.R
writeLines(app_content, "app_temp.R")

# Try the export
tryCatch({
  cat("Attempting shinylive export...\n")
  # Use the temporary app file
  file.copy("app.R", "app_backup.R")  # Backup original
  file.copy("app_temp.R", "app.R")    # Use modified version
  
  shinylive::export("./", export_name)
  cat("Export successful!\n")
  
}, error = function(e) {
  cat("Export failed:", e$message, "\n")
  
  # Restore original app.R
  if (file.exists("app_backup.R")) {
    file.copy("app_backup.R", "app.R", overwrite = TRUE)
  }
  
  stop("Shinylive export failed")
  
}, finally = {
  # Cleanup
  if (file.exists("app_temp.R")) file.remove("app_temp.R")
  if (file.exists("app_backup.R")) file.remove("app_backup.R")
}) 