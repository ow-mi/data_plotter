# Performance optimization utilities for Shinylive projects

#' Clean up Shinylive export directory
#' @param export_dir Path to Shinylive export directory
cleanup_shinylive_export <- function(export_dir = "shinylive_export") {
  if (!dir.exists(export_dir)) {
    stop("Shinylive export directory not found: ", export_dir)
  }
  
  # Remove unnecessary files
  files_to_remove <- c(
    "shinylive-sw.js",  # Service worker not needed for static deployment
    "edit/"            # Edit directory not needed for production
  )
  
  for (file in files_to_remove) {
    file_path <- file.path(export_dir, file)
    if (file.exists(file_path)) {
      if (dir.exists(file_path)) {
        unlink(file_path, recursive = TRUE)
      } else {
        file.remove(file_path)
      }
      cat("Removed:", file, "\n")
    }
  }
  
  # Optimize app.json if it exists
  app_json_path <- file.path(export_dir, "app.json")
  if (file.exists(app_json_path)) {
    optimize_app_json(app_json_path)
  }
  
  cat("Shinylive export cleanup completed\n")
}

#' Optimize app.json file size
#' @param app_json_path Path to app.json file
optimize_app_json <- function(app_json_path) {
  if (!file.exists(app_json_path)) {
    stop("app.json file not found: ", app_json_path)
  }
  
  # Read and parse JSON
  app_data <- jsonlite::fromJSON(app_json_path, simplifyVector = FALSE)
  
  # Remove unnecessary content
  for (i in seq_along(app_data)) {
    file_info <- app_data[[i]]
    
    # Remove large autocomplete files if not needed
    if (grepl("autocomplete", file_info$name, ignore.case = TRUE)) {
      if (nchar(file_info$content) > 100000) {  # If larger than 100KB
        file_info$content <- "# Autocomplete data removed for performance\n"
        app_data[[i]] <- file_info
        cat("Optimized large autocomplete file:", file_info$name, "\n")
      }
    }
    
    # Remove comments from R files to reduce size
    if (grepl("\\.R$", file_info$name)) {
      # Remove comments and extra whitespace
      content <- file_info$content
      content <- gsub("#.*$", "", content, perl = TRUE)  # Remove comments
      content <- gsub("\\s+$", "", content, perl = TRUE)  # Remove trailing whitespace
      content <- gsub("^\\s+", "", content, perl = TRUE)  # Remove leading whitespace
      content <- gsub("\n{3,}", "\n\n", content)  # Reduce multiple newlines
      file_info$content <- content
      app_data[[i]] <- file_info
    }
  }
  
  # Write optimized JSON
  jsonlite::write_json(app_data, app_json_path, pretty = FALSE, auto_unbox = TRUE)
  
  # Report size reduction
  original_size <- file.size(app_json_path)
  cat("Optimized app.json size:", round(original_size / 1024, 1), "KB\n")
}

#' Generate optimized Shinylive export
#' @param app_dir Application directory
#' @param export_dir Export directory
generate_optimized_export <- function(app_dir = ".", export_dir = "shinylive_export_optimized") {
  # Create optimized export directory
  if (dir.exists(export_dir)) {
    unlink(export_dir, recursive = TRUE)
  }
  dir.create(export_dir)
  
  # Copy essential files only
  essential_files <- c("index.html", "app.json")
  for (file in essential_files) {
    src <- file.path("shinylive_export", file)
    dst <- file.path(export_dir, file)
    if (file.exists(src)) {
      file.copy(src, dst)
    }
  }
  
  # Copy only essential shinylive files
  shinylive_src <- file.path("shinylive_export", "shinylive")
  shinylive_dst <- file.path(export_dir, "shinylive")
  if (dir.exists(shinylive_src)) {
    dir.create(shinylive_dst)
    
    # Copy only essential files
    essential_shinylive <- c(
      "shinylive.js",
      "shinylive.css",
      "webr/",
      "load-shinylive-sw.js"
    )
    
    for (file in essential_shinylive) {
      src <- file.path(shinylive_src, file)
      dst <- file.path(shinylive_dst, file)
      if (file.exists(src)) {
        if (dir.exists(src)) {
          file.copy(src, dst, recursive = TRUE)
        } else {
          file.copy(src, dst)
        }
      }
    }
  }
  
  # Optimize the copied app.json
  optimize_app_json(file.path(export_dir, "app.json"))
  
  cat("Optimized Shinylive export created in:", export_dir, "\n")
}

#' Analyze Shinylive export size
#' @param export_dir Export directory
analyze_export_size <- function(export_dir = "shinylive_export") {
  if (!dir.exists(export_dir)) {
    stop("Export directory not found: ", export_dir)
  }
  
  files <- list.files(export_dir, recursive = TRUE, full.names = TRUE)
  file_sizes <- file.size(files)
  file_info <- data.frame(
    file = files,
    size_kb = round(file_sizes / 1024, 1),
    size_mb = round(file_sizes / 1024^2, 2)
  )
  
  # Sort by size
  file_info <- file_info[order(-file_info$size_kb), ]
  
  cat("Shinylive Export Size Analysis:\n")
  cat("Total files:", nrow(file_info), "\n")
  cat("Total size:", round(sum(file_sizes) / 1024^2, 2), "MB\n\n")
  
  cat("Largest files:\n")
  print(head(file_info, 10))
  
  return(file_info)
}

#' Remove unused dependencies from DESCRIPTION
#' @param description_file Path to DESCRIPTION file
cleanup_dependencies <- function(description_file = "DESCRIPTION") {
  if (!file.exists(description_file)) {
    stop("DESCRIPTION file not found: ", description_file)
  }
  
  # Read current dependencies
  desc <- readLines(description_file)
  
  # Remove unused packages (customize based on your analysis)
  unused_packages <- c(
    "shinyTime",    # If not using time inputs
    "nanoparquet",  # If not using nanoparquet
    "paletteer",    # If not using paletteer
    "munsell"       # If not using munsell colors
  )
  
  for (pkg in unused_packages) {
    desc <- gsub(paste0("\\s*", pkg, ",\\s*"), "", desc)
    desc <- gsub(paste0("\\s*", pkg, "\\s*"), "", desc)
  }
  
  # Write cleaned DESCRIPTION
  writeLines(desc, description_file)
  cat("Cleaned DESCRIPTION file\n")
}

# Example usage:
# cleanup_shinylive_export()
# optimize_app_json("shinylive_export/app.json")
# generate_optimized_export()
# analyze_export_size()
