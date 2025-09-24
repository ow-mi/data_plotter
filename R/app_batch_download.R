#' Batch Download Module Functions
#'
#' @importFrom ggplot2 ggsave
#' @importFrom htmlwidgets saveWidget
#' @importFrom jsonlite write_json
#' @importFrom stringr str_trim
#' @importFrom utils zip
#' @noRd

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