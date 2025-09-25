#' Data Manipulation Utility Functions
#'
#' @import data.table
#' @import stringr
#' @noRd

#' Filter data.table by including patterns
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

#' Filter data.table by excluding patterns
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

#' Rename values in data.table column
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

#' Extract values from filename using regex patterns
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