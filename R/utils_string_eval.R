#' String Evaluation Utility Functions
#'
#' @import stringr
#' @noRd

#' Evaluate R code within strings for dynamic text generation
string_eval <- function(text, env = parent.frame(), safe_mode = FALSE, max_length = 1000) {
  if (is.null(text) || is.na(text) || text == "") {
    return(text)
  }

  # Improved regex pattern that handles escaped quotes better
  # Matches quoted strings but avoids matching escaped quotes within
  pattern <- '(["\'])((?:\\\\.|(?!(?<!\\\\)\\1).)*?)\\1'

  # Extract all quoted sections with better handling
  matches <- gregexpr(pattern, text, perl = TRUE)
  match_data <- regmatches(text, matches)[[1]]

  if (length(match_data) == 0) {
    # No quoted code found, return original text
    return(text)
  }

  # Process each quoted section
  result_text <- text
  processed_codes <- list() # Track processed code to avoid duplicate processing

  for (i in seq_along(match_data)) {
    quoted_code <- match_data[i]

    # Skip if we've already processed this exact code
    if (quoted_code %in% names(processed_codes)) {
      next
    }

    # Extract the content between quotes, handling escaped characters
    code_match <- regexec(pattern, quoted_code, perl = TRUE)
    code <- regmatches(quoted_code, code_match)[[1]][3] # Get the captured group

    # Unescape quotes within the code
    code <- gsub('\\\\(["\'])', '\\1', code)

    # Skip empty code
    if (is.null(code) || code == "") {
      next
    }

    tryCatch({
      # Security check in safe mode
      if (safe_mode) {
        # Block potentially dangerous operations
        dangerous_patterns <- c(
          "system\\s*\\(",
          "shell\\s*\\(",
          "eval\\s*\\(",
          "parse\\s*\\(",
          "source\\s*\\(",
          "file\\.remove\\s*\\(",
          "unlink\\s*\\(",
          "setwd\\s*\\(",
          "\\.Call\\s*\\(",
          "\\.External\\s*\\(",
          "\\.C\\s*\\(",
          "\\.Fortran\\s*\\("
        )

        if (any(sapply(dangerous_patterns, function(p) grepl(p, code, ignore.case = TRUE)))) {
          stop("Potentially unsafe code detected")
        }
      }

      # Evaluate the code in the provided environment
      eval_result <- eval(parse(text = code), envir = env)

      # Improved result conversion with better type handling
      result_str <- format_eval_result(eval_result, max_length)

      # Replace the quoted code with the result in the text
      result_text <- gsub(
        pattern = fixed(quoted_code),
        replacement = result_str,
        x = result_text,
        fixed = TRUE
      )

      # Cache the result to avoid reprocessing identical code
      processed_codes[[quoted_code]] <- result_str

    }, error = function(e) {
      # More informative error message
      error_msg <- sprintf("[Error evaluating '%s': %s]", substr(code, 1, 50), e$message)

      # If error message is too long, truncate it
      if (nchar(error_msg) > 200) {
        error_msg <- paste0(substr(error_msg, 1, 197), "...]")
      }

      result_text <- gsub(
        pattern = fixed(quoted_code),
        replacement = error_msg,
        x = result_text,
        fixed = TRUE
      )

      # Cache the error result too
      processed_codes[[quoted_code]] <- error_msg
    })
  }

  return(result_text)
}

#' Helper function to format evaluation results
format_eval_result <- function(result, max_length = 1000) {
  if (is.null(result)) {
    return("NULL")
  }

  if (is.na(result) && length(result) == 1) {
    return("NA")
  }

  # Handle different data types
  if (is.factor(result)) {
    result <- as.character(result)
  } else if (is.logical(result)) {
    result <- ifelse(result, "TRUE", "FALSE")
  } else if (is.list(result)) {
    # Convert list to a readable format
    if (length(result) == 0) {
      return("list()")
    } else {
      items <- sapply(result, function(x) {
        if (is.null(x)) "NULL" else as.character(x)
      })
      return(sprintf("list(%s)", paste(items, collapse = ", ")))
    }
  } else if (is.data.frame(result)) {
    # For data frames, show dimensions
    return(sprintf("data.frame(%d rows, %d cols)", nrow(result), ncol(result)))
  } else if (is.matrix(result)) {
    return(sprintf("matrix(%d x %d)", nrow(result), ncol(result)))
  }

  # Convert to character
  if (is.vector(result) && length(result) > 1) {
    # Join multiple values with commas, but limit total length
    result_str <- paste(result, collapse = ", ")

    if (nchar(result_str) > max_length) {
      # Truncate and add indicator
      result_str <- paste0(substr(result_str, 1, max_length - 3), "...")
    }

    return(result_str)
  } else {
    result_str <- as.character(result)

    # Handle very long single results
    if (nchar(result_str) > max_length) {
      result_str <- paste0(substr(result_str, 1, max_length - 3), "...")
    }

    return(result_str)
  }
}

#' Safe version for user inputs (blocks system calls and file operations)
string_eval_safe <- function(text, env = parent.frame(), max_length = 1000) {
  string_eval(text, env, safe_mode = TRUE, max_length = max_length)
}

#' Version that preserves original quotes and formatting
string_eval_preserve <- function(text, env = parent.frame(), safe_mode = FALSE) {
  if (is.null(text) || is.na(text) || text == "") {
    return(text)
  }

  # Pattern that captures quotes, content, and closing quote separately
  pattern <- '(["\'])((?:\\\\.|(?!(?<!\\\\)\\1).)*?)\\1'

  matches <- gregexpr(pattern, text, perl = TRUE)
  match_data <- regmatches(text, matches)[[1]]

  if (length(match_data) == 0) {
    return(text)
  }

  result_text <- text

  for (quoted_code in match_data) {
    # Extract quote type and content
    code_match <- regexec(pattern, quoted_code, perl = TRUE)
    parts <- regmatches(quoted_code, code_match)[[1]]

    if (length(parts) >= 4) {
      quote_type <- parts[2]
      code <- parts[3]

      # Unescape quotes within the code
      code <- gsub('\\\\(["\'])', '\\1', code)

      if (code != "") {
        tryCatch({
          if (safe_mode) {
            # Same security checks as before
            dangerous_patterns <- c(
              "system\\s*\\(", "shell\\s*\\(", "eval\\s*\\(",
              "parse\\s*\\(", "source\\s*\\(", "file\\.remove\\s*\\(",
              "unlink\\s*\\(", "setwd\\s*\\("
            )

            if (any(sapply(dangerous_patterns, function(p) grepl(p, code, ignore.case = TRUE)))) {
              stop("Potentially unsafe code detected")
            }
          }

          eval_result <- eval(parse(text = code), envir = env)
          result_str <- format_eval_result(eval_result)

          # Replace with same quote type
          replacement <- paste0(quote_type, result_str, quote_type)
          result_text <- gsub(fixed(quoted_code), replacement, result_text, fixed = TRUE)

        }, error = function(e) {
          error_msg <- sprintf("Error: %s", e$message)
          replacement <- paste0(quote_type, error_msg, quote_type)
          result_text <- gsub(fixed(quoted_code), replacement, result_text, fixed = TRUE)
        })
      }
    }
  }

  return(result_text)
}

#' Process text input with evaluation
process_text_input <- function(text_input, env = parent.frame(), safe_mode = TRUE) {
  if (safe_mode) {
    string_eval_safe(text_input, env)
  } else {
    string_eval(text_input, env)
  }
}