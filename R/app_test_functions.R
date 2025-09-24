#' Test and Demonstration Functions
#'
#' @import shiny
#' @importFrom shinyAce aceEditor
#' @importFrom shiny actionButton icon div tagList tags HTML
#' @noRd

#' Test and demonstration functions for string_eval
test_string_eval <- function() {
  # Create a test environment with some variables
  test_env <- new.env()
  test_env$x <- 42
  test_env$name <- "Alice"
  test_env$scores <- c(85, 92, 78, 96)
  test_env$my_list <- list(a = 1, b = "hello", c = TRUE)
  test_env$my_df <- data.frame(id = 1:3, value = c("A", "B", "C"))

  cat("=== Testing string_eval improvements ===\n\n")

  # Test 1: Basic variable evaluation
  text1 <- "Hello 'name', your score is 'x' points!"
  result1 <- string_eval(text1, test_env)
  cat("Test 1 - Basic variables:\n")
  cat("Input: ", text1, "\n")
  cat("Output:", result1, "\n\n")

  # Test 2: Vector handling
  text2 <- "Your scores are: 'paste(scores, collapse=\", \")'"
  result2 <- string_eval(text2, test_env)
  cat("Test 2 - Vector handling:\n")
  cat("Input: ", text2, "\n")
  cat("Output:", result2, "\n\n")

  # Test 3: List handling
  text3 <- "List contents: 'my_list'"
  result3 <- string_eval(text3, test_env)
  cat("Test 3 - List handling:\n")
  cat("Input: ", text3, "\n")
  cat("Output:", result3, "\n\n")

  # Test 4: Data frame handling
  text4 <- "Data frame info: 'my_df'"
  result4 <- string_eval(text4, test_env)
  cat("Test 4 - Data frame handling:\n")
  cat("Input: ", text4, "\n")
  cat("Output:", result4, "\n\n")

  # Test 5: Escaped quotes
  text5 <- "Result: 'paste(\"Hello\", \"World\", sep=\" \")'"
  result5 <- string_eval(text5, test_env)
  cat("Test 5 - Escaped quotes:\n")
  cat("Input: ", text5, "\n")
  cat("Output:", result5, "\n\n")

  # Test 6: Error handling
  text6 <- "This will error: 'nonexistent_var + 1'"
  result6 <- string_eval(text6, test_env)
  cat("Test 6 - Error handling:\n")
  cat("Input: ", text6, "\n")
  cat("Output:", result6, "\n\n")

  # Test 7: Safe mode (blocks dangerous operations)
  text7 <- "This is blocked: 'system(\"echo hello\")'"
  result7 <- string_eval_safe(text7, test_env)
  cat("Test 7 - Safe mode:\n")
  cat("Input: ", text7, "\n")
  cat("Output:", result7, "\n\n")

  # Test 8: Mixed quotes
  text8 <- 'Single quotes: \'x\' and double quotes: "name"'
  result8 <- string_eval(text8, test_env)
  cat("Test 8 - Mixed quotes:\n")
  cat("Input: ", text8, "\n")
  cat("Output:", result8, "\n\n")

  cat("=== All tests completed ===\n")
}

#' Simple test function for debugging fullscreen
aceEditor_test_fullscreen <- function(inputId, value = "# Test editor\n# Click the fullscreen button to test", height = "300px") {

  # Create a simple fullscreen button
  fullscreen_button <- actionButton(
    paste0(inputId, "_test_fullscreen_btn"),
    icon = icon("expand"),
    label = "Test Fullscreen",
    class = "btn-sm btn-primary mb-2"
  )

  # Create the editor
  editor <- aceEditor(
    inputId,
    value = value,
    mode = "r",
    theme = "gruvbox",
    height = height,
    fontSize = 13
  )

  # Simple JavaScript for testing
  test_js <- tags$script(HTML(sprintf("
    $(document).ready(function() {
      console.log('Test fullscreen setup for:', '%s');

      var testBtn = $('#' + '%s' + '_test_fullscreen_btn');
      var editorContainer = $('#' + '%s').closest('.shiny-ace-container');

      testBtn.on('click', function() {
        console.log('Test button clicked!');
        alert('Button clicked! Editor ID: ' + '%s');

        // Simple test - just change background color
        editorContainer.css('background-color', 'yellow');
        setTimeout(function() {
          editorContainer.css('background-color', '');
        }, 2000);
      });
    });
  ", inputId, inputId, inputId, inputId)))

  # Return the test widget
  tagList(
    test_js,
    div(
      fullscreen_button,
      editor
    )
  )
}