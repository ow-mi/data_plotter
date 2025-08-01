# Simple local server for testing the app without shinylive

cat("Starting local Shiny app server...\n")
cat("This will run the app directly without shinylive export.\n")
cat("Press Ctrl+C to stop the server.\n\n")

# Load required packages
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

library(shiny)

# Set port
port <- 8082

# Check if port is available
tryCatch({
  # Try to run the app
  shiny::runApp(".", port = port, host = "0.0.0.0", launch.browser = TRUE)
}, error = function(e) {
  cat("Failed to start on port", port, ":", e$message, "\n")
  cat("Trying port 8083...\n")
  shiny::runApp(".", port = 8083, host = "0.0.0.0", launch.browser = TRUE)
}) 