# Simple export script without complex version management
export_name <- "shinylive_export"

# Remove existing export directory if it exists
if (dir.exists(export_name)) {
  unlink(export_name, recursive = TRUE)
}

# Try to load shinylive
if (!requireNamespace("shinylive", quietly = TRUE)) {
  cat("Installing shinylive package...\n")
  install.packages("shinylive", repos = "https://cran.r-project.org")
}

library(shinylive)

# Try export with error handling
tryCatch(
  {
    cat("Exporting Shiny app to", export_name, "...\n")
    shinylive::export("./", export_name)
    cat("Export completed successfully!\n")
  },
  error = function(e) {
    cat("Export failed with error:", e$message, "\n")
    
    # Try alternative approach: create basic export manually
    cat("Attempting manual export...\n")
    dir.create(export_name, showWarnings = FALSE, recursive = TRUE)
    
    # Copy app files
    file.copy("app.R", file.path(export_name, "app.R"))
    file.copy("functions.R", file.path(export_name, "functions.R"))
    file.copy("r_code_.R", file.path(export_name, "r_code_.R"))
    
    # Copy modules directory
    if (dir.exists("modules")) {
      file.copy("modules", export_name, recursive = TRUE)
    }
    
    # Copy www directory
    if (dir.exists("www")) {
      file.copy("www", export_name, recursive = TRUE)
    }
    
    # Create basic index.html
    cat('<!DOCTYPE html>
<html>
<head>
    <title>Data Plotter</title>
    <script src="https://cdn.jsdelivr.net/npm/@posit-dev/shinylive@0.2.3/dist/shinylive.js"></script>
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/@posit-dev/shinylive@0.2.3/dist/shinylive.css">
</head>
<body>
    <div id="app"></div>
    <script>
        shinylive.runApp({
            appRoot: "./",
            startFiles: ["app.R"]
        });
    </script>
</body>
</html>', file = file.path(export_name, "index.html"))
    
    cat("Manual export completed!\n")
  }
) 