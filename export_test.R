# Simple export for test app
export_name <- "test_export"

# Remove existing directory
if (dir.exists(export_name)) {
  unlink(export_name, recursive = TRUE)
}

# Load shinylive
library(shinylive)

# Create temporary directory with the test app
test_dir <- "temp_test_app"
if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
dir.create(test_dir)

# Copy test app to app.R in temp directory
file.copy("app_test.R", file.path(test_dir, "app.R"))

# Export test app
shinylive::export(test_dir, export_name)

# Cleanup
unlink(test_dir, recursive = TRUE)

cat("Test app export completed to:", export_name, "\n") 