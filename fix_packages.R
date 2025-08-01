# Script to fix package version conflicts for shinylive export

cat("Checking and fixing package versions...\n")

# Check if we're in R 4.4 environment
r_version <- R.Version()
cat("R version:", r_version$version.string, "\n")

# Install/update shinylive to latest stable version
cat("Installing/updating shinylive...\n")
tryCatch({
  # Try to install from CRAN first
  install.packages("shinylive", repos = "https://cran.r-project.org")
  cat("shinylive installed from CRAN\n")
}, error = function(e) {
  # If CRAN fails, try remotes
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("posit-dev/r-shinylive@v0.2.1")
  cat("shinylive installed from GitHub\n")
})

# Check current future version
if (requireNamespace("future", quietly = TRUE)) {
  current_future <- packageVersion("future")
  cat("Current future version:", as.character(current_future), "\n")
  
  # If version is too new, try to install compatible version
  if (current_future > "1.40.0") {
    cat("Attempting to install compatible future version (1.40.0)...\n")
    tryCatch({
      # Try to install specific version
      remotes::install_version("future", version = "1.40.0", repos = "https://cran.r-project.org")
      cat("future 1.40.0 installed successfully\n")
    }, error = function(e) {
      cat("Could not install future 1.40.0, keeping current version\n")
      cat("Warning: Version mismatch may cause issues\n")
    })
  }
}

# Check other key packages that might have conflicts
packages_to_check <- c("promises", "ggplot2", "plotly", "data.table", "DT")
for (pkg in packages_to_check) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(pkg, "version:", as.character(packageVersion(pkg)), "\n")
  }
}

cat("Package version check complete!\n") 