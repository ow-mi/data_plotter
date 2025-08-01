# Install specific stable version of shinylive if not already installed
if (!requireNamespace("shinylive", quietly = TRUE) || packageVersion("shinylive") < "0.1.0") {
  # Install from CRAN instead of development version
  install.packages("shinylive", repos = "https://cran.r-project.org")
}

# Try to downgrade future package to match WebAssembly version
if (requireNamespace("future", quietly = TRUE) && packageVersion("future") != "1.40.0") {
  # Try to install the compatible version
  tryCatch({
    install.packages("https://cran.r-project.org/src/contrib/Archive/future/future_1.40.0.tar.gz", 
                     repos = NULL, type = "source")
  }, error = function(e) {
    warning("Could not install future 1.40.0, proceeding with current version")
  })
}

export_name <- "shinylive_export"

tryCatch(
  {
    shinylive::export("./", export_name)
  },
  error = function(e) {
    stop("Error in shinylive::export: ", e$message)
  }
)