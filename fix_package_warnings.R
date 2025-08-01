#!/usr/bin/env Rscript

# Script to fix package warnings for shinylive compatibility
cat("=== Fixing Package Warnings for Shinylive ===\n\n")

# Update packages to match WebAssembly versions
packages_to_update <- c(
  "knitr", "tinytex", "xfun", "openssl", "pillar", 
  "httr2", "ps", "R.oo", "globals", "parallelly"
)

cat("Updating packages to latest versions...\n")
cat("=========================================\n\n")

for(pkg in packages_to_update) {
  tryCatch({
    if(pkg %in% rownames(installed.packages())) {
      cat("Updating", pkg, "...\n")
      install.packages(pkg, dependencies = FALSE, quiet = TRUE)
      cat("✓ Updated", pkg, "\n\n")
    } else {
      cat("Installing", pkg, "...\n")
      install.packages(pkg, quiet = TRUE)
      cat("✓ Installed", pkg, "\n\n")
    }
  }, error = function(e) {
    cat("✗ Failed to update", pkg, ":", e$message, "\n\n")
  })
}

cat("=== Package Updates Completed! ===\n")
cat("Now checking remaining library paths...\n\n")

# Show library paths
all_libs <- .libPaths()
cat("Current library paths:\n")
for (i in seq_along(all_libs)) {
  cat(paste0("  ", i, ": ", all_libs[i], "\n"))
}

cat("\nScript completed! Re-run your shinylive export to check for reduced warnings.\n") 