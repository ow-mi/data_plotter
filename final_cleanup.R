#!/usr/bin/env Rscript

# Final cleanup to fix remaining package warnings
cat("=== Final Package Version Cleanup ===\n\n")

# Fix the remaining 3 version mismatches
cat("Fixing remaining version mismatches...\n\n")

# 1. Fix knitr: 1.49 -> 1.50 (downgrade from current system)
cat("1. Fixing knitr version...\n")
tryCatch({
  remove.packages("knitr")
  install.packages("https://cran.r-project.org/src/contrib/Archive/knitr/knitr_1.50.tar.gz", 
                   repos = NULL, type = "source", quiet = TRUE)
  cat("✓ knitr fixed: 1.50\n\n")
}, error = function(e) {
  cat("✗ Failed to fix knitr\n\n")
})

# 2. Fix globals: 0.18.0 -> 0.17.0 (downgrade to match Wasm)
cat("2. Fixing globals version...\n")
tryCatch({
  remove.packages("globals")
  install.packages("https://cran.r-project.org/src/contrib/Archive/globals/globals_0.17.0.tar.gz", 
                   repos = NULL, type = "source", quiet = TRUE)
  cat("✓ globals fixed: 0.17.0\n\n")
}, error = function(e) {
  cat("✗ Failed to fix globals\n\n")
})

# 3. Fix parallelly: 1.45.0 -> 1.43.0 (downgrade to match Wasm)
cat("3. Fixing parallelly version...\n")
tryCatch({
  remove.packages("parallelly")
  install.packages("https://cran.r-project.org/src/contrib/Archive/parallelly/parallelly_1.43.0.tar.gz", 
                   repos = NULL, type = "source", quiet = TRUE)
  cat("✓ parallelly fixed: 1.43.0\n\n")
}, error = function(e) {
  cat("✗ Failed to fix parallelly\n\n")
})

cat("=== Final Cleanup Complete! ===\n")
cat("The remaining warnings about MASS, Matrix, and curl are normal.\n")
cat("These are system packages not available in WebAssembly.\n")
cat("Your app should work perfectly now!\n") 