# Helper Code Template
# For R Code Helper Tab

helper_code_template <- "# R Code Helper
# Type R code here to execute it.
# Example: List installed packages
# installed.packages()[, 'Package'] |> head(20) |> paste(collapse = '\\n') |> cat()

# Example: Get help on a function
# help('lm') # This will open help in RStudio, not directly in Shiny UI

# Example: Show vignette (if HTML can be rendered)
# vignette_path <- system.file('doc', 'grid.html', package = 'grid')
# if (file.exists(vignette_path)) {
#   htmltools::HTML(readLines(vignette_path))
# } else {
#   'Vignette not found.'
# }

# Example: Basic calculation
# data.frame(x = 1:5, y = (1:5)^2) |> datatable()

# Available in this environment:
# - 'input' (main Shiny app's input)
# - 'output' (main Shiny app's output - use with caution)

# Get current working directory
getwd()
"