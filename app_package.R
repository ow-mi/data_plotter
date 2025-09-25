#!/usr/bin/env Rscript
# Package-based app.R for deployment using golem structure
library(dataPlotter)

# Load templates using the package function (in correct order)
dataPlotter::load_templates_ordered()

# Run the application using the golem run_app function
dataPlotter::run_app()