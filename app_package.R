#!/usr/bin/env Rscript
# Package-based app.R for deployment
# Load required libraries
library(shiny)
library(devtools)
# options(shiny.error = browser())
devtools::load_all()

# source("code_template/load_templates.R")
# Load templates using the package function (in correct order)
# dataPlotter::load_templates_ordered()

# Now, run your app using the UI and Server objects
# Make sure they are exported from your package's NAMESPACE
# or call them with dataPlotter:::ui_global and dataPlotter:::server_global if not exported.
app <- shiny::shinyApp(
  ui = dataPlotter::ui_global(),
  server = dataPlotter::server_global
)
app