
files <- list.files()

# Loop through each file
for (file in files) {
  # Check if the file is an R script
  if (grepl("\\.R$", file)) { # Use regex for more robust matching
    # Source the file
    if (file != "app.R") {
      source(file)
    }
  }
}

library("shiny")
library("shinyTime")
library("bslib")
library("shinyAce")
library("stringr")
library("data.table")
library("DT")
library("munsell")
library("ggplot2")
library("plotly")
library("lubridate")
library("jsonlite")
library("fasttime")
library("readxl")
library("fst")
library("nanoparquet")
library("tools")
library("scattermore")
library("purrr")
library("future")
library("skimr")
library("htmlwidgets")
library("spsComps")
library("base64enc")

app <- shiny::shinyApp(
  ui = ui_global(),
  server = server_global
)
app