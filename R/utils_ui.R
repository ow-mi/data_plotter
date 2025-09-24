#' UI Utility Functions
#'
#' @import shiny
#' @import bslib
#' @noRd

#' Concatenate all arguments with paste0
p <- function(...) {
  paste0(...)
}

#' Text input with tooltip
text_input_tip <- function(inputId, label, value = "", tip = "Tooltip text", placeholder = "") {
  shiny::textInput(inputId, label, value, placeholder = placeholder) |>
    bslib::tooltip(tip, placement = "right")
}

#' Numeric input with tooltip
numeric_input_tip <- function(inputId, label, value, min = 0, max = 1, step = 0.1, tip = "Tooltip text") {
  shiny::numericInput(inputId, label, value = value, min = min, max = max, step = step) |>
    bslib::tooltip(tip, placement = "right")
}

#' Action button with tooltip
action_input_tip <- function(inputId, label, tip = "Tooltip text", ...) {
  shiny::actionButton(inputId, label, ...) |>
    bslib::tooltip(tip, placement = "right")
}

#' Create eval text input
create_eval_text_input <- function(inputId, label, value = "", tip = "Use single or double quotes around R code to evaluate it", ...) {
  text_input_tip(
    inputId = inputId,
    label = label,
    value = value,
    tip = tip,
    ...
  )
}

#' Custom downloadButton to prevent default browser download behavior
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}