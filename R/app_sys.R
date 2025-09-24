#' Get the system path to a file in the app directory
#'
#' @param ... character vectors, specifying subdirectory and file(s) within app directory
#' @export
app_sys <- function(...) {
  system.file(..., package = "dataPlotter")
}