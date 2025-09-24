#' Cheatsheet Helper Page Server
#'
#' @param id Module ID
#' @param ... Additional arguments
#' @import shiny
#' @noRd
srv_cheatsheet <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    # The cheatsheet page is primarily static content with external links
    # Server-side logic can be added here if needed in the future

    # Example: Track which cheatsheets are most frequently accessed
    # This could be used for analytics or user preferences
    observeEvent(input$cheatsheet_nav, {
      # Could log navigation events here for user experience tracking
      # cat("User navigated to:", input$cheatsheet_nav, "\n")
    })

    # Could add bookmarking functionality
    # observeEvent(input$bookmark_cheatsheet, {
    #   showModal(modalDialog(
    #     title = "Bookmark Cheatsheet",
    #     "Bookmarking functionality would go here"
    #   ))
    # })

    # Return reactive values if needed
    return(list(
      # active_tab = reactive(input$cheatsheet_nav)
    ))
  })
}