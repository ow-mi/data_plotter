server_data_table_display <- function(id, input_data_reactive) {
  moduleServer(id, function(input, output, session) {
    processed_data <- reactiveVal()

    # Debounce the R code input to avoid rapid re-evaluations
    debounced_code_input <- debounce(reactive(input$code_input), 1000)

    observe({ # Unified observer for code changes or data changes
      current_code <- debounced_code_input()
      current_data <- input_data_reactive()
      
      # Don't req(current_data) here - our code templates handle NULL data gracefully
      
      # Only proceed if code is non-empty, otherwise might show raw data
      # Or, if code is empty, could default to showing current_data directly as DT
      if (is.null(current_code) || current_code == "") {
          # Default behavior if no code: render data as a basic DT table
          if (!is.null(current_data) && (is.data.frame(current_data) || is.data.table(current_data))) {
            processed_data(datatable(current_data, options = list(scrollX = TRUE, pageLength = 5, lengthMenu = c(5, 10, 25, 50)), rownames = FALSE, filter = 'top', class = 'compact stripe hover'))
          } else {
            # Handle NULL data case even when no custom code
            processed_data(data.frame(Message = "No data available") |> datatable(options = list(dom = 't', ordering = FALSE), rownames = FALSE, class = 'compact'))
          }
          return()
      }

      spsComps::shinyCatch({
        # Ensure 'df' is available in the eval environment
        env <- new.env(parent = .GlobalEnv) # Safer environment
        env$df <- current_data
        # Make essential functions available for data table display code
        env$datatable <- DT::datatable
        env$renderPrint <- shiny::renderPrint
        env$skim <- skimr::skim  # Use fully qualified name
        env$setDT <- data.table::setDT
        env$data.frame <- data.frame
        env$is.data.table <- data.table::is.data.table
        env$is.data.frame <- is.data.frame
        env$is.null <- is.null
        env$nrow <- nrow
        env$ncol <- ncol
        env$names <- names
        env$paste <- paste
        env$c <- c
        env$library <- library  # Allow library calls in eval code
        env$require <- require   # Allow require calls in eval code
        
        evaluated_output <- eval(parse(text = current_code), envir = env)
        processed_data(evaluated_output)
      })
    }) |> bindEvent(list(debounced_code_input(), input_data_reactive(), input$apply_code), ignoreNULL = FALSE, ignoreInit = FALSE)


    output$data_table <- renderUI({
      req(processed_data()) # Ensure processed_data is available
      tagList(processed_data()) # Wrap in tagList in case it's multiple UI elements
    })
  })
}