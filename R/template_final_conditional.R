# Final Conditional Template
# This code runs after all the above templates and selects the appropriate output

ggplot_final_template <- "# Final conditional code to select output type
# This code runs after all the above templates and selects the appropriate output
# Available: df (processed data), input (UI inputs), incProgress()

if (is.null(df) || nrow(df) == 0) {
  # No data case
  renderUI({
    div(class = 'text-center p-4',
      h4('No Data Available', class = 'text-muted'),
      p('Please process data in the Processing Data tab first.')
    )
  })
} else if (!is.null(input$plot_type)) {
  # Select output based on plot type
  if (input$plot_type == 'text') {
    # Execute text template code and render as print
    if (exists('incProgress')) incProgress(0.1, detail = 'Generating text output...')
    text_output <- eval(parse(text = text_code))
    renderPrint({ text_output })
  } else if (input$plot_type == 'static') {
    # Execute static template code and render as plot
    if (exists('incProgress')) incProgress(0.1, detail = 'Creating static plot...')
    static_plot <- eval(parse(text = static_code))
    renderPlot({ static_plot })
  } else if (input$plot_type == 'interactive') {
    # Execute interactive template code and return plotly object directly
    if (exists('incProgress')) incProgress(0.1, detail = 'Building interactive plot...')
    interactive_plot <- eval(parse(text = interactive_code))
    renderUI({ interactive_plot })
  } else if (input$plot_type == 'table') {
    # Execute table template code and render as DT
    if (exists('incProgress')) incProgress(0.1, detail = 'Creating data table...')
    table_output <- eval(parse(text = table_code))
    table_output # Return DT object directly
  } else {
    # Fallback
    renderUI({
      div(class = 'text-center p-4',
        h4('Unknown Plot Type', class = 'text-muted'),
        p('Please select a valid plot type: Text, Static, Interactive, or Table.')
      )
    })
  }
} else {
  # No plot type selected
  renderUI({
    div(class = 'text-center p-4',
      h4('Select Plot Type', class = 'text-muted'),
      p('Choose Text, Static, Interactive, or Table from the plot controls.')
    )
  })
}
"