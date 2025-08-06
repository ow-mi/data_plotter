# Final Conditional Template
# This code runs after all the above templates and selects the appropriate output

ggplot_final_template <- "# Final conditional code to select output type
# This code runs after all the above templates and selects the appropriate output
# Available: df (processed data), input (UI inputs)

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
    text_output <- eval(parse(text = text_code))
    renderPrint({ text_output })
  } else if (input$plot_type == 'static') {
    # Execute static template code and render as plot
    static_plot <- eval(parse(text = static_code))
    renderPlot({ static_plot }, height = 600)
  } else if (input$plot_type == 'interactive') {
    # Execute interactive template code and return plotly object directly
    interactive_plot <- eval(parse(text = interactive_code))
    renderUI({ interactive_plot })
  } else if (input$plot_type == 'table') {
    # Execute table template code and render as DT
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