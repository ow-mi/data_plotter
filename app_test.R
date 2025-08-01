# Minimal test app without ggplot2 dependencies
library(shiny)
library(bslib)
library(DT)

# Simple test UI
ui <- page_fluid(
  h1("Data Plotter - Test Version"),
  p("This is a test version without ggplot2 to verify core functionality."),
  br(),
  h3("Core Functionality Test:"),
  fluidRow(
    column(6,
      h4("Data Input"),
      numericInput("n", "Number of rows:", value = 100, min = 1, max = 1000),
      actionButton("generate", "Generate Test Data", class = "btn-primary")
    ),
    column(6,
      h4("Simple Plot (Plotly)"),
      plotly::plotlyOutput("test_plot")
    )
  ),
  br(),
  h4("Data Table"),
  DT::DTOutput("test_table")
)

# Simple test server
server <- function(input, output, session) {
  # Test data generation
  test_data <- reactiveVal(data.frame())
  
  observeEvent(input$generate, {
    n <- input$n
    df <- data.frame(
      x = 1:n,
      y = rnorm(n),
      category = sample(c("A", "B", "C"), n, replace = TRUE)
    )
    test_data(df)
    showNotification("Test data generated successfully!", type = "message")
  })
  
  # Simple plotly output (no ggplot2)
  output$test_plot <- plotly::renderPlotly({
    req(test_data())
    df <- test_data()
    
    plotly::plot_ly(
      data = df,
      x = ~x,
      y = ~y,
      color = ~category,
      type = "scatter",
      mode = "markers"
    ) %>%
    plotly::layout(
      title = "Test Scatter Plot",
      xaxis = list(title = "X Values"),
      yaxis = list(title = "Y Values")
    )
  })
  
  # Data table output
  output$test_table <- DT::renderDT({
    req(test_data())
    DT::datatable(
      test_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # Generate initial data
  observeEvent(session$clientData, {
    n <- 50
    df <- data.frame(
      x = 1:n,
      y = rnorm(n),
      category = sample(c("A", "B", "C"), n, replace = TRUE)
    )
    test_data(df)
  }, once = TRUE)
}

# Create the app
shinyApp(ui = ui, server = server) 