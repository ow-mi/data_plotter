#' Cheatsheets UI Module
#'
#' @import shiny
#' @import bslib
#' @importFrom shiny icon div h3 h4 h5 p a br tags HTML actionButton
#' @noRd
ui_cheatsheets <- function(id) {
  ns <- NS(id)

  div(class = "container-fluid",
    div(class = "row",

      # Main content area
      div(class = "col-12",

        # Header
        div(class = "text-center mb-5",
          h2("📚 Help & Cheatsheets", class = "mb-3"),
          p(class = "text-muted",
            "Quick reference guides for the main R libraries used in Data Plotter")
        ),

        # Quick Access Buttons
        div(class = "row mb-4",
          div(class = "col-md-3 mb-3",
            actionButton(ns("open_ggplot"), "ggplot2 Cheatsheet",
                        class = "btn btn-outline-primary w-100 h-100",
                        icon = icon("chart-line"),
                        onclick = "window.open('https://raw.githubusercontent.com/rstudio/cheatsheets/main/data-visualization.pdf', '_blank')")
          ),
          div(class = "col-md-3 mb-3",
            actionButton(ns("open_plotly"), "Plotly Cheatsheet",
                        class = "btn btn-outline-info w-100 h-100",
                        icon = icon("chart-area"),
                        onclick = "window.open('https://images.plot.ly/plotly-documentation/images/r_cheat_sheet.pdf', '_blank')")
          ),
          div(class = "col-md-3 mb-3",
            actionButton(ns("open_shiny"), "Shiny Cheatsheet",
                        class = "btn btn-outline-success w-100 h-100",
                        icon = icon("code"),
                        onclick = "window.open('https://shiny.rstudio.com/images/shiny-cheatsheet.pdf', '_blank')")
          ),
          div(class = "col-md-3 mb-3",
            actionButton(ns("open_dplyr"), "dplyr Cheatsheet",
                        class = "btn btn-outline-warning w-100 h-100",
                        icon = icon("filter"),
                        onclick = "window.open('https://raw.githubusercontent.com/rstudio/cheatsheets/main/data-transformation.pdf', '_blank')")
          )
        ),

        # Individual Library Sections
        div(class = "row",
          div(class = "col-lg-6 mb-4",

            # ggplot2 Section
            div(class = "card h-100",
              div(class = "card-header bg-primary text-white",
                h4(icon("chart-line"), " ggplot2", class = "mb-0")
              ),
              div(class = "card-body",
                h5("Data Visualization"),
                p(class = "text-muted small", "Create static plots and charts"),

                # Key Functions
                div(class = "mb-3",
                  h6("Core Functions"),
                  tags$ul(class = "list-unstyled",
                    tags$li(tags$code("ggplot()"), " - Initialize plot"),
                    tags$li(tags$code("aes()"), " - Map variables to aesthetics"),
                    tags$li(tags$code("geom_point()"), " - Scatter plots"),
                    tags$li(tags$code("geom_line()"), " - Line plots"),
                    tags$li(tags$code("geom_bar()"), " - Bar plots"),
                    tags$li(tags$code("facet_wrap()"), " - Multi-panel plots")
                  )
                ),

                # Quick Examples
                div(class = "mb-3",
                  h6("Quick Examples"),
                  tags$pre(class = "bg-light p-2 small",
                    "# Scatter plot
ggplot(data, aes(x = var1, y = var2)) +
  geom_point()

# Line plot with color
ggplot(data, aes(x = time, y = value, color = category)) +
  geom_line()

# Bar plot with facets
ggplot(data, aes(x = category, y = count)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~group)"
                  )
                )
              )
            )
          ),

          div(class = "col-lg-6 mb-4",

            # Plotly Section
            div(class = "card h-100",
              div(class = "card-header bg-info text-white",
                h4(icon("chart-area"), " plotly", class = "mb-0")
              ),
              div(class = "card-body",
                h5("Interactive Visualizations"),
                p(class = "text-muted small", "Create interactive plots and charts"),

                # Key Functions
                div(class = "mb-3",
                  h6("Core Functions"),
                  tags$ul(class = "list-unstyled",
                    tags$li(tags$code("plot_ly()"), " - Initialize interactive plot"),
                    tags$li(tags$code("add_trace()"), " - Add data traces"),
                    tags$li(tags$code("layout()"), " - Configure plot layout"),
                    tags$li(tags$code("ggplotly()"), " - Convert ggplot to plotly"),
                    tags$li(tags$code("subplot()"), " - Multiple plots")
                  )
                ),

                # Quick Examples
                div(class = "mb-3",
                  h6("Quick Examples"),
                  tags$pre(class = "bg-light p-2 small",
                    "# Interactive scatter plot
plot_ly(data, x = ~var1, y = ~var2, type = 'scatter', mode = 'markers')

# Interactive bar chart
plot_ly(data, x = ~category, y = ~value, type = 'bar')

# Convert ggplot to interactive
ggplot(data, aes(x = time, y = value)) +
  geom_line() -> p
ggplotly(p)"
                  )
                )
              )
            )
          )
        ),

        div(class = "row",
          div(class = "col-lg-6 mb-4",

            # Shiny Section
            div(class = "card h-100",
              div(class = "card-header bg-success text-white",
                h4(icon("code"), " shiny", class = "mb-0")
              ),
              div(class = "card-body",
                h5("Web Applications"),
                p(class = "text-muted small", "Build interactive web applications"),

                # Key Functions
                div(class = "mb-3",
                  h6("Core Functions"),
                  tags$ul(class = "list-unstyled",
                    tags$li(tags$code("shinyApp()"), " - Create application"),
                    tags$li(tags$code("fluidPage()"), " - Create UI layout"),
                    tags$li(tags$code("sidebarLayout()"), " - Sidebar layout"),
                    tags$li(tags$code("reactive()"), " - Create reactive expressions"),
                    tags$li(tags$code("observeEvent()"), " - Respond to events")
                  )
                ),

                # Quick Examples
                div(class = "mb-3",
                  h6("Quick Examples"),
                  tags$pre(class = "bg-light p-2 small",
                    "# Simple reactive expression
output$plot <- renderPlot({
  ggplot(data(), aes(x = x, y = y)) + geom_point()
})

# Event observer
observeEvent(input$update, {
  # Code to run when button is clicked
})

# Reactive values
values <- reactiveValues(counter = 0)
observeEvent(input$increment, {
  values$counter <- values$counter + 1
})"
                  )
                )
              )
            )
          ),

          div(class = "col-lg-6 mb-4",

            # dplyr & stringr Section
            div(class = "card h-100",
              div(class = "card-header bg-warning",
                h4(icon("filter"), " dplyr + stringr", class = "mb-0")
              ),
              div(class = "card-body",
                h5("Data Manipulation & String Processing"),
                p(class = "text-muted small", "Transform and manipulate data"),

                # Key Functions
                div(class = "row",
                  div(class = "col-6",
                    h6("dplyr Functions"),
                    tags$ul(class = "list-unstyled small",
                      tags$li(tags$code("filter()"), " - Filter rows"),
                      tags$li(tags$code("select()"), " - Select columns"),
                      tags$li(tags$code("mutate()"), " - Add/modify columns"),
                      tags$li(tags$code("group_by()"), " - Group data"),
                      tags$li(tags$code("summarise()"), " - Summarize data")
                    )
                  ),
                  div(class = "col-6",
                    h6("stringr Functions"),
                    tags$ul(class = "list-unstyled small",
                      tags$li(tags$code("str_detect()"), " - Detect patterns"),
                      tags$li(tags$code("str_replace()"), " - Replace text"),
                      tags$li(tags$code("str_split()"), " - Split strings"),
                      tags$li(tags$code("str_trim()"), " - Trim whitespace"),
                      tags$li(tags$code("str_extract()"), " - Extract matches")
                    )
                  )
                ),

                # Quick Examples
                div(class = "mb-3",
                  h6("Quick Examples"),
                  tags$pre(class = "bg-light p-2 small",
                    "# Filter and mutate
data %>%
  filter(category == 'A') %>%
  mutate(double_value = value * 2) %>%
  select(name, double_value)

# String operations
text <- 'Hello, World!'
str_detect(text, 'World')  # TRUE
str_replace(text, 'World', 'Universe')  # 'Hello, Universe!'
str_split(text, ', ')  # List with 'Hello' and 'World!'"
                  )
                )
              )
            )
          )
        ),

        # Additional Resources Section
        div(class = "row mt-4",
          div(class = "col-12",
            div(class = "card",
              div(class = "card-header bg-secondary text-white",
                h4(icon("external-link-alt"), " Additional Resources", class = "mb-0")
              ),
              div(class = "card-body",
                div(class = "row",
                  div(class = "col-md-4 mb-3",
                    h5("Official Documentation"),
                    tags$ul(class = "list-unstyled",
                      tags$li(a(href = "https://ggplot2.tidyverse.org/", target = "_blank",
                               icon("external-link-alt"), " ggplot2 Docs")),
                      tags$li(a(href = "https://plotly.com/r/", target = "_blank",
                               icon("external-link-alt"), " Plotly R Docs")),
                      tags$li(a(href = "https://shiny.rstudio.com/", target = "_blank",
                               icon("external-link-alt"), " Shiny Docs"))
                    )
                  ),
                  div(class = "col-md-4 mb-3",
                    h5("Community Resources"),
                    tags$ul(class = "list-unstyled",
                      tags$li(a(href = "https://www.rstudio.com/resources/cheatsheets/", target = "_blank",
                               icon("external-link-alt"), " All RStudio Cheatsheets")),
                      tags$li(a(href = "https://stackoverflow.com/questions/tagged/r", target = "_blank",
                               icon("external-link-alt"), " Stack Overflow R")),
                      tags$li(a(href = "https://community.rstudio.com/", target = "_blank",
                               icon("external-link-alt"), " RStudio Community"))
                    )
                  ),
                  div(class = "col-md-4 mb-3",
                    h5("Learning Resources"),
                    tags$ul(class = "list-unstyled",
                      tags$li(a(href = "https://r4ds.had.co.nz/", target = "_blank",
                               icon("external-link-alt"), " R for Data Science")),
                      tags$li(a(href = "https://mastering-shiny.org/", target = "_blank",
                               icon("external-link-alt"), " Mastering Shiny")),
                      tags$li(a(href = "https://ggplot2-book.org/", target = "_blank",
                               icon("external-link-alt"), " ggplot2 Book"))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}