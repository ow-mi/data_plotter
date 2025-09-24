#' Cheatsheet Helper Page UI
#'
#' @import shiny
#' @import bslib
#' @importFrom shiny icon h2 h3 p a div br tags HTML
#' @noRd
ui_cheatsheet <- function() {
  navset_card_pill(
    id = "cheatsheet_nav",
    title = "R Library Cheatsheets",
    nav_panel(
      "Quick Access",
      icon = icon("rocket"),
      layout_column_wrap(
        width = 1/3,
        heights_equal = "row",
        # Shiny
        card(
          card_header(
            div(
              class = "d-flex align-items-center",
              icon("desktop", class = "me-2"),
              "Shiny"
            )
          ),
          card_body(
            p("Web application framework for R"),
            div(
              class = "d-grid gap-2",
              a(
                href = "https://shiny.rstudio.com/articles/cheatsheet.html",
                target = "_blank",
                class = "btn btn-outline-primary",
                icon("external-link-alt", class = "me-1"),
                "Official Shiny Cheatsheet"
              ),
              a(
                href = "https://rstudio.github.io/shiny/reference/",
                target = "_blank",
                class = "btn btn-outline-info",
                icon("book", class = "me-1"),
                "Shiny Reference Guide"
              )
            )
          )
        ),

        # ggplot2
        card(
          card_header(
            div(
              class = "d-flex align-items-center",
              icon("chart-bar", class = "me-2"),
              "ggplot2"
            )
          ),
          card_body(
            p("Data visualization package"),
            div(
              class = "d-grid gap-2",
              a(
                href = "https://rstudio.github.io/cheatsheets/html/data-visualization.html",
                target = "_blank",
                class = "btn btn-outline-success",
                icon("external-link-alt", class = "me-1"),
                "Official ggplot2 Cheatsheet"
              ),
              a(
                href = "https://ggplot2.tidyverse.org/reference/",
                target = "_blank",
                class = "btn btn-outline-info",
                icon("book", class = "me-1"),
                "ggplot2 Reference"
              )
            )
          )
        ),

        # plotly
        card(
          card_header(
            div(
              class = "d-flex align-items-center",
              icon("chart-line", class = "me-2"),
              "plotly"
            )
          ),
          card_body(
            p("Interactive web graphics"),
            div(
              class = "d-grid gap-2",
              a(
                href = "https://plotly.com/r/",
                target = "_blank",
                class = "btn btn-outline-warning",
                icon("external-link-alt", class = "me-1"),
                "plotly for R"
              ),
              a(
                href = "https://plotly-r.com/",
                target = "_blank",
                class = "btn btn-outline-info",
                icon("book", class = "me-1"),
                "plotly R Book"
              )
            )
          )
        ),

        # dplyr
        card(
          card_header(
            div(
              class = "d-flex align-items-center",
              icon("filter", class = "me-2"),
              "dplyr"
            )
          ),
          card_body(
            p("Data manipulation grammar"),
            div(
              class = "d-grid gap-2",
              a(
                href = "https://rstudio.github.io/cheatsheets/html/data-transformation.html",
                target = "_blank",
                class = "btn btn-outline-danger",
                icon("external-link-alt", class = "me-1"),
                "Official dplyr Cheatsheet"
              ),
              a(
                href = "https://dplyr.tidyverse.org/reference/",
                target = "_blank",
                class = "btn btn-outline-info",
                icon("book", class = "me-1"),
                "dplyr Reference"
              )
            )
          )
        ),

        # stringr
        card(
          card_header(
            div(
              class = "d-flex align-items-center",
              icon("font", class = "me-2"),
              "stringr"
            )
          ),
          card_body(
            p("String manipulation tools"),
            div(
              class = "d-grid gap-2",
              a(
                href = "https://rstudio.github.io/cheatsheets/html/strings.html",
                target = "_blank",
                class = "btn btn-outline-primary",
                icon("external-link-alt", class = "me-1"),
                "Official stringr Cheatsheet"
              ),
              a(
                href = "https://stringr.tidyverse.org/reference/",
                target = "_blank",
                class = "btn btn-outline-info",
                icon("book", class = "me-1"),
                "stringr Reference"
              )
            )
          )
        ),

        # data.table
        card(
          card_header(
            div(
              class = "d-flex align-items-center",
              icon("table", class = "me-2"),
              "data.table"
            )
          ),
          card_body(
            p("Fast data manipulation"),
            div(
              class = "d-grid gap-2",
              a(
                href = "https://rdatatable.gitlab.io/data.table/reference/",
                target = "_blank",
                class = "btn btn-outline-secondary",
                icon("external-link-alt", class = "me-1"),
                "data.table Reference"
              ),
              a(
                href = "https://github.com/Rdatatable/data.table/wiki",
                target = "_blank",
                class = "btn btn-outline-info",
                icon("book", class = "me-1"),
                "data.table Wiki"
              )
            )
          )
        )
      )
    ),

    nav_panel(
      "All Cheatsheets",
      icon = icon("book-open"),
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        card(
          card_header("Base R Cheatsheets"),
          card_body(
            div(
              class = "d-grid gap-2",
              a(
                href = "https://rstudio.github.io/cheatsheets/html/base-r.html",
                target = "_blank",
                class = "btn btn-outline-primary",
                icon("external-link-alt", class = "me-1"),
                "Base R"
              ),
              a(
                href = "https://rstudio.github.io/cheatsheets/html/factors.html",
                target = "_blank",
                class = "btn btn-outline-secondary",
                icon("external-link-alt", class = "me-1"),
                "Factors"
              ),
              a(
                href = "https://rstudio.github.io/cheatsheets/html/dates.html",
                target = "_blank",
                class = "btn btn-outline-info",
                icon("external-link-alt", class = "me-1"),
                "Dates & Times"
              )
            )
          )
        ),

        card(
          card_header("Data Import/Export"),
          card_body(
            div(
              class = "d-grid gap-2",
              a(
                href = "https://rstudio.github.io/cheatsheets/html/data-import.html",
                target = "_blank",
                class = "btn btn-outline-success",
                icon("external-link-alt", class = "me-1"),
                "Data Import"
              ),
              a(
                href = "https://readxl.tidyverse.org/",
                target = "_blank",
                class = "btn btn-outline-warning",
                icon("external-link-alt", class = "me-1"),
                "readxl (Excel)"
              ),
              a(
                href = "https://fstpackage.github.io/fst/",
                target = "_blank",
                class = "btn btn-outline-danger",
                icon("external-link-alt", class = "me-1"),
                "fst (Fast Storage)"
              )
            )
          )
        ),

        card(
          card_header("tidyverse"),
          card_body(
            div(
              class = "d-grid gap-2",
              a(
                href = "https://rstudio.github.io/cheatsheets/html/tidyverse.html",
                target = "_blank",
                class = "btn btn-outline-primary",
                icon("external-link-alt", class = "me-1"),
                "tidyverse Overview"
              ),
              a(
                href = "https://rstudio.github.io/cheatsheets/html/purrr.html",
                target = "_blank",
                class = "btn btn-outline-secondary",
                icon("external-link-alt", class = "me-1"),
                "purrr (Functional)"
              ),
              a(
                href = "https://rstudio.github.io/cheatsheets/html/tidyr.html",
                target = "_blank",
                class = "btn btn-outline-info",
                icon("external-link-alt", class = "me-1"),
                "tidyr (Reshape)"
              )
            )
          )
        ),

        card(
          card_header("Statistics & Modeling"),
          card_body(
            div(
              class = "d-grid gap-2",
              a(
                href = "https://rstudio.github.io/cheatsheets/html/modelr.html",
                target = "_blank",
                class = "btn btn-outline-warning",
                icon("external-link-alt", class = "me-1"),
                "modelr"
              ),
              a(
                href = "https://rstudio.github.io/cheatsheets/html/broom.html",
                target = "_blank",
                class = "btn btn-outline-danger",
                icon("external-link-alt", class = "me-1"),
                "broom (Models)"
              ),
              a(
                href = "https://rstudio.github.io/cheatsheets/html/correlation.html",
                target = "_blank",
                class = "btn btn-outline-success",
                icon("external-link-alt", class = "me-1"),
                "Correlation"
              )
            )
          )
        )
      )
    ),

    nav_panel(
      "Custom References",
      icon = icon("cog"),
      card(
        card_header("Data Plotter Tool"),
        card_body(
          h5("Built-in Function Reference"),
          p("This tool uses many of the libraries above. Here are some quick tips:"),
          tags$ul(
            tags$li(
              strong("String Evaluation: "),
              "Use single or double quotes around R code to evaluate it dynamically. Example: ",
              code("'paste(\"Hello\", \"World\")'")
            ),
            tags$li(
              strong("Data Filtering: "),
              "Use patterns like ", code("column:value1,value2 | column2:pattern*"), " to filter data"
            ),
            tags$li(
              strong("Plot Templates: "),
              "The tool loads code templates from ", code("inst/app/code_template/"), " directory"
            )
          ),
          h5("Keyboard Shortcuts"),
          tags$ul(
            tags$li(code("Ctrl+Enter"), " - Run code in ACE editor"),
            tags$li(code("Ctrl+Shift+F"), " - Toggle fullscreen editor"),
            tags$li(code("Ctrl+Z"), " - Undo in editor"),
            tags$li(code("Ctrl+Y"), " - Redo in editor")
          )
        )
      ),

      card(
        card_header("RStudio IDE Tips"),
        card_body(
          a(
            href = "https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts",
            target = "_blank",
            class = "btn btn-outline-primary mb-3",
            icon("external-link-alt", class = "me-1"),
            "RStudio Keyboard Shortcuts"
          ),
          br(),
          p("Some useful RStudio shortcuts:"),
          tags$ul(
            tags$li(code("Ctrl+Shift+N"), " - New R Script"),
            tags$li(code("Ctrl+1"), " - Move to Source Pane"),
            tags$li(code("Ctrl+2"), " - Move to Console"),
            tags$li(code("Ctrl+Shift+C"), " - Comment/Uncomment lines"),
            tags$li(code("Ctrl+Enter"), " - Run current line/section")
          )
        )
      )
    ),

    nav_panel(
      "About",
      icon = icon("info-circle"),
      card(
        card_body(
          h3("R Library Cheatsheets Helper"),
          p(
            "This page provides quick access to cheatsheets and documentation for the main R libraries used in the Data Plotter tool.",
            "All external links open in new tabs to preserve your current work."
          ),
          h4("Sources"),
          tags$ul(
            tags$li(
              strong("RStudio Cheatsheets: "),
              "Official cheatsheets from the creators of RStudio and tidyverse packages"
            ),
            tags$li(
              strong("Package Documentation: "),
              "Official package websites and reference guides"
            ),
            tags$li(
              strong("Community Resources: "),
              "Well-maintained community documentation and wikis"
            )
          ),
          h4("Tips for Learning"),
          tags$ul(
            tags$li("Start with the official cheatsheets - they're designed to be comprehensive yet concise"),
            tags$li("Use the reference guides for detailed information about specific functions"),
            tags$li("Practice with your own data to reinforce learning"),
            tags$li("Don't hesitate to experiment - R has great error messages to help you learn")
          ),
          div(
            class = "alert alert-info",
            icon("lightbulb", class = "me-2"),
            strong("Pro Tip: "),
            "Bookmark this page! It's designed to be your go-to reference while working with the Data Plotter tool."
          )
        )
      )
    ),

    # Footer with quick navigation
    footer = card(
      class = "mt-3",
      card_body(
        class = "text-center",
        p(
          class = "mb-0",
          "Need help with a specific library? ",
          a(
            href = "#",
            onclick = "window.open('https://www.rdocumentation.org/', '_blank')",
            "Search R Documentation",
            icon("external-link-alt", class = "ms-1")
          )
        )
      )
    )
  )
}