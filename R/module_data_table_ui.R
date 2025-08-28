
ui_data_table_display <- function(id, r_code_on_df, title = NULL) {
  ns <- NS(id)
  card( # Wrapped in card for better structure
    full_screen = TRUE,
    card_header(
      title,
      popover(
        # Gear icon on the right
        bsicons::bs_icon("gear", class = "ms-auto"),
        title = "Table Display R Code",
        placement = "bottom",
        options = list(
          container = "body",
          customClass = "large-popover"
        ),
        # Use fixed height for popover ace editor
        aceEditor_pre(
            ns("code_input"),
            value = r_code_on_df,
            # height = "500px",
            maxLines = 50
        ),
        action_input_tip(
          ns("apply_code"), 
          "Apply Display Code", 
          tip = "Apply R code to customize table view"
        )
      )
    ),
    card_body(
        padding = "0.25rem", # Reduced padding
        style = "height: 100%; display: flex; flex-direction: column;",
        uiOutput(ns("data_table"), style = "height: 100%; min-height: 300px;") # For datatable or other renderPrint outputs
    )
  )
}