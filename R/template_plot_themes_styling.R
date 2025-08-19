# Themes & Styling Module for Static Plots

ggplot_themes_styling_template <- "# Themes & Styling
# Available: p (base plot), plot_df, input (UI inputs), x_label
# Purpose: Apply themes, colors, fonts, and legend settings

# Apply theme
theme_choice <- if(!is.null(input$plot_theme)) input$plot_theme else 'theme_classic'
if (theme_choice == 'theme_classic') {
  p <- p + theme_classic()
} else if (theme_choice == 'theme_minimal') {
  p <- p + theme_minimal()
} else if (theme_choice == 'theme_dark') {
  p <- p + theme_dark()
} else if (theme_choice == 'theme_light') {
  p <- p + theme_light()
} else if (theme_choice == 'theme_bw') {
  p <- p + theme_bw()
} else if (theme_choice == 'theme_void') {
  p <- p + theme_void()
} else {
  p <- p + theme_classic()
}

# Apply color palette
if (!is.null(input$plot_color) && input$plot_color != 'null' && input$plot_color %in% names(plot_df)) {
  color_palette <- if(!is.null(input$color_palette)) input$color_palette else 'default'
  
  if (color_palette == 'paletteer') {
    p <- p + scale_colour_paletteer_d(input$color_palette_paletter)
  } else if (color_palette == 'viridis') {
    p <- p + scale_color_viridis_d()
  } else if (color_palette %in% c('Set1', 'Set2', 'Dark2', 'Paired')) {
    p <- p + scale_color_brewer(type = 'qual', palette = color_palette)
  }
  # Default palette is handled automatically by ggplot
}

# Get font sizes
title_size <- if(!is.null(input$title_font_size)) input$title_font_size else 14
xaxis_size <- if(!is.null(input$xaxis_font_size)) input$xaxis_font_size else 12
yaxis_size <- if(!is.null(input$yaxis_font_size)) input$yaxis_font_size else 12
legend_size <- if(!is.null(input$legend_font_size)) input$legend_font_size else 12

# Get caption settings
caption_size <- if(!is.null(input$caption_font_size)) input$caption_font_size else 10
caption_color <- if(!is.null(input$caption_color)) input$caption_color else 'grey50'
caption_x <- if(!is.null(input$caption_x)) input$caption_x else 1
caption_y <- if(!is.null(input$caption_y)) input$caption_y else 0

# Apply legend settings
legend_pos <- if(!is.null(input$legend_position)) input$legend_position else 'right'

# Customize theme with all settings
p <- p + theme(
  plot.title = element_text(hjust = 0.5, size = title_size),
  axis.title.x = element_text(size = xaxis_size),
  axis.title.y = element_text(size = yaxis_size),
  axis.text.x = element_text(size = xaxis_size * 0.9),
  axis.text.y = element_text(size = yaxis_size * 0.9),
  legend.position = legend_pos,
  legend.text = element_text(size = legend_size),
  legend.title = element_text(size = legend_size),
  plot.caption = element_text(
    size = caption_size,
    color = caption_color,
    hjust = caption_x,
    vjust = caption_y
  )
)

# Process caption with string_eval for dynamic content
caption_text <- if(!is.null(input$plot_caption) && nzchar(input$plot_caption)) {
  # Create environment with plot_df available for string evaluation
  eval_env <- new.env(parent = .GlobalEnv)
  eval_env$df <- plot_df
  eval_env$plot_df <- plot_df
  # Apply string_eval to process any quoted R code in the caption
  string_eval(input$plot_caption, env = eval_env)
} else {
  NULL
}

# Add labels and title
plot_labs <- labs(
  title = input$plot_title |> string_eval(),
  x = x_label |> string_eval(),
  y = input$plot_ylabel |> string_eval(),
  caption = caption_text |> string_eval()
)

# Add proper legend labels
if (!is.null(input$plot_color) && input$plot_color != 'null' && input$plot_color %in% names(plot_df)) {
  plot_labs$colour <- input$plot_color
}
if (!is.null(input$plot_linetype) && input$plot_linetype != 'null' && input$plot_linetype %in% names(plot_df)) {
  plot_labs$linetype <- input$plot_linetype
}

p <- p + plot_labs

p # Return styled plot
"