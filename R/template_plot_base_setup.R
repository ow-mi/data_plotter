# Base Plot Setup Module for Static Plots

ggplot_base_setup_template <- "# Base Plot Setup 
# Available: plot_df (processed data), input (UI inputs), x_label
# Purpose: Create base plot with aesthetics and geometry

# Build aesthetic mapping
aes_mapping <- aes(x = x_axis, y = value)

# Add color mapping if specified
if (!is.null(input$plot_color) && input$plot_color != 'null' && input$plot_color %in% names(plot_df)) {
  aes_mapping$colour <- as.name(input$plot_color)
}

# Add line type mapping if specified  
if (!is.null(input$plot_linetype) && input$plot_linetype != 'null' && input$plot_linetype %in% names(plot_df)) {
  aes_mapping$linetype <- as.name(input$plot_linetype)
}

# Get size and alpha values
line_width <- if(!is.null(input$line_width)) input$line_width else 1
point_size <- if(!is.null(input$point_size)) input$point_size else 2
alpha_val <- if(!is.null(input$alpha)) input$alpha else 1.0

# Create plot with combined aesthetics
p <- ggplot(plot_df, aes_mapping)

# Choose geometry based on geom_type input
geom_type <- if(!is.null(input$geom_type)) input$geom_type else 'geom_line'

if (geom_type == 'geom_line') {
  p <- p + geom_line(linewidth = line_width, alpha = alpha_val)
} else if (geom_type == 'geom_point') {
  p <- p + geom_point(size = point_size, alpha = alpha_val)
} else if (geom_type == 'geom_line_point') {
  p <- p + geom_line(linewidth = line_width, alpha = alpha_val) + 
           geom_point(size = point_size, alpha = alpha_val)
} else if (geom_type == 'geom_area') {
  p <- p + geom_area(alpha = alpha_val * 0.7)
} else if (geom_type == 'geom_smooth') {
  p <- p + geom_smooth(method = 'loess', se = TRUE, alpha = alpha_val * 0.3, linewidth = line_width)
} else if (geom_type == 'geom_col') {
  p <- p + geom_col(alpha = alpha_val)
} else {
  # Default fallback
  p <- p + geom_line(linewidth = line_width, alpha = alpha_val)
}

p # Return base plot
"