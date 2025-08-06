# Faceting & Final Module for Static Plots

ggplot_faceting_final_template <- "# Faceting & Final Assembly
# Available: p (complete plot), plot_df, input (UI inputs), xlim_values, ylim_values  
# Purpose: Add faceting, resolve coord_cartesian vs free scales, apply final legend

# Check if faceting is being used
has_faceting <- !is.null(input$plot_facet_end) && input$plot_facet_end > 0 && 'series' %in% names(plot_df)

# Apply hard boundaries using coord_cartesian for precise plot edges
# Note: coord_cartesian conflicts with facet free scales, so we only apply it when not faceting
if ((!is.null(xlim_values) || !is.null(ylim_values)) && !has_faceting) {
  p <- p + coord_cartesian(xlim = xlim_values, ylim = ylim_values, expand = FALSE)
} else if (!is.null(xlim_values) || !is.null(ylim_values)) {
  # For faceted plots, use scale limits instead of coord_cartesian to preserve free scales
  if (!is.null(xlim_values)) {
    p <- p + scale_x_continuous(limits = xlim_values, expand = c(0, 0))
  }
  if (!is.null(ylim_values)) {
    p <- p + scale_y_continuous(limits = ylim_values, expand = c(0, 0))
  }
}

# Line legends are now handled in the Statistical Overlays template
# No additional legend processing needed here

# Add faceting if specified
if (has_faceting) {
  facet_start <- if(input$plot_facet_start > 0) input$plot_facet_start else 1
  plot_df$facet_var <- str_sub(plot_df$series, facet_start, input$plot_facet_end)
  p <- p + facet_wrap(~facet_var, nrow = input$plot_facet_nrow, scales = 'free_y')
}

p # Return final ggplot object
"