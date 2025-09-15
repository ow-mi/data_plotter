# Faceting & Final Module for Static Plots

ggplot_faceting_final_template <- "# Faceting & Final Assembly
library(data.table)
library(ggplot2)

# Faceting & Final Assembly
# Available: p (complete plot), plot_df, input (UI inputs), xlim_values, ylim_values

# 1. ALWAYS apply hard boundaries using coord_cartesian for a true zoom effect.
# This works for both single and faceted plots.
p <- p + coord_cartesian(xlim = xlim_values, ylim = ylim_values, expand = FALSE)

# Check if faceting is being used
has_faceting <- !is.null(input$plot_facet_end) && input$plot_facet_end != 0 && 'series' %in% names(plot_df)

# 2. Add faceting if specified.
if (has_faceting) {
  facet_start <- if (input$plot_facet_start != 0) input$plot_facet_start else 1
  
  # Create the faceting variable using data.table syntax
  plot_df[, facet_var := str_sub(series, facet_start, input$plot_facet_end)]
  
  # Add the facet_wrap layer. Control the scales here!
  # Change scales to 'free', 'free_x', or 'free_y' as needed.
  p <- p + facet_wrap(
    ~facet_var,
    nrow = input$plot_facet_nrow,
    scales = 'free' # <-- MODIFY THIS to 'free' or 'free_y' etc.
  )
}

# NOTE: This section below FILTERS your data before plotting.
# This is different from the visual zoom of coord_cartesian. Be sure this is intended.
min_ts <- df[, min(timestamp)]
end_ts <- min_ts + hours(40) # 40 hours * 3600 seconds/hour
df <- df[between(timestamp, min_ts, end_ts)]

p # Return final ggplot object
"