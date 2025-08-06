# Statistical Overlays Module for Static Plots
# This file contains the statistical overlays template (lines ~471-807 in original)


ggplot_statistical_overlays_template <- "# Statistical Overlays & Lines
# Available: p (styled plot), plot_df, input (UI inputs)
# Purpose: Add trend lines, mean/median lines, reference lines

# Collect line legend information for the new linetype-based approach
line_legend_names <- c()
line_legend_colors <- c()
line_legend_linetypes <- c()

# Add statistical overlays with enhanced controls
if (!is.null(input$add_smooth) && input$add_smooth == TRUE) {
  smooth_method <- if(!is.null(input$smooth_method)) input$smooth_method else 'loess'
  smooth_linetype <- if(!is.null(input$smooth_linetype)) input$smooth_linetype else 'solid'
  smooth_linewidth <- if(!is.null(input$smooth_linewidth)) input$smooth_linewidth else 1
  smooth_color <- if(!is.null(input$smooth_color)) input$smooth_color else 'red'
  smooth_legend <- if(!is.null(input$smooth_legend)) input$smooth_legend else TRUE
  smooth_name <- if(!is.null(input$smooth_name) && nzchar(input$smooth_name)) input$smooth_name else 'Trend'
  
  if (smooth_legend) {
    # Try to extract equation from smooth fit for legend name
    tryCatch({
      if (smooth_method == 'lm') {
        # For linear model, extract coefficients
        temp_model <- lm(value ~ x_axis, data = plot_df)
        coef_vals <- coef(temp_model)
        if (length(coef_vals) >= 2) {
          equation <- paste0('y = ', round(coef_vals[2], 3), 'x + ', round(coef_vals[1], 3))
          smooth_name_with_eq <- paste0(smooth_name, ' (', equation, ')')
        } else {
          smooth_name_with_eq <- smooth_name
        }
      } else {
        # For other methods, just use the method name
        smooth_name_with_eq <- paste0(smooth_name, ' (', smooth_method, ')')
      }
    }, error = function(e) {
      smooth_name_with_eq <- smooth_name
    })
    
    line_legend_names <- c(line_legend_names, smooth_name_with_eq)
    line_legend_colors <- c(line_legend_colors, smooth_color)
    line_legend_linetypes <- c(line_legend_linetypes, smooth_linetype)
    
    # Add trend line with linetype mapping for legend
    p <- p + geom_smooth(
      aes(linetype = smooth_name_with_eq),
      method = smooth_method, 
      se = TRUE, 
      alpha = 0.3, 
      linewidth = smooth_linewidth,
      color = smooth_color,
      show.legend = TRUE
    )
  } else {
    # Add trend line with no legend
    p <- p + geom_smooth(
      method = smooth_method, 
      se = TRUE, 
      alpha = 0.3, 
      linetype = smooth_linetype,
      linewidth = smooth_linewidth,
      color = smooth_color,
      show.legend = FALSE
    )
  }
}

# Mean and Median Lines - REMOVED

# Enhanced reference lines with full control
if (!is.null(input$enable_hline_1) && input$enable_hline_1 == TRUE && !is.null(input$hline_1)) {
  hline_1_linetype <- if(!is.null(input$hline_1_linetype)) input$hline_1_linetype else 'dashed'
  hline_1_linewidth <- if(!is.null(input$hline_1_linewidth)) input$hline_1_linewidth else 1
  hline_1_color <- if(!is.null(input$hline_1_color)) input$hline_1_color else 'red'
  hline_1_legend <- if(!is.null(input$hline_1_legend)) input$hline_1_legend else FALSE
  hline_1_name <- if(!is.null(input$hline_1_name) && nzchar(input$hline_1_name)) input$hline_1_name else 'H-Line 1'
  
  if (hline_1_legend) {
    line_legend_names <- c(line_legend_names, hline_1_name)
    line_legend_colors <- c(line_legend_colors, hline_1_color)
    line_legend_linetypes <- c(line_legend_linetypes, hline_1_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_hline(
      aes(yintercept = input$hline_1, linetype = hline_1_name),
      linewidth = hline_1_linewidth,
      color = hline_1_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_hline(
      yintercept = input$hline_1,
      linetype = hline_1_linetype,
      linewidth = hline_1_linewidth,
      color = hline_1_color,
      alpha = 0.7
    )
  }
}

if (!is.null(input$enable_hline_2) && input$enable_hline_2 == TRUE && !is.null(input$hline_2)) {
  hline_2_linetype <- if(!is.null(input$hline_2_linetype)) input$hline_2_linetype else 'dashed'
  hline_2_linewidth <- if(!is.null(input$hline_2_linewidth)) input$hline_2_linewidth else 1
  hline_2_color <- if(!is.null(input$hline_2_color)) input$hline_2_color else 'blue'
  hline_2_legend <- if(!is.null(input$hline_2_legend)) input$hline_2_legend else FALSE
  hline_2_name <- if(!is.null(input$hline_2_name) && nzchar(input$hline_2_name)) input$hline_2_name else 'H-Line 2'
  
  if (hline_2_legend) {
    line_legend_names <- c(line_legend_names, hline_2_name)
    line_legend_colors <- c(line_legend_colors, hline_2_color)
    line_legend_linetypes <- c(line_legend_linetypes, hline_2_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_hline(
      aes(yintercept = input$hline_2, linetype = hline_2_name),
      linewidth = hline_2_linewidth,
      color = hline_2_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_hline(
      yintercept = input$hline_2,
      linetype = hline_2_linetype,
      linewidth = hline_2_linewidth,
      color = hline_2_color,
      alpha = 0.7
    )
  }
}

if (!is.null(input$enable_hline_3) && input$enable_hline_3 == TRUE && !is.null(input$hline_3)) {
  hline_3_linetype <- if(!is.null(input$hline_3_linetype)) input$hline_3_linetype else 'dashed'
  hline_3_linewidth <- if(!is.null(input$hline_3_linewidth)) input$hline_3_linewidth else 1
  hline_3_color <- if(!is.null(input$hline_3_color)) input$hline_3_color else 'purple'
  hline_3_legend <- if(!is.null(input$hline_3_legend)) input$hline_3_legend else FALSE
  hline_3_name <- if(!is.null(input$hline_3_name) && nzchar(input$hline_3_name)) input$hline_3_name else 'H-Line 3'
  
  if (hline_3_legend) {
    line_legend_names <- c(line_legend_names, hline_3_name)
    line_legend_colors <- c(line_legend_colors, hline_3_color)
    line_legend_linetypes <- c(line_legend_linetypes, hline_3_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_hline(
      aes(yintercept = input$hline_3, linetype = hline_3_name),
      linewidth = hline_3_linewidth,
      color = hline_3_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_hline(
      yintercept = input$hline_3,
      linetype = hline_3_linetype,
      linewidth = hline_3_linewidth,
      color = hline_3_color,
      alpha = 0.7
    )
  }
}

if (!is.null(input$enable_hline_4) && input$enable_hline_4 == TRUE && !is.null(input$hline_4)) {
  hline_4_linetype <- if(!is.null(input$hline_4_linetype)) input$hline_4_linetype else 'dashed'
  hline_4_linewidth <- if(!is.null(input$hline_4_linewidth)) input$hline_4_linewidth else 1
  hline_4_color <- if(!is.null(input$hline_4_color)) input$hline_4_color else 'orange'
  hline_4_legend <- if(!is.null(input$hline_4_legend)) input$hline_4_legend else FALSE
  hline_4_name <- if(!is.null(input$hline_4_name) && nzchar(input$hline_4_name)) input$hline_4_name else 'H-Line 4'
  
  if (hline_4_legend) {
    line_legend_names <- c(line_legend_names, hline_4_name)
    line_legend_colors <- c(line_legend_colors, hline_4_color)
    line_legend_linetypes <- c(line_legend_linetypes, hline_4_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_hline(
      aes(yintercept = input$hline_4, linetype = hline_4_name),
      linewidth = hline_4_linewidth,
      color = hline_4_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_hline(
      yintercept = input$hline_4,
      linetype = hline_4_linetype,
      linewidth = hline_4_linewidth,
      color = hline_4_color,
      alpha = 0.7
    )
  }
}

# Enhanced vertical reference lines
if (!is.null(input$enable_vline_1) && input$enable_vline_1 == TRUE && !is.null(input$vline_1)) {
  vline_1_linetype <- if(!is.null(input$vline_1_linetype)) input$vline_1_linetype else 'dashed'
  vline_1_linewidth <- if(!is.null(input$vline_1_linewidth)) input$vline_1_linewidth else 1
  vline_1_color <- if(!is.null(input$vline_1_color)) input$vline_1_color else 'red'
  vline_1_legend <- if(!is.null(input$vline_1_legend)) input$vline_1_legend else FALSE
  vline_1_name <- if(!is.null(input$vline_1_name) && nzchar(input$vline_1_name)) input$vline_1_name else 'V-Line 1'
  
  if (vline_1_legend) {
    line_legend_names <- c(line_legend_names, vline_1_name)
    line_legend_colors <- c(line_legend_colors, vline_1_color)
    line_legend_linetypes <- c(line_legend_linetypes, vline_1_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_vline(
      aes(xintercept = input$vline_1, linetype = vline_1_name),
      linewidth = vline_1_linewidth,
      color = vline_1_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_vline(
      xintercept = input$vline_1,
      linetype = vline_1_linetype,
      linewidth = vline_1_linewidth,
      color = vline_1_color,
      alpha = 0.7
    )
  }
}

if (!is.null(input$enable_vline_2) && input$enable_vline_2 == TRUE && !is.null(input$vline_2)) {
  vline_2_linetype <- if(!is.null(input$vline_2_linetype)) input$vline_2_linetype else 'dashed'
  vline_2_linewidth <- if(!is.null(input$vline_2_linewidth)) input$vline_2_linewidth else 1
  vline_2_color <- if(!is.null(input$vline_2_color)) input$vline_2_color else 'blue'
  vline_2_legend <- if(!is.null(input$vline_2_legend)) input$vline_2_legend else FALSE
  vline_2_name <- if(!is.null(input$vline_2_name) && nzchar(input$vline_2_name)) input$vline_2_name else 'V-Line 2'
  
  if (vline_2_legend) {
    line_legend_names <- c(line_legend_names, vline_2_name)
    line_legend_colors <- c(line_legend_colors, vline_2_color)
    line_legend_linetypes <- c(line_legend_linetypes, vline_2_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_vline(
      aes(xintercept = input$vline_2, linetype = vline_2_name),
      linewidth = vline_2_linewidth,
      color = vline_2_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_vline(
      xintercept = input$vline_2,
      linetype = vline_2_linetype,
      linewidth = vline_2_linewidth,
      color = vline_2_color,
      alpha = 0.7
    )
  }
}

if (!is.null(input$enable_vline_3) && input$enable_vline_3 == TRUE && !is.null(input$vline_3)) {
  vline_3_linetype <- if(!is.null(input$vline_3_linetype)) input$vline_3_linetype else 'dashed'
  vline_3_linewidth <- if(!is.null(input$vline_3_linewidth)) input$vline_3_linewidth else 1
  vline_3_color <- if(!is.null(input$vline_3_color)) input$vline_3_color else 'purple'
  vline_3_legend <- if(!is.null(input$vline_3_legend)) input$vline_3_legend else FALSE
  vline_3_name <- if(!is.null(input$vline_3_name) && nzchar(input$vline_3_name)) input$vline_3_name else 'V-Line 3'
  
  if (vline_3_legend) {
    line_legend_names <- c(line_legend_names, vline_3_name)
    line_legend_colors <- c(line_legend_colors, vline_3_color)
    line_legend_linetypes <- c(line_legend_linetypes, vline_3_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_vline(
      aes(xintercept = input$vline_3, linetype = vline_3_name),
      linewidth = vline_3_linewidth,
      color = vline_3_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_vline(
      xintercept = input$vline_3,
      linetype = vline_3_linetype,
      linewidth = vline_3_linewidth,
      color = vline_3_color,
      alpha = 0.7
    )
  }
}

if (!is.null(input$enable_vline_4) && input$enable_vline_4 == TRUE && !is.null(input$vline_4)) {
  vline_4_linetype <- if(!is.null(input$vline_4_linetype)) input$vline_4_linetype else 'dashed'
  vline_4_linewidth <- if(!is.null(input$vline_4_linewidth)) input$vline_4_linewidth else 1
  vline_4_color <- if(!is.null(input$vline_4_color)) input$vline_4_color else 'orange'
  vline_4_legend <- if(!is.null(input$vline_4_legend)) input$vline_4_legend else FALSE
  vline_4_name <- if(!is.null(input$vline_4_name) && nzchar(input$vline_4_name)) input$vline_4_name else 'V-Line 4'
  
  if (vline_4_legend) {
    line_legend_names <- c(line_legend_names, vline_4_name)
    line_legend_colors <- c(line_legend_colors, vline_4_color)
    line_legend_linetypes <- c(line_legend_linetypes, vline_4_linetype)
    
    # Add line with linetype mapping for legend
    p <- p + geom_vline(
      aes(xintercept = input$vline_4, linetype = vline_4_name),
      linewidth = vline_4_linewidth,
      color = vline_4_color,
      alpha = 0.7
    )
  } else {
    # Add line with no legend
    p <- p + geom_vline(
      xintercept = input$vline_4,
      linetype = vline_4_linetype,
      linewidth = vline_4_linewidth,
      color = vline_4_color,
      alpha = 0.7
    )
  }
}

# Apply the new linetype-based legend system that preserves main plot legend
if (length(line_legend_names) > 0) {
  # Create named vectors for the scale
  legend_linetype_values <- setNames(line_legend_linetypes, line_legend_names)
  
  # Add the linetype scale with color override for the legend
  p <- p + scale_linetype_manual(
    name = 'Lines', 
    values = legend_linetype_values,
    guide = guide_legend(
      override.aes = list(color = line_legend_colors),
      order = 2  # Place lines legend after main legend
    )
  )
}

p # Return plot with overlays
"


# NOTE: The complete statistical_overlays template from the original file is quite long
# It includes detailed implementations for 4 horizontal lines and 4 vertical lines
# Each with individual controls for linetype, color, linewidth, legend settings, etc.
# For the complete implementation, refer to lines 471-807 in the original r_code_.R file