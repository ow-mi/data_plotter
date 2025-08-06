# Grid & Axes Module for Static Plots
# This is a condensed version of the grid & axes template

ggplot_grid_axes_template <- "# Grid & Axes Controls
# Available: p (plot with overlays), plot_df, input (UI inputs)
# Purpose: Apply grid controls, axis transformations, and limits

# transformations
x_trans <- if(!is.null(input$x_trans)) input$x_trans else 'identity'
y_trans <- if(!is.null(input$y_trans)) input$y_trans else 'identity'

# major grid spacing
maj_x_by  <- if(!is.null(input$major_vgrid_breaks)) input$major_vgrid_breaks else 10
maj_y_by  <- if(!is.null(input$major_hgrid_breaks)) input$major_hgrid_breaks else 5

# number of minor intervals BETWEEN each pair of majors
nmin_x    <- if(!is.null(input$minor_vgrid_breaks)) input$minor_vgrid_breaks else 3
nmin_y    <- if(!is.null(input$minor_hgrid_breaks)) input$minor_hgrid_breaks else 3

# major grid line styles
maj_x_col <- if(!is.null(input$major_vgrid_color)) input$major_vgrid_color else 'grey80'
maj_y_col <- if(!is.null(input$major_hgrid_color)) input$major_hgrid_color else 'grey80'
maj_x_lw  <- if(!is.null(input$major_vgrid_linewidth)) input$major_vgrid_linewidth else 0.5
maj_y_lw  <- if(!is.null(input$major_hgrid_linewidth)) input$major_hgrid_linewidth else 0.5
maj_x_lt  <- if(!is.null(input$major_vgrid_linetype)) input$major_vgrid_linetype else 'solid'
maj_y_lt  <- if(!is.null(input$major_hgrid_linetype)) input$major_hgrid_linetype else 'solid'

# minor grid line styles
min_x_col <- if(!is.null(input$minor_vgrid_color)) input$minor_vgrid_color else 'grey95'
min_y_col <- if(!is.null(input$minor_hgrid_color)) input$minor_hgrid_color else 'grey95'
min_x_lw  <- if(!is.null(input$minor_vgrid_linewidth)) input$minor_vgrid_linewidth else 0.25
min_y_lw  <- if(!is.null(input$minor_hgrid_linewidth)) input$minor_hgrid_linewidth else 0.25
min_x_lt  <- if(!is.null(input$minor_vgrid_linetype)) input$minor_vgrid_linetype else 'dashed'
min_y_lt  <- if(!is.null(input$minor_hgrid_linetype)) input$minor_hgrid_linetype else 'dashed'

# grid enable flags
enable_maj_x <- !is.null(input$enable_major_vgrid) && input$enable_major_vgrid == TRUE
enable_maj_y <- !is.null(input$enable_major_hgrid) && input$enable_major_hgrid == TRUE
enable_min_x <- !is.null(input$enable_minor_vgrid) && input$enable_minor_vgrid == TRUE
enable_min_y <- !is.null(input$enable_minor_hgrid) && input$enable_minor_hgrid == TRUE

# axis hard limits
y_start_enabled <- !is.null(input$enable_y_start) && input$enable_y_start == TRUE
y_end_enabled <- !is.null(input$enable_y_end) && input$enable_y_end == TRUE

if (y_start_enabled || y_end_enabled) {
  y_data_range <- range(plot_df$value, na.rm = TRUE)
  y_start <- if(y_start_enabled && !is.null(input$y_start)) input$y_start else y_data_range[1]
  y_end <- if(y_end_enabled && !is.null(input$y_end)) input$y_end else y_data_range[2]
  ylim_values <- c(y_start, y_end)
} else {
  ylim_values <- NULL
}

# X axis limits (support both numeric and timestamp)
x_limit_type <- if(!is.null(input$x_limit_type)) input$x_limit_type else 'numeric'

if (x_limit_type == 'numeric') {
  x_start_enabled <- !is.null(input$enable_x_start_numeric) && input$enable_x_start_numeric == TRUE
  x_end_enabled <- !is.null(input$enable_x_end_numeric) && input$enable_x_end_numeric == TRUE
  
  if (x_start_enabled || x_end_enabled) {
    x_data_range <- range(plot_df$x_axis, na.rm = TRUE)
    x_start <- if(x_start_enabled && !is.null(input$x_start_numeric)) input$x_start_numeric else x_data_range[1]
    x_end <- if(x_end_enabled && !is.null(input$x_end_numeric)) input$x_end_numeric else x_data_range[2]
    xlim_values <- c(x_start, x_end)
  } else {
    xlim_values <- NULL
  }
} else if (x_limit_type == 'timestamp') {
  x_start_enabled <- !is.null(input$enable_x_start_timestamp) && input$enable_x_start_timestamp == TRUE
  x_end_enabled <- !is.null(input$enable_x_end_timestamp) && input$enable_x_end_timestamp == TRUE
  
  if (x_start_enabled || x_end_enabled) {
    x_data_range <- range(plot_df$x_axis, na.rm = TRUE)
    
    x_start <- if(x_start_enabled && !is.null(input$x_start_date) && !is.null(input$x_start_time)) {
      as.POSIXct(paste(
        as.character(input$x_start_date), 
        format(input$x_start_time, '%H:%M:%S')
      ))
    } else {
      x_data_range[1]
    }
    
    x_end <- if(x_end_enabled && !is.null(input$x_end_date) && !is.null(input$x_end_time)) {
      as.POSIXct(paste(
        as.character(input$x_end_date), 
        format(input$x_end_time, '%H:%M:%S')
      ))
    } else {
      x_data_range[2]
    }
    
    xlim_values <- c(x_start, x_end)
  } else {
    xlim_values <- NULL
  }
} else {
  xlim_values <- NULL
}

# -----------------------------------------------------------------------------
# 2. Helper to compute minor breaks from majors + a count
# -----------------------------------------------------------------------------
make_minor_breaks <- function(majors, n_between) {
  if (length(majors) < 2 || n_between < 1) return(numeric(0))
  
  minors <- c()
  for (i in seq_len(length(majors) - 1)) {
    a <- majors[i]
    b <- majors[i + 1]
    
    if (inherits(a, c('POSIXct', 'POSIXt', 'Date'))) {
      # For time data - handle POSIXct properly
      time_diff_seconds <- as.numeric(difftime(b, a, units = 'secs'))
      step_seconds <- time_diff_seconds / (n_between + 1)
      
      # Create minor time points
      for (j in 1:n_between) {
        minor_time <- a + (j * step_seconds)
        minors <- c(minors, as.numeric(minor_time))
      }
    } else {
      # For numeric data
      step <- (b - a) / (n_between + 1)
      minor_vals <- seq(a + step, b - step, length.out = n_between)
      minors <- c(minors, minor_vals)
    }
  }
  
  # Convert back to original class
  if (length(majors) > 0 && inherits(majors[1], c('POSIXct', 'POSIXt'))) {
    minors <- as.POSIXct(minors, origin = '1970-01-01', tz = attr(majors[1], 'tzone'))
  } else if (length(majors) > 0 && inherits(majors[1], 'Date')) {
    minors <- as.Date(minors, origin = '1970-01-01')
  }
  
  return(minors)
}

# -----------------------------------------------------------------------------
# 3. Compute major breaks based on data type
# -----------------------------------------------------------------------------
x_breaks <- NULL
y_breaks <- NULL
x_minor_breaks <- NULL
y_minor_breaks <- NULL

# Get vertical grid type setting (user choice: 'auto', 'numeric', 'timestamp')
vertical_grid_type <- if(!is.null(input$vertical_grid_type)) input$vertical_grid_type else 'auto'

# X-axis breaks (handle timestamps and numeric based on user choice)
if (enable_maj_x) {
  # Determine how to treat x-axis data based on user selection
  use_timestamp_mode <- (vertical_grid_type == 'timestamp') || 
                       (vertical_grid_type == 'auto' && inherits(plot_df$x_axis, c('POSIXct', 'POSIXt', 'Date')))
  
  if (use_timestamp_mode) {
    # For timestamp data, maj_x_by is interpreted as minutes
    x_data_range <- range(plot_df$x_axis, na.rm = TRUE)
    
    # Create breaks at exact minute intervals for consistent spacing
    interval_seconds <- maj_x_by * 60  # Convert minutes to seconds
    
    # Round start time down to nearest interval and end time up to nearest interval
    start_time <- x_data_range[1]
    end_time <- x_data_range[2]
    
    # Create sequence with exact time intervals
    x_breaks <- seq(from = start_time, to = end_time, by = interval_seconds)
    
    # Ensure we have at least 2 breaks for the grid to work
    if (length(x_breaks) < 2) {
      # If interval is too large, create at least start and end
      x_breaks <- seq(from = start_time, to = end_time, length.out = 3)
    }
  } else {
    # For numeric data (force convert if needed)
    x_numeric_data <- as.numeric(plot_df$x_axis)
    x_data_range <- range(x_numeric_data, na.rm = TRUE)
    x_breaks <- seq(
      from = floor(x_data_range[1] / maj_x_by) * maj_x_by,
      to = ceiling(x_data_range[2] / maj_x_by) * maj_x_by,
      by = maj_x_by
    )
  }
  
  # Calculate minor breaks if enabled
  if (enable_min_x && !is.null(x_breaks)) {
    x_minor_breaks <- make_minor_breaks(x_breaks, nmin_x)
  }
}

# Y-axis breaks (always numeric in this case)
if (enable_maj_y && is.numeric(plot_df$value)) {
  y_data_range <- range(plot_df$value, na.rm = TRUE)
  y_breaks <- seq(
    from = floor(y_data_range[1] / maj_y_by) * maj_y_by,
    to = ceiling(y_data_range[2] / maj_y_by) * maj_y_by,
    by = maj_y_by
  )
  
  # Calculate minor breaks if enabled
  if (enable_min_y && !is.null(y_breaks)) {
    y_minor_breaks <- make_minor_breaks(y_breaks, nmin_y)
  }
}

# -----------------------------------------------------------------------------
# 4. Apply scales with custom breaks (trans only works with continuous scales)
# -----------------------------------------------------------------------------
if (!is.null(x_breaks)) {
  # Determine which scale function to use
  use_timestamp_mode <- (vertical_grid_type == 'timestamp') || 
                       (vertical_grid_type == 'auto' && inherits(plot_df$x_axis, c('POSIXct', 'POSIXt', 'Date')))
  
  if (use_timestamp_mode) {
    if (inherits(plot_df$x_axis, c('POSIXct', 'POSIXt'))) {
      p <- p + scale_x_datetime(
        breaks = x_breaks,
        minor_breaks = x_minor_breaks
      )
    } else if (inherits(plot_df$x_axis, 'Date')) {
      p <- p + scale_x_date(
        breaks = x_breaks,
        minor_breaks = x_minor_breaks
      )
    }
  } else {
    # Use continuous scale with transformation support
    p <- p + scale_x_continuous(
      trans = x_trans,
      breaks = x_breaks,
      minor_breaks = x_minor_breaks
    )
  }
} else {
  # No custom breaks - apply appropriate scale
  use_timestamp_mode <- (vertical_grid_type == 'timestamp') || 
                       (vertical_grid_type == 'auto' && inherits(plot_df$x_axis, c('POSIXct', 'POSIXt', 'Date')))
  
  if (use_timestamp_mode) {
    if (inherits(plot_df$x_axis, c('POSIXct', 'POSIXt'))) {
      p <- p + scale_x_datetime()
    } else if (inherits(plot_df$x_axis, 'Date')) {
      p <- p + scale_x_date()
    }
  } else {
    p <- p + scale_x_continuous(trans = x_trans)
  }
}

if (!is.null(y_breaks)) {
  p <- p + scale_y_continuous(
    trans = y_trans,
    breaks = y_breaks,
    minor_breaks = y_minor_breaks
  )
} else {
  p <- p + scale_y_continuous(trans = y_trans)
}

# -----------------------------------------------------------------------------
# 5. Apply grid line theming
# -----------------------------------------------------------------------------
theme_updates <- list()

# Major grid lines
if (enable_maj_x) {
  theme_updates$panel.grid.major.x <- element_line(
    color = maj_x_col,
    linewidth = maj_x_lw,
    linetype = maj_x_lt
  )
} else {
  theme_updates$panel.grid.major.x <- element_blank()
}

if (enable_maj_y) {
  theme_updates$panel.grid.major.y <- element_line(
    color = maj_y_col,
    linewidth = maj_y_lw,
    linetype = maj_y_lt
  )
} else {
  theme_updates$panel.grid.major.y <- element_blank()
}

# Minor grid lines
if (enable_min_x) {
  theme_updates$panel.grid.minor.x <- element_line(
    color = min_x_col,
    linewidth = min_x_lw,
    linetype = min_x_lt
  )
} else {
  theme_updates$panel.grid.minor.x <- element_blank()
}

if (enable_min_y) {
  theme_updates$panel.grid.minor.y <- element_line(
    color = min_y_col,
    linewidth = min_y_lw,
    linetype = min_y_lt
  )
} else {
  theme_updates$panel.grid.minor.y <- element_blank()
}

# Apply all theme updates at once
p <- p + do.call(theme, theme_updates)

# Store limits for use in final template
assign('xlim_values', xlim_values, envir = .GlobalEnv) 
assign('ylim_values', ylim_values, envir = .GlobalEnv)

p # Return plot with grid and axis controls
"

# NOTE: The complete grid & axes template is very extensive (lines 810-1125)
# It includes detailed implementations for:
# - Complex break calculations for both numeric and timestamp data
# - Minor break generation functions
# - Grid line styling with full control over colors, linetypes, linewidths
# - Timestamp vs numeric handling for vertical grids
# - X and Y axis limit controls with both numeric and timestamp support
# For the complete implementation, please refer to the original r_code_.R file