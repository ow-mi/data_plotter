# Grid & Axes Module for Static Plots
# This is a condensed version of the grid & axes template

ggplot_grid_axes_template <- "# Grid & Axes Controls
# Grid & Axes Controls
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
# 1. Helper to compute safe sequences with dynamic rescaling
# -----------------------------------------------------------------------------
safe_seq <- function(from, to, by, max_n = 1000) {
  if (is.null(from) || is.null(to) || is.na(from) || is.na(to)) return(NULL)
  if (by <= 0) return(NULL)
  
  range_size <- to - from
  n <- range_size / by
  
  if (n > max_n) {
    # Dynamically scale 'by' upwards until breaks are under max_n
    new_by <- ceiling(range_size / max_n)
    warning(sprintf(
      'Requested grid step %.6f would create %.0f breaks. Rescaling to %.6f.',
      by, n, new_by
    ))
    by <- new_by
  }
  
  # Final safety: if still broken, fallback to pretty()
  n <- range_size / by
  if (n > max_n * 2) {
    warning('Still too many breaks after rescaling. Using pretty().')
    return(pretty(c(from, to), n = 10))
  }
  
  seq(from = from, to = to, by = by)
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
      time_diff_seconds <- as.numeric(difftime(b, a, units = 'secs'))
      step_seconds <- time_diff_seconds / (n_between + 1)
      for (j in 1:n_between) {
        minor_time <- a + (j * step_seconds)
        minors <- c(minors, as.numeric(minor_time))
      }
    } else {
      step <- (b - a) / (n_between + 1)
      minor_vals <- seq(a + step, b - step, length.out = n_between)
      minors <- c(minors, minor_vals)
    }
  }
  
  if (length(majors) > 0 && inherits(majors[1], c('POSIXct', 'POSIXt'))) {
    minors <- as.POSIXct(minors, origin = '1970-01-01', tz = attr(majors[1], 'tzone'))
  } else if (length(majors) > 0 && inherits(majors[1], 'Date')) {
    minors <- as.Date(minors, origin = '1970-01-01')
  }
  
  return(minors)
}

# -----------------------------------------------------------------------------
# 3. Compute major breaks with safety checks
# -----------------------------------------------------------------------------
x_breaks <- NULL
y_breaks <- NULL
x_minor_breaks <- NULL
y_minor_breaks <- NULL

vertical_grid_type <- if(!is.null(input$vertical_grid_type)) input$vertical_grid_type else 'auto'

# X-axis breaks
if (enable_maj_x) {
  use_timestamp_mode <- (vertical_grid_type == 'timestamp') || 
                       (vertical_grid_type == 'auto' && inherits(plot_df$x_axis, c('POSIXct', 'POSIXt', 'Date')))
  
  if (use_timestamp_mode) {
    x_data_range <- range(plot_df$x_axis, na.rm = TRUE)
    interval_seconds <- maj_x_by * 60
    
    start_time <- x_data_range[1]
    end_time   <- x_data_range[2]
    
    x_breaks <- safe_seq(start_time, end_time, by = interval_seconds)
    
    if (length(x_breaks) < 2) {
      x_breaks <- seq(from = start_time, to = end_time, length.out = 3)
    }
  } else {
    x_numeric_data <- as.numeric(plot_df$x_axis)
    x_data_range <- range(x_numeric_data, na.rm = TRUE)
    
    x_breaks <- safe_seq(
      from = floor(x_data_range[1] / maj_x_by) * maj_x_by,
      to   = ceiling(x_data_range[2] / maj_x_by) * maj_x_by,
      by   = maj_x_by
    )
  }
  
  if (enable_min_x && !is.null(x_breaks)) {
    x_minor_breaks <- make_minor_breaks(x_breaks, nmin_x)
  }
}

# Y-axis breaks
if (enable_maj_y && is.numeric(plot_df$value)) {
  y_data_range <- range(plot_df$value, na.rm = TRUE)
  y_breaks <- safe_seq(
    from = floor(y_data_range[1] / maj_y_by) * maj_y_by,
    to   = ceiling(y_data_range[2] / maj_y_by) * maj_y_by,
    by   = maj_y_by
  )
  
  if (enable_min_y && !is.null(y_breaks)) {
    y_minor_breaks <- make_minor_breaks(y_breaks, nmin_y)
  }
}

# -----------------------------------------------------------------------------
# 4. Apply scales
# -----------------------------------------------------------------------------
if (!is.null(x_breaks)) {
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
    p <- p + scale_x_continuous(
      trans = x_trans,
      breaks = x_breaks,
      minor_breaks = x_minor_breaks
    )
  }
} else {
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
"