# Example Plotly Code for WebAssembly Environment
# Use these examples when ggplot2 is not available

# ========================================
# EXAMPLE 1: Basic Line Plot with Plotly
# ========================================
if (exists("ggplot2_available") && !ggplot2_available) {
  # Pure plotly approach
  plot_ly(df, x = ~date, y = ~value, color = ~series, type = 'scatter', mode = 'lines') %>%
    layout(
      title = "Time Series Plot",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Value"),
      hovermode = 'closest'
    )
} else {
  # Regular ggplot2 approach
  ggplot(df, aes(x = date, y = value, color = series)) +
    geom_line() +
    labs(title = "Time Series Plot", x = "Date", y = "Value") +
    theme_bw()
}

# ========================================
# EXAMPLE 2: Scatter Plot with Plotly
# ========================================
if (exists("ggplot2_available") && !ggplot2_available) {
  # Pure plotly approach
  plot_ly(df, x = ~x_col, y = ~y_col, color = ~category, 
          type = 'scatter', mode = 'markers',
          text = ~paste("Category:", category, "<br>X:", x_col, "<br>Y:", y_col),
          hovertemplate = "%{text}<extra></extra>") %>%
    layout(
      title = "Scatter Plot",
      xaxis = list(title = "X Variable"),
      yaxis = list(title = "Y Variable")
    )
} else {
  # Regular ggplot2 approach  
  ggplot(df, aes(x = x_col, y = y_col, color = category)) +
    geom_point() +
    labs(title = "Scatter Plot", x = "X Variable", y = "Y Variable") +
    theme_bw()
}

# ========================================
# EXAMPLE 3: Faceted Plot with Plotly
# ========================================
if (exists("ggplot2_available") && !ggplot2_available) {
  # Create separate plots for each facet
  unique_categories <- unique(df$category)
  plot_list <- lapply(unique_categories, function(cat) {
    df_subset <- df[df$category == cat, ]
    plot_ly(df_subset, x = ~date, y = ~value, type = 'scatter', mode = 'lines',
            name = cat) %>%
      layout(title = paste("Category:", cat),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Value"))
  })
  subplot(plot_list, nrows = length(unique_categories), shareX = TRUE, shareY = TRUE)
} else {
  # Regular ggplot2 approach
  ggplot(df, aes(x = date, y = value)) +
    geom_line() +
    facet_wrap(~category) +
    labs(title = "Faceted Time Series", x = "Date", y = "Value") +
    theme_bw()
}

# ========================================
# EXAMPLE 4: Bar Chart with Plotly
# ========================================
if (exists("ggplot2_available") && !ggplot2_available) {
  # Aggregate data first
  summary_df <- df %>%
    group_by(category) %>%
    summarise(avg_value = mean(value, na.rm = TRUE), .groups = 'drop')
  
  plot_ly(summary_df, x = ~category, y = ~avg_value, type = 'bar',
          text = ~round(avg_value, 2), textposition = 'outside') %>%
    layout(
      title = "Average Values by Category",
      xaxis = list(title = "Category"),
      yaxis = list(title = "Average Value")
    )
} else {
  # Regular ggplot2 approach
  df %>%
    group_by(category) %>%
    summarise(avg_value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
    ggplot(aes(x = category, y = avg_value)) +
    geom_col() +
    labs(title = "Average Values by Category", x = "Category", y = "Average Value") +
    theme_bw()
}

# ========================================
# EXAMPLE 5: Advanced Multi-trace Plot
# ========================================
if (exists("ggplot2_available") && !ggplot2_available) {
  # Create base plot
  p <- plot_ly(df, x = ~date)
  
  # Add traces for each series
  unique_series <- unique(df$series)
  for (s in unique_series) {
    df_series <- df[df$series == s, ]
    p <- p %>% add_lines(data = df_series, y = ~value, name = s, 
                        line = list(width = 2))
  }
  
  # Add layout
  p %>% layout(
    title = "Multi-Series Time Plot",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Value"),
    hovermode = 'x unified',
    legend = list(x = 0.02, y = 0.98)
  )
} else {
  # Regular ggplot2 approach
  ggplot(df, aes(x = date, y = value, color = series)) +
    geom_line(size = 1) +
    labs(title = "Multi-Series Time Plot", x = "Date", y = "Value") +
    theme_bw() +
    theme(legend.position = c(0.02, 0.98))
}

# ========================================
# NOTE FOR USERS:
# ========================================
# If you see warnings about ggplot2 functions not being available,
# use the plotly examples above. Plotly is more reliable in the 
# WebAssembly environment and provides interactive plots. 