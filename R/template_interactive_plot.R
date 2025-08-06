# Interactive Plot Template (Plotly)

# This is a condensed version of the interactive template
ggplot_interactive_template <- r"---(
# Interactive plot code (plotly)
# Available: df (processed data), input (UI inputs)  
# Return: htmltools tagList object

# Ensure required libraries are available
library(plotly)
library(data.table)
library(htmltools)

# Handle X-axis transformation
plot_df <- copy(df) # Work on a copy

if ('timestamp' %in% names(plot_df)) {
  min_ts <- min(plot_df$timestamp, na.rm = TRUE)
  
  # Transform X-axis based on input selection
  if (!is.null(input$plot_xlabel)) {
    if (input$plot_xlabel == 'Duration Minutes') {
      plot_df[, x_axis := as.numeric(difftime(timestamp, min_ts, units = 'mins'))]
      x_label <- 'Duration (Minutes)'
    } else if (input$plot_xlabel == 'Duration Hours') {
      plot_df[, x_axis := as.numeric(difftime(timestamp, min_ts, units = 'hours'))]
      x_label <- 'Duration (Hours)'
    } else if (input$plot_xlabel == 'Duration Days') {
      plot_df[, x_axis := as.numeric(difftime(timestamp, min_ts, units = 'days'))]
      x_label <- 'Duration (Days)'
    } else if (input$plot_xlabel == 'Duration Seconds') {
      plot_df[, x_axis := as.numeric(difftime(timestamp, min_ts, units = 'secs'))]
      x_label <- 'Duration (Seconds)'
    } else {
      # Default to timestamp
      plot_df[, x_axis := timestamp]
      x_label <- input$plot_xlabel
    }
  } else {
    plot_df[, x_axis := timestamp]
    x_label <- 'Timestamp'
  }
} else {
  # No timestamp column
  plot_df[, x_axis := .I] # Use row index
  x_label <- 'Index'
}

# Create base plotly plot with transformed x-axis
if (!is.null(input$plot_color) && input$plot_color != 'null' && input$plot_color %in% names(plot_df)) {
  # With color grouping
  p <- plot_ly(plot_df, x = ~x_axis, y = ~value, 
               color = as.formula(paste('~', input$plot_color)),
               type = 'scatter', mode = 'lines',
               hovertemplate = paste('<b>%{fullData.name}</b><br>',
                                     x_label, ': %{x}<br>',
                                     'Value: %{y}<extra></extra>'))
} else {
  # Without color grouping
  p <- plot_ly(plot_df, x = ~x_axis, y = ~value, 
               type = 'scatter', mode = 'lines',
               hovertemplate = paste(x_label, ': %{x}<br>',
                                     'Value: %{y}<extra></extra>'))
}

# Get font sizes
title_size <- if(!is.null(input$title_font_size)) input$title_font_size else 14
xaxis_size <- if(!is.null(input$xaxis_font_size)) input$xaxis_font_size else 12
yaxis_size <- if(!is.null(input$yaxis_font_size)) input$yaxis_font_size else 12

# Add best fit line if specified
if (!is.null(input$add_smooth) && input$add_smooth == TRUE) {
  tryCatch({
    smooth_fit <- loess(value ~ x_axis, data = plot_df)
    p <- p |> add_trace(
      type = 'scatter', mode = 'lines',
      x = plot_df$x_axis, y = fitted(smooth_fit),
      line = list(color = 'rgba(255,0,0,0.5)', dash = 'dot'),
      name = 'Trend Line', showlegend = TRUE
    )
  }, error = function(e) {
    lm_fit <- lm(value ~ x_axis, data = plot_df)
    p <- p |> add_trace(
      type = 'scatter', mode = 'lines',
      x = plot_df$x_axis, y = fitted(lm_fit),
      line = list(color = 'rgba(255,0,0,0.5)', dash = 'dot'),
      name = 'Linear Trend', showlegend = TRUE
    )
  })
}

# Add reference lines
shapes_list <- list()
if (!is.null(input$hline_1) && !is.na(input$hline_1)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = 0, x1 = 1, xref = "paper", y0 = input$hline_1, y1 = input$hline_1, line = list(color = 'red', dash = 'dash'))
}
if (!is.null(input$hline_2) && !is.na(input$hline_2)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = 0, x1 = 1, xref = "paper", y0 = input$hline_2, y1 = input$hline_2, line = list(color = 'blue', dash = 'dash'))
}
if (!is.null(input$hline_3) && !is.na(input$hline_3)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = 0, x1 = 1, xref = "paper", y0 = input$hline_3, y1 = input$hline_3, line = list(color = 'purple', dash = 'dash'))
}
if (!is.null(input$hline_4) && !is.na(input$hline_4)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = 0, x1 = 1, xref = "paper", y0 = input$hline_4, y1 = input$hline_4, line = list(color = 'orange', dash = 'dash'))
}
if (!is.null(input$vline_1) && !is.na(input$vline_1)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = input$vline_1, x1 = input$vline_1, y0 = 0, y1 = 1, yref = "paper", line = list(color = 'red', dash = 'dash'))
}
if (!is.null(input$vline_2) && !is.na(input$vline_2)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = input$vline_2, x1 = input$vline_2, y0 = 0, y1 = 1, yref = "paper", line = list(color = 'blue', dash = 'dash'))
}
if (!is.null(input$vline_3) && !is.na(input$vline_3)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = input$vline_3, x1 = input$vline_3, y0 = 0, y1 = 1, yref = "paper", line = list(color = 'purple', dash = 'dash'))
}
if (!is.null(input$vline_4) && !is.na(input$vline_4)) {
  shapes_list[[length(shapes_list) + 1]] <- list(type = 'line', x0 = input$vline_4, x1 = input$vline_4, y0 = 0, y1 = 1, yref = "paper", line = list(color = 'orange', dash = 'dash'))
}

# Add layout and labels
p <- p |> layout(
  title = list(text = if(!is.null(input$plot_title) && nzchar(input$plot_title)) input$plot_title else 'Interactive Plot', font = list(size = title_size)),
  xaxis = list(title = list(text = x_label, font = list(size = xaxis_size)), tickfont = list(size = xaxis_size * 0.9)),
  yaxis = list(title = list(text = if(!is.null(input$plot_ylabel)) input$plot_ylabel else 'Y Axis', font = list(size = yaxis_size)), tickfont = list(size = yaxis_size * 0.9)),
  hovermode = 'x unified',
  legend = list(orientation = 'h', y = -0.2),
  shapes = shapes_list,
  annotations = list(
    list(
      x = 1, y = 0, xref = 'paper', yref = 'paper',
      text = if(!is.null(input$plot_caption) && nzchar(input$plot_caption)) input$plot_caption else '',
      showarrow = FALSE, xanchor = 'right', yanchor = 'bottom',
      font = list(size = 10, color = 'grey')
    )
  )
)

# JavaScript for dynamic subtitles and table
js_code <- r"(
function(el, x) {
  if (typeof el._originalDataNames === 'undefined') {
    el._originalDataNames = el.data.map(trace => trace.name);
  }
  const tableContainer = document.getElementById('dynamic-stats-table-container');
  function updateDynamicElements(eventdata) {
    let new_names = [];
    let full_view = (eventdata && eventdata['xaxis.autorange']) || !eventdata;
    let tableHtml = '<table id=\"dynamic-stats-table\"><thead><tr><th>Trace Name</th><th>Min</th><th>Max</th></tr></thead><tbody>';
    for (let i = 0; i < el.data.length; i++) {
      let trace = el.data[i];
      let original_name = el._originalDataNames[i];
      if (!original_name || trace.mode !== 'lines') {
        new_names.push(original_name);
        continue;
      }
      let x_range;
      if(full_view){
        x_range = [Math.min(...trace.x), Math.max(...trace.x)];
      } else {
        x_range = [eventdata['xaxis.range[0]'], eventdata['xaxis.range[1]']];
      }
      let visible_y = [];
      for (let j = 0; j < trace.x.length; j++) {
        if (trace.x[j] >= x_range[0] && trace.x[j] <= x_range[1]) {
          visible_y.push(trace.y[j]);
        }
      }
      if (visible_y.length > 0) {
        let min_val = Math.min(...visible_y).toFixed(2);
        let max_val = Math.max(...visible_y).toFixed(2);
        new_names.push(original_name + ' (Min: ' + min_val + ', Max: ' + max_val + ')');
        tableHtml += '<tr><td>' + original_name + '</td><td>' + min_val + '</td><td>' + max_val + '</td></tr>';
      } else {
        new_names.push(original_name + ' (No data in view)');
        tableHtml += '<tr><td>' + original_name + '</td><td colspan=\"2\">No data in view</td></tr>';
      }
    }
    tableHtml += '</tbody></table>';
    Plotly.restyle(el, {name: new_names});
    if(tableContainer) {
       tableContainer.innerHTML = '<h4>Visible Data Summary</h4>' + tableHtml;
    }
  }
  updateDynamicElements(null);
  el.on('plotly_relayout', updateDynamicElements);
}
)"

# Attach the JavaScript to the plot object
p <- p |> htmlwidgets::onRender(js_code)

# Create a placeholder for the table and add styling
stats_table_div <- div(
  id = "dynamic-stats-table-container",
  h4("Visible Data Summary"),
  tags$style(HTML("
    #dynamic-stats-table { border-collapse: collapse; width: 50%; margin-top: 10px; font-family: sans-serif; font-size: 14px; }
    #dynamic-stats-table th, #dynamic-stats-table td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    #dynamic-stats-table th { background-color: #f2f2f2; }
  "))
)

# Combine the plot and the table placeholder into a single output
final_output <- navset_card_pill(
nav_panel(title = "plot", p),
nav_panel(title = "table", stats_table_div),
)

# Return the final combined object
final_output
)---"

# NOTE: The complete interactive template includes:
# - Font size controls
# - Best fit lines
# - Reference lines (H-lines and V-lines)
# - Dynamic JavaScript for subtitle updates
# - Statistics table generation
# - Tab-based layout with plot and table views
# For the complete implementation, please refer to the original r_code_.R file