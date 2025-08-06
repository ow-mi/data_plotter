# Interactive Plot Template (Plotly)

# This is a condensed version of the interactive template
ggplot_interactive_template <- r"---(

# Interactive plot code (plotly)
# Available: df (processed data), input (UI inputs)  
# Return: htmltools tagList object

# Ensure required libraries are available
library(plotly)

static_plot <- eval(parse(text = static_code))


p <- ggplotly(static_plot, tooltip = c("x", "y", "colour"))



# Add reference lines
shapes_list <- list()
add_hline <- function(val, color) {
  list(type = 'line', x0 = 0, x1 = 1, xref = "paper", y0 = val, y1 = val,
       line = list(color = color, dash = 'dash'))
}
add_vline <- function(val, color) {
  list(type = 'line', x0 = val, x1 = val, y0 = 0, y1 = 1, yref = "paper",
       line = list(color = color, dash = 'dash'))
}

colors <- c("red", "blue", "purple", "orange")
for (i in 1:4) {
  h_val <- input[[paste0("hline_", i)]]
  if (!is.null(h_val) && !is.na(h_val)) shapes_list[[length(shapes_list) + 1]] <- add_hline(h_val, colors[i])
  
  v_val <- input[[paste0("vline_", i)]]
  if (!is.null(v_val) && !is.na(v_val)) shapes_list[[length(shapes_list) + 1]] <- add_vline(v_val, colors[i])
}

# Add layout
p <- p |> layout(
  hovermode = "x unified",
  legend = list(orientation = 'h', y = -0.2),
  shapes = shapes_list,
  annotations = list(
    list(
      x = 1, y = 0, xref = 'paper', yref = 'paper',
      text = input$plot_caption %||% '',
      showarrow = FALSE, xanchor = 'right', yanchor = 'bottom',
      font = list(size = 10, color = 'grey')
    )
  )
)

# Attach dynamic table JavaScript
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
        new_names.push(original_name);
        tableHtml += '<tr><td>' + original_name + '</td><td>' + min_val + '</td><td>' + max_val + '</td></tr>';
      } else {
        new_names.push(original_name);
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

p <- p |> htmlwidgets::onRender(js_code)

# Table placeholder
stats_table_div <- div(
  id = "dynamic-stats-table-container",
  h4("Visible Data Summary"),
  tags$style(HTML("
    #dynamic-stats-table { border-collapse: collapse; width: 50%; margin-top: 10px; font-family: sans-serif; font-size: 14px; }
    #dynamic-stats-table th, #dynamic-stats-table td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    #dynamic-stats-table th { background-color: #f2f2f2; }
  "))
)

# Combine
final_output <- navset_card_pill(
  nav_panel(title = "plot", p),
  nav_panel(title = "table", stats_table_div)
)
)---"

# NOTE: The complete interactive template includes:
# - Font size controls
# - Best fit lines
# - Reference lines (H-lines and V-lines)
# - Dynamic JavaScript for subtitle updates
# - Statistics table generation
# - Tab-based layout with plot and table views
# For the complete implementation, please refer to the original r_code_.R file