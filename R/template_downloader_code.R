# Downloader Code Template
# For Downloader Tab (Refactored)

downloader_code_refactored <- "# Batch Downloader - Select and Download Multiple Plots
# This creates a user-friendly interface for selecting which plots to download

library(shiny)
library(DT)

# Get available plots
available_plots <- names(dynamic_plots_map)

if (length(available_plots) == 0) {
  # No plots available
  div(class = 'alert alert-warning',
    h4('No Active Plots Found'),
    p('Create some plots first, then return here to download them.')
  )
} else {
  # Create selection interface
  div(
    h4('Select Plots to Download'),
    p(class = 'text-muted', 'Choose which plots you want to include in the ZIP file:'),
    
    # Plot selection checkboxes
    div(class = 'mb-3',
      lapply(available_plots, function(plot_id) {
        # Get a friendly name for the plot
        friendly_name <- gsub('_', ' ', plot_id)
        friendly_name <- tools::toTitleCase(friendly_name)
        
        div(class = 'form-check',
          tags$input(
            type = 'checkbox',
            class = 'form-check-input',
            id = paste0('select_plot_', plot_id),
            value = plot_id,
            checked = 'checked'  # Default to selected
          ),
          tags$label(
            class = 'form-check-label',
            `for` = paste0('select_plot_', plot_id),
            friendly_name
          )
        )
      })
    ),
    
    # Select/Deselect All buttons
    div(class = 'mb-3',
      actionButton('select_all_plots', 'Select All', class = 'btn btn-sm btn-outline-primary me-2'),
      actionButton('deselect_all_plots', 'Deselect All', class = 'btn btn-sm btn-outline-secondary')
    ),
    
         # Download options
     div(class = 'mb-3',
       h5('Download Options'),
       p(class = 'text-muted small', 'Plots will be saved in their native format: Interactive plots as HTML, Static plots as PNG, Text outputs as TXT files.'),
       div(class = 'row',
         div(class = 'col-md-6',
           numericInput('download_dpi', 'DPI for PNG files',
             value = 300, min = 72, max = 600, step = 50
           )
         ),
         div(class = 'col-md-6',
           numericInput('download_width', 'Plot Width (inches)',
             value = 10, min = 4, max = 20, step = 1
           )
         )
       ),
       div(class = 'row',
         div(class = 'col-md-6',
           numericInput('download_height', 'Plot Height (inches)',
             value = 7, min = 3, max = 15, step = 1
           )
         )
       )
     ),
    
    # Download button
    div(class = 'text-center',
      downloadButton('batch_download_zip', 
        'Download Selected Plots as ZIP',
        class = 'btn btn-success btn-lg',
        icon = icon('download')
      )
    )
  )
}
"