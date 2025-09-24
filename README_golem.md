# Data Plotter - Golem Package

A powerful, modular Shiny application for data import, processing, and visualization with dynamic plotting capabilities.

## Installation

```r
# Install from GitHub (when available)
# devtools::install_github("yourusername/dataPlotter")

# Or install in development mode
devtools::install()
```

## Usage

### Basic Usage

```r
library(dataPlotter)

# Run the application
run_app()
```

### Development Mode

For development, use the provided development script:

```r
# From the project root
source("dev/run_dev.R")
```

## Package Structure

This package follows the golem framework structure:

```
R/
├── run_app.R              # Main application runner
├── app_ui.R               # UI components
├── app_server.R           # Server components
├── app_sys.R              # System utilities
├── srv_global.R           # Global server logic
├── ui_global.R            # Global UI logic
├── utils_*.R              # Utility functions
└── app_*.R                # Module components

inst/
└── app/
    ├── www/               # Static assets
    └── code_template/     # Code templates

dev/
└── run_dev.R              # Development runner
```

## Key Features

- **Modular Design**: Separate modules for data import, processing, and plotting
- **Multiple File Formats**: Support for CSV, Excel, FST, Parquet, and compressed files
- **Dynamic Code Evaluation**: Safe R code execution within strings
- **Interactive Visualizations**: Both static and interactive plots
- **Template System**: Save and load configurations as JSON templates
- **Batch Processing**: Generate multiple plots and outputs in batches

## Functions

### Main Functions

- `run_app()`: Launch the Shiny application
- `ui_global()`: Main UI component
- `server_global()`: Main server component

### Utility Functions

- `load_templates_ordered()`: Load code templates in correct order
- `string_eval()`: Evaluate R code within text strings
- `filter_in()`, `filter_out()`: Data filtering functions
- `extract_from_filename()`: Extract metadata from filenames

### UI Helpers

- `text_input_tip()`: Text input with tooltips
- `numeric_input_tip()`: Numeric input with tooltips
- `aceEditor_pre()`: Enhanced ACE code editor

## Configuration

The application can be configured using the following options:

```r
options(
  golem.app.prod = FALSE,  # Development vs production mode
  shiny.maxRequestSize = 1000 * 1024^2  # Max upload size (1GB)
)
```

## Dependencies

Key dependencies include:

- **shiny**: Web application framework
- **bslib**: Bootstrap components
- **data.table**: Fast data manipulation
- **ggplot2**: Static plotting
- **plotly**: Interactive plotting
- **DT**: Data tables
- **readxl**: Excel file reading
- **jsonlite**: JSON handling

## Development

### Adding New Modules

1. Create UI components in `R/ui_*.R`
2. Create server components in `R/srv_*.R`
3. Add module logic to `R/app_*.R`
4. Update the main UI and server functions

### Adding Templates

1. Add R files to `inst/app/code_template/`
2. Update the template loading order in `load_templates_ordered()`

### Documentation

Use roxygen2 comments for function documentation:

```r
#' Function description
#'
#' @param param_name description
#' @export
function_name <- function(param_name) {
  # function body
}
```

## Deployment

### Package Installation

```r
# Install the package
devtools::install()

# Run the application
dataPlotter::run_app()
```

### Docker Deployment

The application can be deployed using the provided Docker configuration or by installing the package in a Docker container.

## License

MIT License - see LICENSE file for details.