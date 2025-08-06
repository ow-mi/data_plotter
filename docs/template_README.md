# Code Templates Directory

This directory contains the modularized R code templates that were previously in the large `r_code_.R` file. The templates are now organized into smaller, manageable files for better maintainability.

## Directory Structure

### Core Data Processing Templates
- `data_import_preprocessing.R` - Template for reading and initially processing uploaded files
- `data_import_postprocessing.R` - Template for transforming data after initial processing  
- `plot_data_processing.R` - Template for processing combined data for specific plots

### Plot Output Templates
- `text_output.R` - Template for text-based output
- `table_output.R` - Template for data table output
- `interactive_plot.R` - Template for interactive plotly visualizations
- `static_plot.R` - Combined template for static ggplot2 visualizations

### Static Plot Modules
The `static_plot_modules/` subdirectory contains modular components for building static plots:
- `data_processing.R` - X-axis transformations and data setup
- `base_setup.R` - Base plot creation with aesthetics and geometry
- `themes_styling.R` - Themes, colors, fonts, and legend settings
- `statistical_overlays.R` - Trend lines, reference lines, and statistical overlays
- `grid_axes.R` - Grid controls and axis transformations
- `faceting_final.R` - Faceting and final plot assembly

### Data Display Templates
- `data_table_display.R` - Template for basic data table display
- `combined_data_summary.R` - Template for combined data summary tables
- `combined_data_sample.R` - Template for combined data sample display

### Utility Templates
- `helper_code.R` - Template for R code helper functionality
- `downloader_code.R` - Template for batch plot downloading
- `final_conditional.R` - Final conditional logic for output type selection

### System Files
- `load_templates.R` - Main loader that sources all template files
- `combined_template.R` - Backward compatibility wrapper that combines templates
- `README.md` - This documentation file

## Usage

The templates are automatically loaded when the Shiny app starts via:
```r
source("./code_template/load_templates.R")
```

This replaces the previous single file approach:
```r
source("./r_code_.R")  # Old approach - now in rubbish/
```

## Benefits of Modularization

1. **Maintainability** - Easier to edit specific functionality without affecting other templates
2. **Organization** - Related code is grouped together logically
3. **Collaboration** - Multiple developers can work on different templates simultaneously
4. **Testing** - Individual templates can be tested in isolation
5. **Reusability** - Specific modules can be reused across different parts of the application

## Backward Compatibility

All existing functionality is preserved. The `combined_template.R` file reconstructs the original `ggplot_template` variable for any code that depends on it.