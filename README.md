# Plotter App v1.1

A powerful, modular Shiny application for data import, processing, and visualization with dynamic plotting capabilities.

## 🌟 Features

### Core Capabilities
- **Multi-File Data Import**: Support for CSV, Excel, FST, Parquet, and compressed formats
- **Dynamic Plotter Creation**: Add unlimited plotter tabs on-demand
- **Advanced Data Processing**: Custom R code execution at multiple pipeline stages
- **Interactive Visualizations**: Both static (ggplot2) and dynamic (plotly) plotting
- **Template System**: Save and load complete application configurations
- **Batch Export**: Download all plots in a single ZIP file

### Supported File Formats
- `.csv` and `.csv.gz` (compressed CSV)
- `.xlsx` (Excel files)
- `.fst` (Fast Storage format)
- `.parquet` (Apache Parquet)
- Additional formats can be added via custom R code

### Data Processing Pipeline
1. **Pre-processing**: Custom R code for file reading and initial processing
2. **Post-processing**: Data transformation, melting, filtering, and renaming
3. **Combination**: Automatic merging of data from multiple importers
4. **Plot-specific Processing**: Per-plot data filtering and sampling
5. **Visualization**: Flexible plotting with customizable aesthetics

## 🚀 Getting Started

### Prerequisites
- R (≥ 4.0.0)
- RStudio (recommended)

### Required R Packages
```r
# Core packages
install.packages(c(
  "shiny", "shinyTime", "bslib", "shinyAce", 
  "stringr", "data.table", "DT"
))

# Visualization packages  
install.packages(c(
  "ggplot2", "plotly", "scattermore", "htmlwidgets"
))

# Data handling packages
install.packages(c(
  "lubridate", "jsonlite", "fasttime", "readxl", 
  "fst", "nanoparquet", "tools"
))

# Additional packages
install.packages(c(
  "purrr", "future", "skimr", "spsComps", "base64enc"
))
```

### Installation

#### Option 1: Package-based Development (Recommended)
1. Clone or download this repository
2. Open R/RStudio and set working directory to the app folder
3. Install required packages (see above)
4. Install package development tools:
```r
install.packages(c("devtools", "pkgload"))
```
5. Load and run the application:
```r
# For development
devtools::load_all(".")
run_data_plotter()

# Or using the package-based app.R
shiny::runApp("app_package.R")
```

#### Option 2: Traditional Single-file App
```r
# Run the original app structure
shiny::runApp("app.R")
```

## 📊 Usage Guide

### 1. Data Import
- Use the **Data Import 1-3** tabs to upload files
- Configure file reading options (skip rows, sampling)
- Apply pre-processing R code for custom file handling
- Transform data with post-processing code (melting, filtering, etc.)

### 2. Data Combination
- View combined data from all active importers in the **Combined Data** tab
- Inspect data summaries and samples
- All data is automatically merged with source tracking

### 3. Creating Plots
- Click **"Add New Plotter Tab"** to create a new plotter
- Process data specifically for each plot (sampling, filtering)
- Customize plot aesthetics (colors, line types, faceting)
- Choose between dynamic (plotly) or static (ggplot2) output

### 4. Advanced Features
- **R Code Helper**: Execute custom R code with full environment access
- **Batch Downloader**: Download all active plots as a ZIP file
- **Templates**: Save current configuration and reload later
- **Custom Processing**: Modify R code at any pipeline stage

## 🏗️ Architecture

### Package Structure
This application follows R package development best practices:
- **Organized Code**: All R code in `R/` directory with proper modularization
- **Dependency Management**: All packages declared in `DESCRIPTION` file
- **Development Workflow**: Use `devtools::load_all()` for fast iteration
- **Deployment Ready**: Package-based deployment with `pkgload`

### Module Structure
- **`server_data_import`**: Handles file upload and processing for each importer
- **`server_data_combiner`**: Merges data from multiple importers
- **`server_plotter`**: Manages individual plot creation and rendering
- **`server_data_table_display`**: Provides interactive data table views

### Key Files
- **`R/ui.R`**: User interface definition (extracted from app.R)
- **`R/server.R`**: Server logic (extracted from app.R)
- **`R/run_app.R`**: Main application wrapper function
- **`R/functions.R`**: Core helper functions and utilities
- **`R/module_*.R`**: Shiny modules for different components
- **`inst/code_template/`**: R code templates for data processing
- **`DESCRIPTION`**: Package metadata and dependencies
- **`app_package.R`**: Package-based app launcher for deployment

### Data Flow
```
Files → Import Modules → Pre-processing → Post-processing → 
Combined Data → Plot-specific Processing → Visualization
```

## 🎨 Customization

### Custom R Code Templates
The application uses editable R code templates for different processing stages:

1. **Pre-processing**: File reading and initial data preparation
2. **Post-processing**: Data transformation and standardization  
3. **Plot processing**: Plot-specific data filtering and sampling
4. **Visualization**: Custom plot generation with ggplot2/plotly

### Adding New File Formats
Extend the pre-processing template in `r_code_.R` to support additional formats:

```r
# Add to read_functions list
'new_format' = function(fp, sr) your_custom_reader(fp, skip = sr)
```

### Custom Plot Types
Modify the plotting template to add new visualization types:

```r
# Add custom plot logic in ggplot_template
if (input$plot_type == 'custom') {
  # Your custom visualization code
}
```

## 📁 Project Structure

```
plotter_app/
├── app.R                    # Main application
├── functions.R              # Helper functions and modules
├── r_code_.R               # R code templates
├── www/                    # Web assets
│   └── r_functions_autocomplete.json
├── docs/                   # Documentation
├── README.md               # This file
└── Ubuntu/                 # Backup/alternative files
```

## 🔧 Configuration

### Performance Tuning
- Adjust `shiny.maxRequestSize` for large file uploads
- Configure `future::plan()` for parallel processing
- Modify debounce timers for responsive code execution

### UI Customization
- Change Bootstrap theme in `bs_theme()`
- Modify sidebar widths and layout configurations
- Customize color schemes and visual elements

## 🐛 Troubleshooting

### Common Issues
1. **File Upload Failures**: Check file size limits and format support
2. **Memory Issues**: Reduce data size or increase system memory
3. **Plot Rendering Errors**: Verify data structure and R code syntax
4. **Package Conflicts**: Ensure all required packages are installed

### Debug Mode
Enable verbose error reporting by modifying `spsComps::shinyCatch()` calls.

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## 📄 License

This project is open source. Please check with the original authors for licensing terms.

## 📞 Support

For questions, issues, or feature requests:
- Check the documentation in the `docs/` folder
- Review the built-in R code templates
- Examine the helper functions in `functions.R`

## 🎯 Version History

### v1.1 (Current)
- Modular architecture with reusable components
- Dynamic plotter creation
- Advanced template system
- Batch export functionality
- Improved error handling and user feedback

---

*Built with ❤️ using R Shiny, ggplot2, and plotly* 