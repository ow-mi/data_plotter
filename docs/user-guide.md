# Plotter App User Guide

## Getting Started

The Plotter App is designed to make data visualization accessible and powerful. This guide will walk you through all features and help you make the most of the application.

## Application Layout

The main interface consists of several tabs:
- **Data Import 1-3**: Three independent data import modules
- **Combined Data**: View all imported data together
- **Arranger**: Placeholder for organizing plotters
- **Dynamic Plotter Tabs**: Created on-demand for each plot
- **R Code Helper**: Execute custom R code
- **Batch Downloader**: Download all plots at once
- **Templates**: Save and load configurations

## Data Import Workflow

### Step 1: Upload Files

1. Navigate to any **Data Import** tab (1, 2, or 3)
2. Click the **Upload Files** button or drag and drop files
3. Supported formats:
   - `.csv` - Comma-separated values
   - `.csv.gz` - Compressed CSV files
   - `.xlsx` - Excel spreadsheets
   - `.fst` - Fast Storage format
   - `.parquet` - Apache Parquet format

**File Size Limits**: Default is 1GB per file (configurable)

### Step 2: Configure Import Settings

In the **Input File List** panel:
- **Comment**: Add optional description for this import batch
- **Filter IN**: Include only files matching patterns (regex/text, comma-separated)
- **Filter OUT**: Exclude files matching patterns
- **Read every Nth row**: Downsample large files (0 = all rows)

**Example Filters**:
- Filter IN: `experiment1,test_data` (includes files containing these terms)
- Filter OUT: `backup,temp` (excludes files containing these terms)

### Step 3: File Pre-processing

Switch to the **File Pre-processing & Preview** panel:

1. **Select a file** from the dropdown to preview
2. **Skip rows**: Skip header rows at file start
3. **Edit R Code**: Customize file reading logic

**Default Pre-processing Code**:
- Automatically detects file format
- Handles compressed files
- Adds file source tracking
- Applies row sampling if specified

**Customization Examples**:
```r
# Custom CSV reading
df <- fread(file_path, sep = ";", dec = ",")  # European format

# Custom Excel sheet
df <- as.data.table(read_excel(file_path, sheet = 2))

# Custom column selection
df <- df[, .(timestamp = V1, temperature = V3, pressure = V5)]
```

### Step 4: Data Transformation

Move to the **Data Transformation & Combination** panel:

1. **Filter Series**: Apply filters after data is melted to long format
2. **Rename Series**: Transform series names using pattern matching
3. **Date Format**: Specify timestamp parsing format
4. **Edit Post-processing Code**: Customize data transformation

**Common Transformations**:
```r
# Custom timestamp parsing
df_long[, timestamp := parse_date_time(timestamp, "dmy HMS")]

# Unit conversions
df_long[series %like% "temp", value := value * 9/5 + 32]  # C to F

# Derived variables
df_long[, value_normalized := (value - mean(value)) / sd(value), by = series]
```

### Step 5: Process and Combine

Click **"Process & Combine All Files in This Importer"** to:
- Apply all settings to uploaded files
- Execute custom R code on each file
- Combine results into a single dataset
- Add source tracking information

## Data Combination

### Viewing Combined Data

The **Combined Data** tab shows:
- **Summary**: Statistics by series (min, max, mean, count)
- **Sample**: Random sample of combined data (up to 100 rows)

This data automatically updates when any importer processes new files.

### Data Structure

The combined dataset has these standard columns:
- `timestamp`: Parsed datetime values
- `series`: Variable/measurement names
- `value`: Numeric measurement values
- `file_name_source`: Original file name
- `source_importer_id`: Which importer module processed the data
- `dut`: Device under test identifier (extracted from filename)

## Creating Plots

### Step 1: Add a Plotter

Click **"Add New Plotter Tab"** in the top menu to create a new plotting interface.

### Step 2: Process Data for Plot

In the **Plot-Specific Data Processing** section:

1. **Configure filters**:
   - **Sample Size**: Fraction of data to use (0 = all, 0.1 = 10%)
   - **Filter Series IN**: Include only specific series
   - **Filter Series OUT**: Exclude specific series
   - **Rename Series**: Transform series names for this plot
   - **Time Range**: Filter by date/time range

2. **Edit Processing Code**: Customize plot-specific data transformations

3. **Click "Process Data for Plot"**: Apply settings and generate plot dataset

**Example Plot Processing**:
```r
# Focus on temperature data only
df <- df[series %like% "temp"]

# Apply time-based sampling
df <- df[hour(timestamp) %in% 9:17]  # Business hours only

# Smooth noisy data
df[, value_smoothed := frollmean(value, 10), by = series]
```

### Step 3: Configure Plot Aesthetics

In the **Plot Configuration & Rendering** section:

#### Labels & Titles
- **Plot Title**: Main plot title
- **X-axis Label**: Choose from presets or create custom
  - Timestamp, Duration (Days/Hours/Minutes/Seconds)
- **Y-axis Label**: Units and description

#### Aesthetics & Type
- **Split by Color**: Group data by series, dut, file_name, or none
- **Split by Linetype**: Additional grouping dimension
- **Use Scattermore**: Enable for large datasets (>10k points)

#### Facetting
- **Substring Start/End**: Extract part of series name for faceting
- **Number of Rows**: Layout control for faceted plots

### Step 4: Generate Plot

1. **Edit Plot Code**: Customize visualization logic
2. **Select Output Type**:
   - **Dynamic**: Interactive plotly visualization
   - **Static**: High-quality ggplot2 output
   - **Text**: Data summary or custom text output
3. **Click "Render Plot"**: Generate and display the plot

### Step 5: Download Plot

Click **"Download Plot Output"** to save:
- HTML files for interactive plots
- PNG files for static plots
- Text files for text output

## Advanced Features

### Custom R Code Execution

#### R Code Helper Tab
Execute arbitrary R code with access to the full application environment:

```r
# Explore data structure
str(combined_data)

# Custom analysis
summary_stats <- combined_data[, .(
  mean_val = mean(value, na.rm = TRUE),
  sd_val = sd(value, na.rm = TRUE)
), by = .(series, dut)]

# Display results
datatable(summary_stats)
```

#### Custom Data Processing
Each processing stage allows custom R code:

**Pre-processing**: File reading and initial cleaning
**Post-processing**: Data transformation and standardization
**Plot processing**: Plot-specific data preparation
**Visualization**: Custom plot generation

### Template System

#### Saving Templates
1. Configure application settings (imports, plots, code)
2. Go to **Templates** menu
3. Enter template filename
4. Click **"Download Current Settings as Template"**

#### Loading Templates
1. Go to **Templates** menu
2. Click **"Upload Template"**
3. Select saved CSV template file
4. All settings will be restored automatically

**Template Contents**:
- All input values (text, numeric, checkbox)
- Ace Editor R code content
- File selections and configurations

### Batch Export

#### Download All Plots
1. Create multiple plotter tabs with different configurations
2. Navigate to **Batch Downloader** tab
3. Customize download code if needed
4. Click **"Download All Active Plots as ZIP"**

The ZIP file contains:
- Individual plot files (HTML/PNG/TXT)
- Dependency folders for HTML plots
- Organized by plotter tab names

## Tips and Best Practices

### Data Import
1. **Start Small**: Test with a few files before batch processing
2. **Use Consistent Naming**: Helps with automatic pattern detection
3. **Check Previews**: Always preview before combining large datasets
4. **Document Changes**: Use comments to track custom modifications

### Data Processing
1. **Validate Timestamps**: Ensure correct parsing before analysis
2. **Handle Missing Data**: Use `na.rm = TRUE` in calculations
3. **Memory Management**: Sample large datasets for interactive exploration
4. **Backup Code**: Save custom R code separately before major changes

### Plotting
1. **Choose Appropriate Types**: Dynamic for exploration, static for publication
2. **Use Faceting**: Break complex data into manageable panels
3. **Consider Performance**: Use scattermore for large point clouds
4. **Test Different Aesthetics**: Color and linetype can reveal patterns

### Performance Optimization
1. **Reduce Data Size**: Filter unnecessary series early in pipeline
2. **Batch Operations**: Process similar files together
3. **Monitor Memory**: Watch for memory usage with large datasets
4. **Use Parallel Processing**: Let the app use multiple cores for file processing

## Troubleshooting

### Common Issues

#### File Upload Problems
- **Error**: "File too large"
  - **Solution**: Check file size limits in configuration
- **Error**: "Unsupported file type"
  - **Solution**: Convert to supported format or modify pre-processing code

#### Data Processing Errors
- **Error**: "Column not found"
  - **Solution**: Check column names in preview, adjust R code
- **Error**: "Cannot parse timestamp"
  - **Solution**: Verify date format specification matches data

#### Plot Rendering Issues
- **Error**: "No data to plot"
  - **Solution**: Check filtering settings, ensure data remains after processing
- **Error**: "Aesthetic mapping error"
  - **Solution**: Verify column names exist in processed data

#### Memory Issues
- **Slow Performance**: 
  - Reduce data size with sampling
  - Close unused plotter tabs
  - Filter data more aggressively

### Getting Help

1. **Check Error Messages**: Often contain specific guidance
2. **Review R Code**: Examine templates for correct syntax
3. **Test Step by Step**: Isolate issues by testing each processing stage
4. **Use Simple Examples**: Start with small, known datasets

## Keyboard Shortcuts

### Ace Editor (R Code)
- `Ctrl+Space`: Auto-completion
- `Ctrl+/`: Comment/uncomment lines
- `Ctrl+F`: Find and replace
- `Ctrl+A`: Select all
- `Tab`: Indent selection

### General Navigation
- `F11`: Toggle full screen for cards
- Browser zoom controls work for entire interface

This user guide covers the core functionality of the Plotter App. The application's flexibility means there are many ways to accomplish data visualization tasks - experiment with different approaches to find what works best for your data and analysis needs. 