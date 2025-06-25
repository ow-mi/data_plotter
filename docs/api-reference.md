# API Reference

## Overview

This document provides detailed reference information for all functions, modules, and customizable code templates in the Plotter App.

## Core Modules

### Data Import Module

#### `server_data_import(id)`

**Purpose**: Creates a data import module instance

**Parameters**:
- `id` (character): Unique module identifier

**Returns**: 
- List with `df` reactive containing processed data

**Key Reactives**:
- `df_list_uploaded()`: File information from uploads
- `df_list_filtered()`: Files after applying filters
- `df_preview_pre_processed()`: Single file preview data
- `combined_data_for_this_importer()`: Final processed dataset

**Key Observers**:
- File upload handling
- Preview processing
- Batch processing trigger

### Data Combiner Module

#### `server_data_combiner(id, list_of_importer_df_reactives)`

**Purpose**: Combines data from multiple import modules

**Parameters**:
- `id` (character): Module identifier
- `list_of_importer_df_reactives` (list): List of reactive data functions

**Returns**:
- List with `df` reactive containing combined dataset

**Key Functions**:
- Validates data from each importer
- Adds source tracking via `idcol`
- Handles different column structures

### Plotter Module

#### `server_plotter(id, combined_data_reactive)`

**Purpose**: Creates individual plot instances

**Parameters**:
- `id` (character): Unique plotter identifier
- `combined_data_reactive` (reactive): Combined dataset reactive

**Returns**:
- List with `plot` reactive containing plot object

**Key Reactives**:
- `df_plot_data_processed()`: Plot-specific processed data
- `plot_object_reactive()`: Generated plot object

**Download Handler**: Supports HTML, PNG, and text formats

### Data Table Display Module

#### `server_data_table_display(id, input_data_reactive)`

**Purpose**: Displays interactive data tables with custom formatting

**Parameters**:
- `id` (character): Module identifier
- `input_data_reactive` (reactive): Data to display

**Features**:
- Custom R code execution for table formatting
- DataTables integration
- Debounced code evaluation

## Helper Functions

### Data Manipulation

#### `filter_in(df, col_to_effect, input_text)`

**Purpose**: Filter data to include rows matching patterns

**Parameters**:
- `df` (data.table): Input data
- `col_to_effect` (character): Column name to filter
- `input_text` (character): Comma-separated patterns

**Returns**: Filtered data.table

**Example**:
```r
# Include rows where series contains "temp" or "pressure"
filtered_data <- filter_in(df, "series", "temp,pressure")
```

#### `filter_out(df, col_to_effect, input_text)`

**Purpose**: Filter data to exclude rows matching patterns

**Parameters**: Same as `filter_in`

**Returns**: Filtered data.table

#### `rname(df, col_to_effect, input_text)`

**Purpose**: Rename values in a column using pattern replacement

**Parameters**:
- `df` (data.table): Input data
- `col_to_effect` (character): Column name to modify
- `input_text` (character): Comma-separated old,new pairs

**Example**:
```r
# Rename "Temperature_1" to "Temp_Sensor_1"
renamed_data <- rname(df, "series", "Temperature_1,Temp_Sensor_1")
```

### UI Helpers

#### `text_input_tip(inputId, label, value, tip)`

**Purpose**: Creates text input with tooltip

**Parameters**:
- `inputId` (character): Input ID
- `label` (character): Input label
- `value` (character): Default value
- `tip` (character): Tooltip text

#### `numeric_input_tip(inputId, label, value, min, max, step, tip)`

**Purpose**: Creates numeric input with tooltip

#### `action_input_tip(inputId, label, tip, ...)`

**Purpose**: Creates action button with tooltip

### Ace Editor Functions

#### `aceEditor_pre(inputId, value, mode, theme, minLines, maxLines, fontSize)`

**Purpose**: Creates pre-configured Ace Editor

**Parameters**:
- `inputId` (character): Editor ID
- `value` (character): Initial code content
- `mode` (character): Language mode (default: "r")
- `theme` (character): Editor theme (default: "gruvbox")
- `minLines` (numeric): Minimum lines (default: 6)
- `maxLines` (numeric): Maximum lines (default: 40)
- `fontSize` (numeric): Font size (default: 13)

#### `ace_server_functions(ace_input_name)`

**Purpose**: Enables advanced Ace Editor features

## R Code Templates

### Pre-processing Template (`r_code_data_pre_process`)

**Purpose**: File reading and initial processing

**Available Variables**:
- `file_path`: Full path to uploaded file
- `file_name`: Original filename
- `n_every`: Row sampling parameter
- `skip_rows`: Rows to skip at start
- `showNotification()`: Notification function

**Expected Output**: data.table or data.frame

**Default Behavior**:
- Detects file format automatically
- Handles compressed files
- Applies row sampling
- Adds file source tracking

**Customization Examples**:
```r
# Custom CSV delimiter
df <- fread(file_path, sep = ";", skip = skip_rows)

# Excel with specific sheet
df <- as.data.table(read_excel(file_path, sheet = "Data", skip = skip_rows))

# Custom column naming
setnames(df, old = c("V1", "V2", "V3"), new = c("time", "temp", "pressure"))
```

### Post-processing Template (`r_code_data_processing`)

**Purpose**: Data transformation and standardization

**Available Variables**:
- `df`: Pre-processed data.table
- `input_filter_in_1`: Filter IN setting
- `input_filter_out_1`: Filter OUT setting
- `input_rename_1`: Rename setting
- `input_date_format`: Date format specification
- Helper functions: `filter_in()`, `filter_out()`, `rname()`

**Expected Output**: Long-format data.table with columns:
- `timestamp`: Parsed datetime
- `series`: Variable names
- `value`: Numeric values
- `file_name_source`: Source tracking

**Default Behavior**:
- Renames first column to "timestamp"
- Converts measure columns to numeric
- Melts to long format
- Parses timestamps
- Applies filters and renames
- Extracts DUT from filename

### Plot Processing Template (`r_code_plot_process_template`)

**Purpose**: Plot-specific data preparation

**Available Variables**:
- `df`: Combined dataset
- `input`: Plot-specific UI inputs
- Helper functions for filtering and renaming

**Expected Output**: Plot-ready data.table

**Key Features**:
- Data sampling
- Series filtering
- Time range filtering
- Plot-specific transformations

### Plotting Template (`ggplot_template`)

**Purpose**: Plot generation with flexible aesthetics

**Available Variables**:
- `df`: Plot-ready dataset
- `input`: Plot configuration inputs
- All ggplot2 and plotly functions

**Expected Output**: 
- ggplot object (for static plots)
- plotly object (for dynamic plots)
- Any R object (for text output)

**Key Features**:
- Smart axis transformations
- Aesthetic mapping based on UI
- Faceting support
- Multiple output formats

## Data Table Templates

### `DT_head`

**Purpose**: Standard data table display

**Features**:
- Shows first 100 rows
- Scrollable X and Y
- Compact styling
- No row names

### `r_code_combined_data_summary`

**Purpose**: Summary statistics by series

**Expected Columns**: `value`, `series`

**Output**: DataTable with statistics (min, mean, median, max, SD, count)

### `r_code_combined_data_sample`

**Purpose**: Random data sample display

**Features**:
- Random sample of up to 100 rows
- Full interactive DataTable

## Helper Code Templates

### `helper_code_template`

**Purpose**: General R code execution

**Available Environment**:
- Main application input/output objects
- All loaded packages
- Current working directory

**Use Cases**:
- Data exploration
- Custom analysis
- Debugging
- Package management

### `downloader_code_refactored`

**Purpose**: Batch plot download functionality

**Available Variables**:
- `dynamic_plots_map`: Named list of plot reactives
- Shiny functions: `downloadHandler`, `showNotification`
- Main app objects (use with caution)

**Features**:
- Supports multiple plot formats
- Creates organized ZIP files
- Progress tracking
- Error handling

## Configuration Options

### File Upload Settings

```r
# Maximum file size (default: 1GB)
options(shiny.maxRequestSize = 1000 * 1024^2)

# Future processing settings
options(future.globals.maxSize = 2 * 1024^3)
```

### UI Theme Configuration

```r
# Bootstrap theme
bs_theme(bootswatch = "minty", version = 5)

# Dark mode toggle
input_dark_mode(mode = "light")
```

### Performance Tuning

```r
# Debounce timers (milliseconds)
debounce(reactive(...), 500)  # Ace editor updates
debounce(reactive(...), 1000) # Table display updates

# Parallel processing
plan(multisession)  # Enable parallel file processing
```

## Error Handling

### `spsComps::shinyCatch()`

**Purpose**: Safe execution of user code

**Features**:
- Catches and displays errors gracefully
- Prevents app crashes
- User-friendly error messages

**Usage**:
```r
spsComps::shinyCatch({
  # User code execution
  result <- eval(parse(text = user_code), envir = safe_env)
})
```

### Environment Isolation

**Pattern**: Create safe execution environments

```r
env <- new.env(parent = .GlobalEnv)
env$df <- data
env$helper_function <- helper_function
result <- eval(parse(text = code), envir = env)
```

## Reactive Patterns

### Module Communication

```r
# Module returns reactive
list(df = reactive(processed_data()))

# Parent consumes reactive
module_output <- server_module("id")
processed_data <- module_output$df()
```

### Dynamic UI Management

```r
# Insert new tab
nav_insert(
  id = "mainmenu",
  target = "Arranger",
  position = "before",
  nav = nav_panel(title = plot_id, ui_plotter(plot_id))
)

# Store module instance
instances[[plot_id]] <- server_plotter(plot_id, data_reactive)
```

## Extension Points

### Adding File Formats

1. Extend `read_functions` in pre-processing template
2. Add format detection logic
3. Test with sample files

### Custom Plot Types

1. Modify plotting template
2. Add UI controls if needed
3. Update plot type selector

### New Processing Stages

1. Create processing function
2. Add to appropriate template
3. Wire reactive connections

This API reference provides the foundation for understanding and extending the Plotter App's functionality. 