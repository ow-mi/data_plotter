# Examples and Use Cases

## Overview

This document provides practical examples and common use cases for the Plotter App, demonstrating how to leverage its features for real-world data analysis scenarios.

## Example Workflows

### Example 1: Temperature Sensor Analysis

**Scenario**: Analyzing temperature data from multiple sensors across different test conditions.

#### Step 1: Data Import Setup

**File Structure**:
```
temperature_data/
├── sensor_1_experiment_A.csv
├── sensor_1_experiment_B.csv
├── sensor_2_experiment_A.csv
└── sensor_2_experiment_B.csv
```

**Sample CSV Format**:
```csv
timestamp,temp_ambient,temp_surface,temp_core,humidity
2024-01-01 10:00:00,22.5,25.3,28.1,45.2
2024-01-01 10:01:00,22.7,25.8,28.4,45.1
```

#### Step 2: Import Configuration

**Data Import Tab 1 Settings**:
- Filter IN: `sensor_1`
- Comment: "Sensor 1 temperature measurements"

**Pre-processing Code**:
```r
# Standard file reading with data validation
df <- fread(file_path, skip = skip_rows)

# Validate required columns
required_cols <- c("timestamp", "temp_ambient", "temp_surface", "temp_core")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  showNotification(paste("Missing columns:", paste(missing_cols, collapse = ", ")), 
                   type = "error")
  return(NULL)
}

# Add file metadata
df[, file_name_source := file_name]
df[, sensor_id := str_extract(file_name, "sensor_\\d+")]
df[, experiment := str_extract(file_name, "experiment_[A-Z]")]

df
```

### Example 2: Multi-File Performance Analysis

**Scenario**: Analyzing system performance metrics from log files.

**Advanced Pre-processing**:
```r
# Handle complex log file format
ext <- tools::file_ext(tolower(file_name))

if (ext == "log" || ext == "txt") {
  # Read raw lines first
  raw_lines <- readLines(file_path)
  
  # Find data start (skip comments and headers)
  data_start <- which(str_detect(raw_lines, "^time,"))[1]
  if (is.na(data_start)) {
    showNotification("Could not find data header in log file", type = "error")
    return(NULL)
  }
  
  # Read from data start
  df <- fread(file_path, skip = data_start - 1)
} else {
  df <- fread(file_path, skip = skip_rows)
}

# Extract test run information from filename
df[, test_run := str_extract(file_name, "\\d{3}")]
df[, test_date := str_extract(raw_lines[2], "\\d{4}-\\d{2}-\\d{2}")]

df[, file_name_source := file_name]
df
```

## Best Practices

### 1. Data Validation
- Always validate required columns exist
- Check data types and handle conversions gracefully
- Implement meaningful error messages

### 2. Flexible Processing
- Use configuration parameters for reusable code
- Implement conditional processing based on data characteristics
- Add metadata extraction for better data organization

### 3. Visualization Strategy
- Design plots that scale with data complexity
- Use faceting for multi-dimensional comparisons
- Provide both static and interactive options

These examples demonstrate the flexibility and power of the Plotter App for various analytical workflows.