# Working with Package Files

This document explains how to properly add and access files in your R package.

## Package File Structure

Your package can include files in several standard locations:

### 1. `inst/` folder (Most Common)
Files in the `inst/` folder are automatically included in the package and can be accessed at runtime.

**Structure:**
```
dataPlotter/
├── inst/
│   ├── code_template/     # Template files
│   │   ├── helper_code.R
│   │   ├── downloader_code.R
│   │   └── ...
│   └── www/              # Web assets
│       └── r_functions_autocomplete.json
```

**Access from R:**
```r
# Get the path to a file in inst/
system.file("code_template/helper_code.R", package = "dataPlotter")

# Read the file
readLines(system.file("code_template/helper_code.R", package = "dataPlotter"))
```

### 2. `data/` folder
For R data objects (`.rda`, `.rds` files).

**Structure:**
```
dataPlotter/
├── data/
│   ├── sample_data.rda
│   └── configuration.rds
```

**Access from R:**
```r
# Load data
data("sample_data", package = "dataPlotter")
```

### 3. `extdata/` folder
For external data files (CSV, JSON, etc.).

**Structure:**
```
dataPlotter/
├── extdata/
│   ├── sample.csv
│   └── config.json
```

**Access from R:**
```r
# Get path to extdata file
system.file("extdata/sample.csv", package = "dataPlotter")
```

## Package Functions for File Access

The `dataPlotter` package provides several functions to work with package files:

### Template Loading Functions

```r
# Load all templates in correct order
dataPlotter::load_templates_ordered()

# Load specific templates only
dataPlotter::load_specific_templates(c("helper_code", "downloader_code"))

# Get template content as text
content <- dataPlotter::get_template_content("helper_code")

# List all available templates
templates <- dataPlotter::list_templates()
```

### General File Access Functions

```r
# Get full path to a package file
path <- dataPlotter::get_package_file_path("code_template/helper_code.R")

# Read a file from the package
content <- dataPlotter::read_package_file("www/r_functions_autocomplete.json")

# Check if a file exists in the package
exists <- dataPlotter::package_file_exists("code_template/helper_code.R")

# List all files in a package directory
files <- dataPlotter::list_package_files("code_template")
```

## Adding New Files to Your Package

### 1. Adding Template Files

1. Place your `.R` files in `inst/code_template/`
2. Update the `load_templates_ordered()` function in `R/functions.R` to include your new template
3. Rebuild the package

### 2. Adding Data Files

1. For R data objects: Place `.rda` or `.rds` files in `data/`
2. For external data: Place files in `extdata/`
3. Update documentation if needed

### 3. Adding Web Assets

1. Place files in `inst/www/`
2. Access using `system.file("www/filename", package = "dataPlotter")`

## Best Practices

### 1. Use `system.file()` for Paths
Always use `system.file()` to get paths to package files:

```r
# ✅ Correct
file_path <- system.file("code_template/helper_code.R", package = "dataPlotter")

# ❌ Incorrect (hard-coded paths)
file_path <- "inst/code_template/helper_code.R"
```

### 2. Check File Existence
Always check if files exist before trying to read them:

```r
file_path <- system.file("some_file.txt", package = "dataPlotter")
if (file_path != "" && file.exists(file_path)) {
  content <- readLines(file_path)
}
```

### 3. Use Package Functions
Use the provided package functions when possible:

```r
# ✅ Use package function
dataPlotter::load_templates_ordered()

# ❌ Direct sourcing
source("inst/code_template/load_templates.R")
```

### 4. Handle Missing Files Gracefully
Always provide fallbacks for missing files:

```r
template_content <- tryCatch({
  dataPlotter::get_template_content("my_template")
}, error = function(e) {
  # Provide default content
  "default_template_content"
})
```

## Example Usage

```r
# Load the package
library(dataPlotter)

# Load all templates
dataPlotter::load_templates_ordered()

# Check what templates are available
templates <- dataPlotter::list_templates()
print(templates)

# Load specific templates
dataPlotter::load_specific_templates(c("helper_code", "downloader_code"))

# Get template content
helper_code <- dataPlotter::get_template_content("helper_code")
cat(helper_code, sep = "\n")

# Check if a file exists
if (dataPlotter::package_file_exists("www/r_functions_autocomplete.json")) {
  cat("Autocomplete file found\n")
}
```

## Troubleshooting

### File Not Found Errors

1. **Check file location**: Make sure files are in the correct `inst/` subdirectory
2. **Rebuild package**: Run `devtools::load_all()` or reinstall the package
3. **Check package name**: Ensure you're using the correct package name in `system.file()`

### Template Loading Issues

1. **Check template order**: Use `load_templates_ordered()` for correct dependency order
2. **Verify file names**: Ensure template files have `.R` extensions
3. **Check for syntax errors**: Template files should be valid R code

### Package Installation Issues

1. **Clean and rebuild**: Run `devtools::clean_and_rebuild()`
2. **Check DESCRIPTION**: Ensure all dependencies are listed
3. **Verify NAMESPACE**: Make sure functions are properly exported 