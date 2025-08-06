# Ace Editor Fullscreen Functionality

The `dataPlotter` package provides enhanced Ace editor functions with fullscreen capability to improve the coding experience in your Shiny applications.

## Available Functions

### 1. `aceEditor_fullscreen()` - Enhanced Fullscreen Editor

This function provides a professional fullscreen experience with overlay and header.

**Features:**
- Fullscreen button positioned in the top-right corner
- Dark overlay when in fullscreen mode
- Header with editor name and exit button
- ESC key support to exit fullscreen
- Smooth transitions

**Usage:**
```r
dataPlotter::aceEditor_fullscreen(
  inputId = "my_editor",
  value = "# Your code here",
  mode = "r",
  theme = "gruvbox",
  height = "400px",
  showFullscreenButton = TRUE  # Set to FALSE to hide button
)
```

### 2. `aceEditor_simple_fullscreen()` - Simple Fullscreen Editor

This function provides a lightweight fullscreen experience without overlay.

**Features:**
- Fullscreen button above the editor
- Simple toggle without overlay
- ESC key support
- Button text changes when in fullscreen mode

**Usage:**
```r
dataPlotter::aceEditor_simple_fullscreen(
  inputId = "my_editor",
  value = "# Your code here",
  mode = "r",
  theme = "gruvbox",
  height = "400px"
)
```

### 3. `aceEditor_pre()` - Regular Editor

The original Ace editor function without fullscreen functionality.

**Usage:**
```r
dataPlotter::aceEditor_pre(
  inputId = "my_editor",
  value = "# Your code here",
  mode = "r",
  theme = "gruvbox",
  height = "400px"
)
```

## Parameters

All functions accept the same parameters as the original `aceEditor()` function:

- `inputId`: Unique identifier for the editor
- `value`: Initial code content
- `mode`: Language mode (default: "r")
- `theme`: Color theme (default: "gruvbox")
- `minLines`: Minimum number of lines
- `maxLines`: Maximum number of lines
- `fontSize`: Font size in pixels
- `height`: Editor height (can be "auto" or specific value)

**Additional parameters for `aceEditor_fullscreen()`:**
- `showFullscreenButton`: Boolean to show/hide the fullscreen button (default: TRUE)

## Examples

### Basic Usage

```r
# Enhanced fullscreen editor
ui <- fluidPage(
  dataPlotter::aceEditor_fullscreen(
    inputId = "code_editor",
    value = "# Your R code here",
    height = "500px"
  )
)

# Simple fullscreen editor
ui <- fluidPage(
  dataPlotter::aceEditor_simple_fullscreen(
    inputId = "code_editor",
    value = "# Your R code here",
    height = "500px"
  )
)
```

### In Sidebar Layout

```r
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("Editor Controls"),
      actionButton("run_code", "Run Code")
    ),
    mainPanel(
      dataPlotter::aceEditor_fullscreen(
        inputId = "main_editor",
        value = "# Main code editor\n# Use fullscreen for better editing experience",
        height = "600px"
      )
    )
  )
)
```

### Multiple Editors

```r
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Editor 1",
      dataPlotter::aceEditor_fullscreen(
        inputId = "editor1",
        value = "# First editor",
        height = "400px"
      )
    ),
    tabPanel("Editor 2",
      dataPlotter::aceEditor_simple_fullscreen(
        inputId = "editor2",
        value = "# Second editor",
        height = "400px"
      )
    )
  )
)
```

## Server-Side Access

Access the editor content in your server function:

```r
server <- function(input, output, session) {
  # Get editor content
  editor_content <- reactive({
    input$code_editor
  })
  
  # Run code when button is clicked
  observeEvent(input$run_code, {
    code <- input$code_editor
    # Process the code here
    cat("Running code:", code, "\n")
  })
}
```

## Keyboard Shortcuts

- **ESC**: Exit fullscreen mode (both enhanced and simple versions)
- **Ctrl+S**: Save (if implemented in your app)
- **Ctrl+Enter**: Run code (if implemented in your app)

## Styling Customization

The fullscreen editors include built-in CSS for styling. You can override these styles by adding your own CSS:

```r
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .ace-editor-fullscreen {
        background: #1e1e1e !important;
      }
      
      .fullscreen-header {
        background: #2d2d2d !important;
        color: white !important;
      }
    "))
  ),
  dataPlotter::aceEditor_fullscreen(
    inputId = "custom_editor",
    value = "# Custom styled editor",
    height = "400px"
  )
)
```

## Best Practices

### 1. Choose the Right Editor Type

- **Enhanced Fullscreen**: Use for professional applications where you want a polished fullscreen experience
- **Simple Fullscreen**: Use for lightweight applications or when you prefer minimal UI
- **Regular Editor**: Use when fullscreen functionality isn't needed

### 2. Set Appropriate Heights

```r
# For sidebars
height = "calc(100vh - 300px)"

# For main content areas
height = "600px"

# For auto-sizing
height = "auto"
```

### 3. Handle Large Code Content

```r
# For large code files, consider using auto height
dataPlotter::aceEditor_fullscreen(
  inputId = "large_editor",
  value = large_code_content,
  height = "auto",
  maxLines = 1000  # Allow more lines for large files
)
```

### 4. Combine with Other Features

```r
# Combine with autocompletion and tooltips
server <- function(input, output, session) {
  dataPlotter::ace_server_functions("my_editor")
}
```

## Troubleshooting

### Editor Not Resizing in Fullscreen

If the editor doesn't resize properly in fullscreen mode:

1. Make sure the editor container has proper CSS positioning
2. Check that the Ace editor is properly initialized
3. Try calling `editor.resize()` manually if needed

### Fullscreen Button Not Appearing

If the fullscreen button doesn't appear:

1. Check that `showFullscreenButton = TRUE` (default)
2. Ensure the editor container has `position: relative`
3. Check for CSS conflicts that might hide the button

### JavaScript Errors

If you see JavaScript errors:

1. Make sure jQuery is loaded (usually included with Shiny)
2. Check that the editor ID is unique
3. Ensure the Ace editor is properly initialized before fullscreen functions run

## Integration with Existing Code

To upgrade existing Ace editors to fullscreen versions:

```r
# Before
aceEditor("my_editor", value = "code")

# After
dataPlotter::aceEditor_fullscreen("my_editor", value = "code")
```

The fullscreen editors maintain all the functionality of the original Ace editor while adding the fullscreen capability. 