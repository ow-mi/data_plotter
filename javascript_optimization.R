# JavaScript optimization for Shinylive performance

#' Optimize JavaScript in UI files
#' @param ui_file Path to UI file
optimize_ui_javascript <- function(ui_file = "R/ui_global.R") {
  if (!file.exists(ui_file)) {
    stop("UI file not found: ", ui_file)
  }
  
  # Read file content
  content <- readLines(ui_file, warn = FALSE)
  
  # Find JavaScript sections
  js_start <- grep("tags\\$script\\(HTML\\(r\"---\\(", content)
  js_end <- grep("\\)---\"\\)\\)", content)
  
  if (length(js_start) > 0 && length(js_end) > 0) {
    for (i in seq_along(js_start)) {
      start_idx <- js_start[i]
      end_idx <- js_end[i]
      
      # Extract JavaScript
      js_content <- content[start_idx:end_idx]
      
      # Optimize JavaScript
      optimized_js <- optimize_javascript_code(js_content)
      
      # Replace in content
      content[start_idx:end_idx] <- optimized_js
    }
  }
  
  # Write optimized content
  writeLines(content, ui_file)
  cat("Optimized JavaScript in UI file\n")
}

#' Optimize JavaScript code
#' @param js_lines JavaScript code lines
optimize_javascript_code <- function(js_lines) {
  # Join lines into single string
  js_code <- paste(js_lines, collapse = "\n")
  
  # Remove comments
  js_code <- gsub("/\\*.*?\\*/", "", js_code, perl = TRUE)  # Multi-line comments
  js_code <- gsub("//.*$", "", js_code, perl = TRUE)        # Single-line comments
  
  # Remove extra whitespace
  js_code <- gsub("\\s+", " ", js_code)  # Multiple spaces to single
  js_code <- gsub("\\s*([{}();,])\\s*", "\\1", js_code)  # Remove spaces around operators
  
  # Remove empty lines
  js_code <- gsub("\\n\\s*\\n", "\n", js_code)
  
  # Split back into lines
  strsplit(js_code, "\n")[[1]]
}

#' Create optimized UI template
#' @param output_file Output file path
create_optimized_ui_template <- function(output_file = "R/ui_optimized.R") {
  template <- '
# Optimized UI with minimal JavaScript

ui_global_optimized <- function() {
  page_navbar(
    theme = bs_theme(bootswatch = "minty", version = 5),
    title = "Data Plotter v5",
    id = "mainmenu",
    
    # Simplified navigation structure
    nav_panel(
      title = "Input Files",
      icon = icon("upload"),
      layout_sidebar(
        sidebar = sidebar(
          title = "File Management",
          width = 400,
          class = "p-3",
          
          # Simplified file upload
          fileInput(
            "global_file_upload",
            "Select Files",
            multiple = TRUE,
            buttonLabel = list(icon("upload"), "Browse Files"),
            placeholder = "No files selected",
            width = "100%"
          ),
          
          actionButton(
            "clear_all_files",
            "Clear All Files",
            icon = icon("trash-alt"),
            class = "btn-outline-danger w-100"
          )
        ),
        
        div(class = "p-3",
          h4("Uploaded Files"),
          DT::DTOutput("global_file_list")
        )
      )
    ),
    
    nav_panel(
      title = "Combined Data", 
      icon = icon("database"),
      ui_data_combiner("combiner")
    ),
    
    nav_panel(
      title = "Analysis", 
      icon = icon("chart-area"),
      div(class = "text-center p-5",
        icon("chart-line", class = "fa-3x text-muted mb-3"),
        h4("No Plotters Created Yet"),
        p(class = "text-muted", "Click \'Add Plotter\' to create your first plot.")
      )
    ),
    
    nav_spacer(),
    
    # Simplified menus
    nav_menu(
      title = "Tools",
      icon = icon("tools"),
      align = "right",
      nav_panel(
        title = "R Code Helper",
        icon = icon("code"),
        layout_sidebar(
          sidebar = sidebar(
            title = "R Code Sandbox",
            position = "left",
            width = 500,
            aceEditor_pre("helper_input", value = "# Enter R code here")
          ),
          div(class = "p-3",
            h5("Code Output"),
            uiOutput("helper_output")
          )
        )
      )
    ),
    
    # Add buttons
    nav_item(
      div(class = "px-3 py-2",
        actionButton(
          "insert_importer", 
          "Add Data Import",
          icon = icon("file-import"), 
          class = "btn-info",
          width = "100%"
        )
      )
    ),
    
    nav_item(
      div(class = "px-3 py-2",
        actionButton(
          "insert_plot", 
          "Add Plotter",
          icon = icon("chart-area"), 
          class = "btn-success",
          width = "100%"
        )
      )
    ),
    
    # Minimal JavaScript
    tags$script(HTML("
      // Optimized JavaScript for tab management
      $(document).ready(function() {
        // Basic tab functionality
        $(\'.nav-link\').on(\'click\', function() {
          $(\'.nav-link\').removeClass(\'active\');
          $(this).addClass(\'active\');
        });
        
        // Basic file upload handling
        $(\'#global_file_upload\').on(\'change\', function() {
          console.log(\'Files selected:\', this.files.length);
        });
      });
    "))
  )
  
  writeLines(template, output_file)
  cat("Created optimized UI template:", output_file, "\n")
}'

#' Analyze JavaScript performance
#' @param ui_file Path to UI file
analyze_javascript_performance <- function(ui_file = "R/ui_global.R") {
  if (!file.exists(ui_file)) {
    stop("UI file not found: ", ui_file)
  }
  
  content <- readLines(ui_file, warn = FALSE)
  
  # Find JavaScript sections
  js_start <- grep("tags\\$script\\(HTML\\(r\"---\\(", content)
  js_end <- grep("\\)---\"\\)\\)", content)
  
  if (length(js_start) > 0 && length(js_end) > 0) {
    total_js_size <- 0
    js_sections <- 0
    
    for (i in seq_along(js_start)) {
      start_idx <- js_start[i]
      end_idx <- js_end[i]
      
      js_content <- content[start_idx:end_idx]
      js_size <- sum(nchar(js_content))
      total_js_size <- total_js_size + js_size
      js_sections <- js_sections + 1
      
      cat("JavaScript section", i, ":", round(js_size / 1024, 1), "KB\n")
    }
    
    cat("\nTotal JavaScript size:", round(total_js_size / 1024, 1), "KB\n")
    cat("Number of JavaScript sections:", js_sections, "\n")
    
    # Recommendations
    if (total_js_size > 50000) {  # 50KB
      cat("\nRecommendations:\n")
      cat("- Consider splitting JavaScript into separate files\n")
      cat("- Remove unused event handlers\n")
      cat("- Minify JavaScript code\n")
      cat("- Use lazy loading for non-critical features\n")
    }
  }
}
}
