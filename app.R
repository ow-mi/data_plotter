# app.R
library(shiny)
library(shinyTime)
library(shinyjqui)
library(bslib)
library(shinyAce)
library(shinyFiles)
library(stringr)
library(data.table)
library(DT)
library(R.utils)
# Try to load ggplot2 - may fail in WebAssembly due to missing MASS/Matrix
tryCatch({
  library(ggplot2)
  cat("ggplot2 loaded successfully\n")
}, error = function(e) {
  cat("Warning: ggplot2 failed to load:", e$message, "\n")
  cat("This is expected in WebAssembly - plotly can be used instead\n")
})
library(plotly)
library(lubridate)
library(jsonlite)
library(fasttime)
library(readxl)
library(fst)
library(nanoparquet)
library(tools)
library(scattermore)
library(purrr)
library(promises)

# Use sequential plan for shinylive compatibility
if (requireNamespace("future", quietly = TRUE)) {
  library(future)
  plan(sequential)  # Changed from multisession for browser compatibility
} else {
  cat("Warning: future package not available\n")
}

library(rstudioapi)
library(skimr)
library(htmlwidgets)
library(spsComps)
library(base64enc)

# Source helper functions and default R code strings
source("./functions.R")
source("./r_code_.R")

# Source all module files
source("./modules/module_data_table_ui.R")
source("./modules/module_data_table_server.R")
source("./modules/module_data_combiner_ui.R")
source("./modules/module_data_combiner_server.R")
source("./modules/module_importer_ui.R")
source("./modules/module_importer_server.R")
source("./modules/module_plotter_ui.R")
source("./modules/module_plotter_server.R")

# Custom downloadButton to prevent default browser download behavior
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# UI Definition
ui <- page_navbar(
  theme = bs_theme(bootswatch = "minty", version = 5),
  title = "Data Plotter v1.1",
  id = "mainmenu",

  # Input Data Management Tab
  nav_panel(
    title = "Input Data",
    icon = icon("upload"),
    layout_sidebar(
      sidebar = sidebar(
        title = "File Management",
        width = 400,
        class = "p-3",
        
        # File Upload Section
        div(class = "mb-3",
          h5("Upload Files"),
          
          # File upload buttons in tabs or side by side
          navset_pill(
            nav_panel(
              "Files",
              icon = icon("file"),
              fileInput(
                "global_file_upload",
                "Select Individual Files",
                multiple = TRUE,
                buttonLabel = list(icon("upload"), "Browse Files"),
                placeholder = "No files selected",
                width = "100%"
              ) |> tooltip("Upload individual data files: CSV, Excel (.xlsx), FST, Parquet, or compressed (.csv.gz)")
            ),
            nav_panel(
              "Folder",
              icon = icon("folder"),
              div(
                # Custom folder input with webkitdirectory
                tags$input(
                  id = "global_folder_upload",
                  type = "file",
                  webkitdirectory = NA,
                  multiple = TRUE,
                  style = "width: 100%; padding: 8px; border: 1px solid #ccc; border-radius: 4px; margin-bottom: 8px;"
                ),
                
                # Progress bar for folder upload
                div(id = "folder_upload_progress_container", style = "display: none; margin-bottom: 8px;",
                  div(class = "d-flex justify-content-between align-items-center mb-1",
                    tags$small("Processing files...", class = "text-muted"),
                    tags$small(id = "folder_upload_progress_text", "0 / 0", class = "text-muted")
                  ),
                  div(class = "progress", style = "height: 8px;",
                    div(id = "folder_upload_progress_bar", class = "progress-bar bg-primary", 
                        role = "progressbar", style = "width: 0%", 
                        `aria-valuenow` = "0", `aria-valuemin` = "0", `aria-valuemax` = "100")
                  )
                ),
                
                # File type filter
                div(class = "mb-2",
                  h6("File Type Filters", class = "text-muted mb-1 small"),
                  textInput(
                    "folder_ignore_extensions",
                    NULL,
                    value = "txt,log,tmp,bak,old,~",
                    placeholder = "txt,log,tmp,bak...",
                    width = "100%"
                  ) |> tooltip("Comma-separated list of file extensions to ignore (without dots). Leave empty to upload all files."),
                  textInput(
                    "folder_allow_extensions",
                    NULL,
                    value = "csv,xlsx,xls,fst,parquet,gz",
                    placeholder = "csv,xlsx,fst,parquet...",
                    width = "100%"
                  ) |> tooltip("Comma-separated list of file extensions to ALLOW (without dots). Leave empty to allow all files (except ignored ones).")
                ),
                
                # Filename filters
                div(class = "mb-2",
                  h6("Filename Filters", class = "text-muted mb-1 small"),
                  textInput(
                    "folder_content_include",
                    NULL,
                    value = "",
                    placeholder = "Include filenames containing (comma-separated)...",
                    width = "100%"
                  ) |> tooltip("Only upload files whose filename contains any of these comma-separated strings. Leave empty to skip filename filtering."),
                  textInput(
                    "folder_content_exclude",
                    NULL,
                    value = "",
                    placeholder = "Exclude filenames containing (comma-separated)...",
                    width = "100%"
                  ) |> tooltip("Exclude files whose filename contains any of these comma-separated strings. Applied after include filter.")
                ),
                
                p(class = "text-muted small", "Select a folder to upload all files within it. Files will be named as: folder-subfolder-filename")
              ) |> tooltip("Upload entire folders - files will be automatically named with folder structure")
            )
          ),
          
          checkboxInput(
            "overwrite_files",
            "Allow file overwrite",
            value = FALSE
          ) |> tooltip("If checked, uploading files with the same name will replace existing files")
        ),
        
        # File Actions
        div(class = "mb-3",
          h5("File Actions"),
          actionButton(
            "remove_selected_files",
            "Remove Selected",
            icon = icon("trash"),
            class = "btn-danger w-100 mb-2"
          ) |> tooltip("Remove selected files from the list"),
          
          actionButton(
            "clear_all_files",
            "Clear All Files",
            icon = icon("trash-alt"),
            class = "btn-outline-danger w-100"
          ) |> tooltip("Remove all uploaded files")
        ),
        
        # File Statistics
        div(class = "mb-3",
          h5("Statistics"),
          verbatimTextOutput("file_stats")
        )
      ),
      
      # Main content - file list
      div(class = "p-3",
        h4("Uploaded Files"),
        p(class = "text-muted", "Manage your uploaded files here. These files will be available in all Data Import tabs."),
        DT::DTOutput("global_file_list")
      )
    )
  ),

  # Dynamic data import tabs will be inserted here
  nav_panel(
    title = "Combined Data", 
    icon = icon("database"),
    ui_data_combiner("combiner")
  ),
  
  # Placeholder for where plots are inserted before
  nav_panel(
    title = "Analysis", 
    icon = icon("chart-area"),
    div(class = "text-center p-5",
      icon("chart-line", class = "fa-3x text-muted mb-3"),
      h4("No Plotters Created Yet"),
      p(class = "text-muted", "Click 'Add Plotter' to create your first plot."),
      p(class = "text-muted small", "Plotters will appear as tabs before this one.")
    )
  ),

  nav_spacer(),
  
  # Utility Tools Menu
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
          p(class = "text-muted small", "Test R code in a safe environment"),
          aceEditor_pre("helper_input", value = helper_code_template)
        ),
        div(class = "p-3",
          h5("Code Output"),
          uiOutput("helper_output")
        )
      )
    ),
    nav_panel(
      title = "Batch Download",
      icon = icon("download"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Download Configuration",
          position = "left",
          width = 500,
          p(class = "text-muted small", "Configure batch download of all plots"),
          aceEditor_pre("downloader_input", value = downloader_code_refactored)
        ),
        div(class = "p-3",
          h5("Download Interface"),
          uiOutput("downloader_output")
        )
      )
    )
  ),
  
  # Templates Menu
  nav_menu(
    title = "Templates",
    icon = icon("bookmark"),
    align = "right",
    nav_item(
      div(class = "px-3 py-2",
        h6("Load Template"),
        fileInput(
          "template_upload", 
          "Upload Template",
          accept = ".json", 
          width = "100%",
          buttonLabel = list(icon("upload"), "Browse")
        ) |> tooltip("Load a previously saved JSON template configuration")
      )
    ),
    nav_item(hr()),
    nav_item(
      div(class = "px-3 py-2",
        h6("Save Template"),
        textInput(
          "template_file_name", 
          "Filename", 
          value = paste0("template_", Sys.Date()),
          placeholder = "Enter template name..."
        ),
        downloadButton(
          "download_template", 
          "Download Template",
          icon = icon("download"),
          class = "btn-outline-primary",
          width = "100%"
        ) |> tooltip("Save current app settings as a JSON template")
      )
    )
  ),
  
  # Theme Toggle
  nav_item(
    input_dark_mode(mode = "light") |> 
      tooltip("Toggle light/dark theme")
  ),
  
  # Add Data Import
  nav_item(
    div(class = "px-3 py-2",
      action_input_tip(
        "insert_importer", 
        "Add Data Import Tab",
        icon = icon("file-import"), 
        class = "btn-info",
        width = "100%",
        tip = "Create a new data import tab for uploading and processing files"
      )
    )
  ),
  nav_item(
    div(class = "px-3 py-2",
      action_input_tip(
        "insert_plot", 
        "Add Plotter Tab",
        icon = icon("chart-area"), 
        class = "btn-success",
        width = "100%",
        tip = "Create a new plotter for visualizing your combined data"
      )
    )
  )
)

# Add JavaScript for dynamic tab renaming and folder upload
ui <- tagList(
  ui,
  tags$script(HTML(r"---(
    console.log('Tab renaming JavaScript loaded');
    
    // Dynamic navbar height calculation and module sizing
    function updateModuleHeights() {
      // Calculate the actual navbar height
      const navbar = document.querySelector('.navbar');
      const navbarHeight = navbar ? navbar.offsetHeight : 56; // fallback to typical bootstrap navbar height
      
      // Add some padding for safety (e.g., for borders, margins)
      const padding = 50;
      const totalOffset = navbarHeight + padding;
      
      // Set CSS custom property for dynamic height
      document.documentElement.style.setProperty('--navbar-height', navbarHeight + 'px');
      document.documentElement.style.setProperty('--module-height', `calc(100vh - ${totalOffset}px)`);
      
      console.log('Navbar height detected:', navbarHeight + 'px');
      console.log('Module height set to:', `calc(100vh - ${totalOffset}px)`);
      
      // Apply to all module containers
      const moduleSelectors = [
        '[data-bs-toggle=\"pill\"]', // navset_card_pill containers
        '.layout-sidebar',            // layout_sidebar containers
        '.card-tabs',                 // card tab containers
        '.module-container'           // custom module containers
      ];
      
      moduleSelectors.forEach(selector => {
        const elements = document.querySelectorAll(selector);
        elements.forEach(el => {
          // Only apply to top-level module containers, not nested ones
          if (!el.closest('.tab-pane') && !el.closest('.sidebar')) {
            el.style.height = `calc(100vh - ${totalOffset}px)`;
            el.style.maxHeight = `calc(100vh - ${totalOffset}px)`;
            el.style.overflow = 'hidden';
          }
        });
      });
    }
    
    // Run on page load
    document.addEventListener('DOMContentLoaded', function() {
      updateModuleHeights();
    });
    
    // Run when window resizes
    window.addEventListener('resize', updateModuleHeights);
    
    // Run when new tabs are added (using MutationObserver)
    // Throttle the height update function to prevent excessive calls
    let heightUpdateTimeout = null;
    function throttledUpdateModuleHeights() {
      if (heightUpdateTimeout) {
        clearTimeout(heightUpdateTimeout);
      }
      heightUpdateTimeout = setTimeout(updateModuleHeights, 150);
    }

    const observer = new MutationObserver(function(mutations) {
      let shouldUpdate = false;
      
      mutations.forEach(function(mutation) {
        if (mutation.type === 'childList' && mutation.addedNodes.length > 0) {
          // Only trigger for significant DOM changes, not every hover/input change
          const hasNewNavPanel = Array.from(mutation.addedNodes).some(node => 
            node.nodeType === 1 && (
              node.classList?.contains('nav-item') || 
              node.classList?.contains('tab-pane') ||
              node.querySelector?.('.nav-item') ||
              node.querySelector?.('.module-container')
            )
          );
          
          if (hasNewNavPanel) {
            shouldUpdate = true;
          }
        }
      });
      
      if (shouldUpdate) {
        throttledUpdateModuleHeights();
      }
    });
    
         // Observe the navbar and main content for changes
     const targetElements = [
       document.querySelector('.navbar'),
       document.querySelector('#mainmenu'),
       document.body
     ].filter(Boolean);
     
     targetElements.forEach(target => {
       if (target) {
         observer.observe(target, { 
           childList: true, 
           subtree: true 
         });
       }
     });
     
     // Store tab mappings to help with repeated renames
     window.tabMappings = window.tabMappings || {};
     
     // Handle custom message for selecting/deselecting all plots
    Shiny.addCustomMessageHandler('selectAllPlots', function(data) {
      var checkboxes = document.querySelectorAll('input[id^=\"select_plot_\"]');
      checkboxes.forEach(function(checkbox) {
        checkbox.checked = data.select;
      });
    });
    
    // Folder upload handler
    $(document).ready(function() {
      console.log('Setting up folder upload handler');
      
      // Handle folder selection
      $(document).on('change', '#global_folder_upload', function(e) {
        console.log('Folder upload change detected');
        var files = e.target.files;
        console.log('Number of files selected:', files.length);
        
        if (files.length > 0) {
          // Show progress bar
          $('#folder_upload_progress_container').show();
          $('#folder_upload_progress_bar').css('width', '0%').attr('aria-valuenow', 0);
          $('#folder_upload_progress_text').text('0 / ' + files.length);
          // Get file type filters from Shiny inputs
          var ignoreExtensions = [];
          var allowExtensions = [];
          var filenameIncludeStrings = [];
          var filenameExcludeStrings = [];
          
          // Read the filter inputs
          var ignoreInput = $('#folder_ignore_extensions').val();
          var allowInput = $('#folder_allow_extensions').val();
          var filenameIncludeInput = $('#folder_content_include').val();
          var filenameExcludeInput = $('#folder_content_exclude').val();
          
          if (ignoreInput && ignoreInput.trim() !== '') {
            ignoreExtensions = ignoreInput.toLowerCase().split(',').map(function(ext) {
              return ext.trim().replace(/^\\./, ''); // Remove leading dot if present
            });
          }
          
          if (allowInput && allowInput.trim() !== '') {
            allowExtensions = allowInput.toLowerCase().split(',').map(function(ext) {
              return ext.trim().replace(/^\\./, ''); // Remove leading dot if present
            });
          }
          
          if (filenameIncludeInput && filenameIncludeInput.trim() !== '') {
            filenameIncludeStrings = filenameIncludeInput.split(',').map(function(str) {
              return str.trim().toLowerCase();
            }).filter(function(str) {
              return str.length > 0;
            });
          }
          
          if (filenameExcludeInput && filenameExcludeInput.trim() !== '') {
            filenameExcludeStrings = filenameExcludeInput.split(',').map(function(str) {
              return str.trim().toLowerCase();
            }).filter(function(str) {
              return str.length > 0;
            });
          }
          
          console.log('Ignore extensions:', ignoreExtensions);
          console.log('Allow extensions:', allowExtensions);
          console.log('Filename include strings:', filenameIncludeStrings);
          console.log('Filename exclude strings:', filenameExcludeStrings);
          
          // Update progress bar to show filtering phase
          $('#folder_upload_progress_text').text('Filtering files...');
          
          // Create array for filtered files
          var fileDataArray = [];
          var filesFiltered = 0;
          
          // Apply all filters in one pass: extension filtering and filename filtering
          for (var i = 0; i < files.length; i++) {
            var file = files[i];
            
            // Extract folder structure from webkitRelativePath
            var relativePath = file.webkitRelativePath || file.name;
            var fileName = relativePath.split('/').pop(); // Get just the filename
            
            // Get file extension
            var fileExt = '';
            var lastDotIndex = fileName.lastIndexOf('.');
            if (lastDotIndex > 0 && lastDotIndex < fileName.length - 1) {
              fileExt = fileName.substring(lastDotIndex + 1).toLowerCase();
            }
            
            // Apply file type filters
            var shouldInclude = true;
            
            // Check if file extension should be ignored
            if (ignoreExtensions.length > 0 && ignoreExtensions.includes(fileExt)) {
              console.log('Ignoring file (extension blacklisted):', fileName, 'ext:', fileExt);
              shouldInclude = false;
              filesFiltered++;
            }
            
            // Check if file extension is in allow list (if allow list is specified)
            if (shouldInclude && allowExtensions.length > 0 && !allowExtensions.includes(fileExt)) {
              console.log('Ignoring file (not in allow list):', fileName, 'ext:', fileExt);
              shouldInclude = false;
              filesFiltered++;
            }
            
            if (!shouldInclude) {
              continue; // Skip this file
            }
            
            // Create new file name with folder structure
            var pathParts = relativePath.split('/');
            var folderName = pathParts[0]; // Root folder name
            
            // Create hierarchical name: folder-subfolder-filename
            var newName;
            if (pathParts.length > 2) {
              // Has subfolders
              var subfolders = pathParts.slice(1, -1).join('-');
              newName = folderName + '-' + subfolders + '-' + fileName;
            } else {
              // Direct file in root folder
              newName = folderName + '-' + fileName;
            }
            
            console.log('File passed extension filter:', relativePath, '-> New name:', newName);
            
            // Apply filename include filter
            if (filenameIncludeStrings.length > 0) {
              var includeMatch = filenameIncludeStrings.some(function(includeStr) {
                return newName.toLowerCase().includes(includeStr);
              });
              
              if (!includeMatch) {
                console.log('Ignoring file (filename include filter):', newName);
                shouldInclude = false;
                filesFiltered++;
              }
            }
            
            // Apply filename exclude filter
            if (shouldInclude && filenameExcludeStrings.length > 0) {
              var excludeMatch = filenameExcludeStrings.some(function(excludeStr) {
                return newName.toLowerCase().includes(excludeStr);
              });
              
              if (excludeMatch) {
                console.log('Ignoring file (filename exclude filter):', newName);
                shouldInclude = false;
                filesFiltered++;
              }
            }
            
            if (!shouldInclude) {
              continue; // Skip this file
            }
            
            // Store file info for files that passed all filters
            var fileInfo = {
              name: newName,
              size: file.size,
              type: file.type,
              lastModified: file.lastModified,
              originalPath: relativePath,
              file: file,
              extension: fileExt
            };
            
            fileDataArray.push(fileInfo);
          }
          
          // All filtering complete, proceed with filtered files
          console.log('Filtering complete, proceeding with', fileDataArray.length, 'files (', filesFiltered, 'filtered out)');
          proceedWithFilteredFiles();
          
          function proceedWithFilteredFiles() {
            // Send folder data to Shiny
            console.log('Sending folder data to Shiny:', fileDataArray.length, 'files (', filesFiltered, 'filtered out)');
            Shiny.setInputValue('global_folder_upload_data', {
              files: fileDataArray.map(function(f) {
                return {
                  name: f.name,
                  size: f.size,
                  type: f.type,
                  lastModified: f.lastModified,
                  originalPath: f.originalPath,
                  extension: f.extension
                };
              }),
              filesFiltered: filesFiltered,
              totalFiles: files.length,
              timestamp: Date.now()
            });
            
            // Process files sequentially with progress updates and batched sending
            var fileResults = [];
            var totalFiles = fileDataArray.length;
            var processedFiles = 0;
            var batchSize = 5; // Send files in smaller batches to improve responsiveness
            var batchCounter = 0;
            
            function sendBatchToShiny(batch, isLastBatch) {
              console.log('Sending batch of', batch.length, 'files to Shiny (batch', batchCounter + 1, ')');
              Shiny.setInputValue('global_folder_upload_files_batch', {
                files: batch,
                batchNumber: batchCounter,
                isLastBatch: isLastBatch,
                totalBatches: Math.ceil(totalFiles / batchSize),
                timestamp: Date.now()
              });
              batchCounter++;
            }
          
          function processNextFile(index) {
            if (index >= totalFiles) {
              // All files processed - send final batch if any remain and hide progress
              if (fileResults.length > 0) {
                sendBatchToShiny(fileResults, true);
              }
              console.log('All files processed and sent to Shiny in batches');
              $('#folder_upload_progress_container').hide();
              return;
            }
            
            var fileData = fileDataArray[index];
            var reader = new FileReader();
            
            reader.onload = function(e) {
              fileResults.push({
                index: index,
                name: fileData.name,
                dataURL: e.target.result,
                originalPath: fileData.originalPath
              });
              
              // Send batch to Shiny when batch is full
              if (fileResults.length >= batchSize) {
                sendBatchToShiny(fileResults.slice(), false); // Send copy of current batch
                fileResults = []; // Clear the batch
              }
               
               processedFiles++;
               var progressPercent = Math.round((processedFiles / totalFiles) * 100);
               
               // Update progress bar
               $('#folder_upload_progress_bar').css('width', progressPercent + '%').attr('aria-valuenow', progressPercent);
               var statusText = processedFiles + ' / ' + totalFiles;
               if (processedFiles < totalFiles) {
                 statusText += ' (reading files...)';
               } else {
                 statusText += ' (sending to server...)';
               }
               $('#folder_upload_progress_text').text(statusText);
               
               console.log('Processed file', processedFiles, 'of', totalFiles, '(' + progressPercent + '%)');
               
               // Use longer delay for larger files to prevent UI blocking
               var delay = fileData.size > 1000000 ? 100 : 50; // 100ms for files > 1MB, 50ms otherwise
               setTimeout(function() { processNextFile(index + 1); }, delay);
             };
            
            reader.onerror = function(e) {
              console.error('Error reading file:', fileData.name, e);
              processedFiles++;
              var progressPercent = Math.round((processedFiles / totalFiles) * 100);
              
                             // Update progress bar even on error
               $('#folder_upload_progress_bar').css('width', progressPercent + '%').attr('aria-valuenow', progressPercent);
               var statusText = processedFiles + ' / ' + totalFiles;
               if (processedFiles < totalFiles) {
                 statusText += ' (reading files...)';
               } else {
                 statusText += ' (sending to server...)';
               }
               $('#folder_upload_progress_text').text(statusText);
              
              // Process next file with longer delay to prevent UI blocking
              setTimeout(function() { processNextFile(index + 1); }, 50);
            };
            
            reader.readAsDataURL(fileData.file);
          }
          
            // Start processing files
            if (totalFiles > 0) {
              $('#folder_upload_progress_text').text('0 / ' + totalFiles + ' (reading files...)');
              processNextFile(0);
            } else {
              $('#folder_upload_progress_container').hide();
            }
          } // End of proceedWithFilteredFiles function
         } else {
           // No files selected - hide progress bar
           $('#folder_upload_progress_container').hide();
         }
       });
    });
    
    // Handle custom message for updating tab titles
    Shiny.addCustomMessageHandler('updateNavTabTitle', function(data) {
      console.log('=== TAB RENAME MESSAGE RECEIVED ===');
      console.log('Module ID:', data.moduleId, 'New Title:', data.newTitle, 'Old Title:', data.oldTitle);
      
      var moduleId = data.moduleId;
      var newTitle = data.newTitle;
      var oldTitle = data.oldTitle;
      var success = false;
      var updatedElement = null;
      
      // Strategy 1: Find by previously stored data-module-id attribute
      var existingElement = document.querySelector('.nav-link[data-module-id="' + moduleId + '"]');
      if (existingElement) {
        existingElement.textContent = newTitle;
        updatedElement = existingElement;
        success = true;
        console.log('SUCCESS: Updated via stored module ID');
      }
      
      // Strategy 2: Find by exact text match with old title
      if (!success && oldTitle) {
        var textMatches = Array.from(document.querySelectorAll('.nav-link')).filter(function(link) {
          return link.textContent.trim() === oldTitle.trim();
        });
        
        if (textMatches.length > 0) {
          textMatches[0].textContent = newTitle;
          updatedElement = textMatches[0];
          success = true;
          console.log('SUCCESS: Updated via exact text match');
        }
      }
      
      // Strategy 3: Find by module ID pattern (for import and plotter modules)
      if (!success) {
        var navLinks = document.querySelectorAll('.nav-link');
        
        // For import modules, look for 'Import X' pattern and match the number
        if (moduleId.includes('data_import_module_')) {
          var moduleNumber = moduleId.replace('data_import_module_', '');
          var importPattern = new RegExp('Import\\s+' + moduleNumber + '($|\\s)');
          
          Array.from(navLinks).forEach(function(link) {
            if (!success && importPattern.test(link.textContent.trim())) {
              link.textContent = newTitle;
              updatedElement = link;
              success = true;
              console.log('SUCCESS: Updated import module via pattern matching');
            }
          });
        }
        
        // For plotter modules, look for 'plotter_X' pattern
        if (!success && moduleId.includes('plotter_')) {
          Array.from(navLinks).forEach(function(link) {
            if (!success && link.textContent.trim() === moduleId) {
              link.textContent = newTitle;
              updatedElement = link;
              success = true;
              console.log('SUCCESS: Updated plotter module via ID matching');
            }
          });
        }
      }
      
      // Strategy 4: Check stored mappings as fallback
      if (!success && window.tabMappings[moduleId]) {
        console.log('Strategy 4: Looking for previously mapped tab');
        var mappedTitle = window.tabMappings[moduleId];
        var mappedElements = Array.from(document.querySelectorAll('.nav-link, .navbar-nav a, .nav-item a')).filter(function(link) {
          return link.textContent.trim() === mappedTitle.trim();
        });
        
        console.log('Found', mappedElements.length, 'elements matching mapped title:', mappedTitle);
        
        if (mappedElements.length > 0) {
          mappedElements[0].textContent = newTitle;
          updatedElement = mappedElements[0];
          success = true;
          console.log('SUCCESS: Updated via mapping');
        }
      }
      
      // Strategy 5: Brute force - find any nav element and update it (last resort)
      if (!success) {
        console.log('Strategy 5: Brute force update of first available nav element');
        var allNavElements = document.querySelectorAll('.nav-link, .navbar-nav a, .nav-item a');
        
        // Look for elements that might be import tabs (avoid updating other tabs)
        var candidateElements = Array.from(allNavElements).filter(function(element) {
          var text = element.textContent.trim();
          return text.includes('Import') || text.includes('Data') || text === oldTitle;
        });
        
        if (candidateElements.length > 0) {
          candidateElements[0].textContent = newTitle;
          updatedElement = candidateElements[0];
          success = true;
          console.log('SUCCESS: Updated via brute force');
        }
      }
      
      // Store the mapping for future renames
      if (success && updatedElement) {
        window.tabMappings[moduleId] = newTitle;
        console.log('Stored mapping:', moduleId, '->', newTitle);
        
        // Also add a data attribute to the element for future identification
        updatedElement.setAttribute('data-module-id', moduleId);
      }
      
      if (success) {
        console.log('=== TAB RENAME SUCCESSFUL ===');
      } else {
        console.log('=== TAB RENAME FAILED ===');
      }
    });
    
         console.log('Tab renaming handler registered');
     
     // Fixed tab renaming code for Bootstrap 5
     $(document).on('dblclick', '.nav-link', function(e) {
       e.preventDefault();
       
       var $navLink = $(this);
       var currentText = $navLink.text().trim();
       
       console.log('Double-click detected on tab:', currentText);
       
       // Don't allow editing if already editing
       if ($navLink.find('input').length > 0) {
         return;
       }
       
       // Create input element
       var $input = $('<input type="text" class="form-control form-control-sm tab-rename-input">');
       $input.css({
         'width': 'auto',
         'min-width': '120px',
         'display': 'inline-block',
         'font-size': '14px',
         'padding': '2px 8px',
         'margin': '0',
         'border': '2px solid #007bff',
         'border-radius': '4px'
       });
       $input.val(currentText);
       
       // Store original text
       $navLink.data('original-text', currentText);
       
       // Replace tab text with input
       $navLink.html($input);
       $input.focus().select();
       
       // Handle input events
       $input.on('blur keydown', function(e) {
         if (e.type === 'blur' || e.keyCode === 13) { // Enter key
           var newText = $(this).val().trim();
           
           if (newText && newText !== currentText) {
             console.log('Tab rename requested:', currentText, '->', newText);
             
             // Send message to Shiny for server-side update
             Shiny.setInputValue('tab_rename_request', {
               oldText: currentText,
               newText: newText,
               timestamp: Date.now()
             });
           }
           
           // Restore original text (will be updated by server if successful)
           $navLink.text(currentText);
           
         } else if (e.keyCode === 27) { // Escape key
           console.log('Tab rename cancelled');
           $navLink.text(currentText);
         }
       });
       
       // Prevent the input from triggering navigation
       $input.on('click', function(e) {
         e.stopPropagation();
         e.preventDefault();
       });
     });
   )---")),
  
  # Add CSS for module styling
  tags$style(HTML("
    /* CSS custom properties for dynamic heights */
    :root {
      --navbar-height: 56px; /* fallback */
      --module-height: calc(100vh - 76px); /* fallback */
    }
    
    /* Apply dynamic height to main module containers */
    .module-container,
    .navset-card-pill,
    .layout-sidebar {
      height: var(--module-height) !important;
      max-height: var(--module-height) !important;
    }
    
    /* Ensure sidebar content is scrollable */
    .sidebar .sidebar-content {
      height: calc(100% - 40px) !important;
      overflow-y: auto !important;
    }
    
    /* Ensure main content areas are scrollable */
    .tab-content > .tab-pane,
    .layout-sidebar > .layout-sidebar-main {
      height: 100% !important;
      overflow-y: auto !important;
    }
    
    /* Fix for nested tab containers */
    .tab-content .navset-card-pill,
    .tab-content .layout-sidebar {
      height: calc(100% - 20px) !important;
      max-height: calc(100% - 20px) !important;
    }
    
    /* Ensure tables and plots fill their containers properly */
    .dataTables_wrapper {
      height: calc(100% - 50px) !important;
      max-height: calc(100% - 50px) !important;
      overflow: auto !important;
    }
    
    .plotly,
    .shiny-plot-output {
      height: calc(100% - 50px) !important;
      max-height: calc(100% - 50px) !important;
      overflow: auto !important;
    }
    
    /* Ensure card bodies take full height */
    .card-body {
      display: flex !important;
      flex-direction: column !important;
      height: 100% !important;
    }
    
    /* Ace editor specific styling for sidebars */
    .sidebar .ace_editor {
      height: calc(100vh - 350px) !important;
      min-height: 300px !important;
      max-height: calc(100vh - 200px) !important;
    }
    
    /* Ace editor in popovers should stay smaller */
    .popover .ace_editor {
      height: 200px !important;
      min-height: 150px !important;
      max-height: 300px !important;
    }
    
    /* Large popover for table code editing */
    .large-popover {
      max-width: 800px !important;
      width: 800px !important;
    }
    
    .large-popover .popover-body {
      padding: 15px !important;
      max-height: 500px !important;
      overflow-y: auto !important;
    }
    
    .large-popover .ace_editor {
      height: 300px !important;
      min-height: 250px !important;
      max-height: 400px !important;
    }
    
    /* Responsive adjustments */
    @media (max-width: 768px) {
      :root {
        --module-height: calc(100vh - 90px); /* More space for mobile navbar */
      }
    }
  "))
)

# Server Logic
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024^2) # 1GB
  toggle_dark_mode(mode = "dark")

  # JSON-Based Template Save/Load Logic
  safe_input_value <- function(val) {
    if (is.null(val)) {
      return(NULL)
    } else if (is.list(val) && length(val) > 1) {
      return(as.character(val))
    } else if (inherits(val, c("Date", "POSIXct", "POSIXlt"))) {
      return(as.character(val))
    } else if (is.logical(val) || is.numeric(val)) {
      return(val)
    } else {
      return(as.character(val))
    }
  }

  output$download_template <- downloadHandler(
    filename = function() {
      req(input$template_file_name)
      paste0(tools::file_path_sans_ext(input$template_file_name), ".json")
    },
    content = function(file) {
      tryCatch({
        all_inputs_list <- reactiveValuesToList(input)
        
        cat("=== JSON TEMPLATE SAVE ===\n")
        cat("Total inputs captured:", length(all_inputs_list), "\n")
        
        # Get current module counts
        current_importer_count <- isolate(importer_counter())
        current_plotter_count <- isolate(plotter_counter())
        
        # Split inputs by category
        general_inputs <- list()
        importer_inputs <- list()
        plotter_inputs <- list()
        ace_inputs <- list()
        
        for (input_name in names(all_inputs_list)) {
          val <- safe_input_value(all_inputs_list[[input_name]])
          
          if (grepl("r_code", input_name)) {
            ace_inputs[[input_name]] <- val
          } else if (grepl("data_import_module_[0-9]+", input_name)) {
            importer_inputs[[input_name]] <- val
          } else if (grepl("plotter_[0-9]+", input_name)) {
            plotter_inputs[[input_name]] <- val
          } else {
            general_inputs[[input_name]] <- val
          }
        }
        
        # Create structured template
        template_data <- list(
          metadata = list(
            template_version = "2.0",
            created_date = as.character(Sys.time()),
            importer_count = current_importer_count,
            plotter_count = current_plotter_count,
            total_inputs = length(all_inputs_list)
          ),
          modules = list(
            importers = current_importer_count,
            plotters = current_plotter_count
          ),
          inputs = list(
            general = general_inputs,
            importers = importer_inputs,
            plotters = plotter_inputs,
            ace_editors = ace_inputs
          )
        )
        
        # Write JSON file
        jsonlite::write_json(template_data, file, pretty = TRUE, auto_unbox = TRUE)
        
        cat("Saved:", length(general_inputs), "general,", length(importer_inputs), "importer,", 
            length(plotter_inputs), "plotter,", length(ace_inputs), "ace inputs\n")
        
        showNotification(paste("Template saved:", current_importer_count, "importers,", 
                               current_plotter_count, "plotters"), type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error saving template:", e$message), type = "error", duration = 10)
        # Create minimal JSON file to prevent download failure
        minimal_template <- list(
          metadata = list(template_version = "2.0", error = e$message),
          inputs = list()
        )
        jsonlite::write_json(minimal_template, file, pretty = TRUE, auto_unbox = TRUE)
      })
    }
  )

  observeEvent(input$template_upload, {
    req(input$template_upload)
    tryCatch({
      # Load JSON template
      template_data <- jsonlite::fromJSON(input$template_upload$datapath, simplifyVector = FALSE)
      
      cat("=== JSON TEMPLATE LOAD ===\n")
      cat("Template version:", if(!is.null(template_data$metadata$template_version)) template_data$metadata$template_version else "unknown", "\n")
      
      # Validate template structure
      if (is.null(template_data$metadata) || is.null(template_data$inputs)) {
        showNotification("Invalid JSON template file structure", type = "error", duration = 10)
        return()
      }
      
      # Extract module requirements
      importer_count_needed <- if(!is.null(template_data$modules$importers)) as.integer(template_data$modules$importers) else 1
      plotter_count_needed <- if(!is.null(template_data$modules$plotters)) as.integer(template_data$modules$plotters) else 0
      
      # Ensure valid counts
      importer_count_needed <- max(1, importer_count_needed)
      plotter_count_needed <- max(0, plotter_count_needed)
      
      cat("Modules needed - Importers:", importer_count_needed, "Plotters:", plotter_count_needed, "\n")
      
      # Get current counts
      current_importer_count <- isolate(importer_counter())
      current_plotter_count <- isolate(plotter_counter())
      
      cat("Current modules - Importers:", current_importer_count, "Plotters:", current_plotter_count, "\n")
      
      # STEP 1: Create required importer modules
      if (current_importer_count < importer_count_needed) {
        tabs_to_create <- importer_count_needed - current_importer_count
        cat("Creating", tabs_to_create, "additional importer tabs\n")
        
        for (i in 1:tabs_to_create) {
          current_count <- isolate(importer_counter()) + 1
          importer_counter(current_count)
          import_id <- paste0("data_import_module_", current_count)

          nav_insert(
            id = "mainmenu",
            target = "Combined Data",
            position = "before",
            nav = nav_panel(
              title = paste("Import", current_count),
              ui_data_importer(import_id)
            ),
            session = session
          )
          
          # Initialize server module
          importer_module_output <- server_data_import(import_id, global_files)
          current_instances <- importer_instances()
          current_instances[[import_id]] <- importer_module_output
          importer_instances(current_instances)
          
          # Observe tab rename trigger from this importer module
          observeEvent(importer_module_output$tab_rename_trigger(), {
            req(importer_module_output$tab_rename_trigger())
            
            rename_data <- importer_module_output$tab_rename_trigger()
            cat("Importer tab rename triggered:", rename_data$newName, "\n")
            
            # Send message to JavaScript to update the tab
            session$sendCustomMessage("updateNavTabTitle", list(
              moduleId = rename_data$moduleId,
              newTitle = rename_data$newName,
              oldTitle = rename_data$currentTitle
            ))
          })
          
          cat("Created importer:", import_id, "\n")
        }
      }
      
      # STEP 2: Create required plotter modules
      if (current_plotter_count < plotter_count_needed) {
        tabs_to_create <- plotter_count_needed - current_plotter_count
        cat("Creating", tabs_to_create, "additional plotter tabs\n")
        
        for (i in 1:tabs_to_create) {
          current_count <- isolate(plotter_counter()) + 1
          plotter_counter(current_count)
          plot_id <- paste0("plotter_", current_count)

          nav_insert(
            id = "mainmenu",
            target = "Analysis",
            position = "before",
            nav = nav_panel(
              title = plot_id,
              ui_plotter(plot_id)
            ),
            session = session
          )
          
          # Initialize server module
          plotter_module_output <- server_plotter(plot_id, data_combiner$df, input)
          current_instances <- plotter_instances()
          current_instances[[plot_id]] <- plotter_module_output
          plotter_instances(current_instances)
          
          # Observe plot rename trigger from this plotter module
          observeEvent(plotter_module_output$plot_rename_trigger(), {
            req(plotter_module_output$plot_rename_trigger())
            
            rename_data <- plotter_module_output$plot_rename_trigger()
            cat("Plotter tab rename triggered:", rename_data$newName, "\n")
            
            # Send message to JavaScript to update the tab
            session$sendCustomMessage("updateNavTabTitle", list(
              moduleId = rename_data$moduleId,
              newTitle = rename_data$newName,
              oldTitle = rename_data$currentTitle
            ))
          })
          
          cat("Created plotter:", plot_id, "\n")
        }
      }
      
      # STEP 3: Apply inputs in order (modules are now ready)
      cat("Applying template inputs...\n")
      
      # Helper function to safely update inputs
      safe_update_input <- function(input_id, value) {
        if (is.null(value)) return(FALSE)
        
        tryCatch({
          current_val <- isolate(input[[input_id]])
          
          if (is.null(current_val)) {
            cat("  Input", input_id, "not found, skipping\n")
            return(FALSE)
          }
          
          # Convert value based on current input type
          if (is.logical(current_val)) {
            updateCheckboxInput(session, input_id, value = as.logical(value))
          } else if (is.numeric(current_val)) {
            updateNumericInput(session, input_id, value = as.numeric(value))
          } else if (inherits(current_val, "Date")) {
            updateDateInput(session, input_id, value = as.Date(value))
          } else if (is.character(value) && length(value) > 1) {
            # Multiple values for selectize
            updateSelectizeInput(session, input_id, selected = value)
          } else {
            # Try select first, then text
            tryCatch({
              updateSelectInput(session, input_id, selected = as.character(value))
            }, error = function(e) {
              updateTextInput(session, input_id, value = as.character(value))
            })
          }
          
          cat("  Updated", input_id, "=", paste(value, collapse=","), "\n")
          return(TRUE)
        }, error = function(e) {
          cat("  Failed to update", input_id, ":", e$message, "\n")
          return(FALSE)
        })
      }
      
      # Apply general inputs first
      if (!is.null(template_data$inputs$general)) {
        cat("Applying", length(template_data$inputs$general), "general inputs\n")
        for (input_id in names(template_data$inputs$general)) {
          safe_update_input(input_id, template_data$inputs$general[[input_id]])
        }
      }
      
      # Apply importer inputs
      if (!is.null(template_data$inputs$importers)) {
        cat("Applying", length(template_data$inputs$importers), "importer inputs\n")
        for (input_id in names(template_data$inputs$importers)) {
          safe_update_input(input_id, template_data$inputs$importers[[input_id]])
        }
      }
      
      # Apply ace editor inputs
      if (!is.null(template_data$inputs$ace_editors)) {
        cat("Applying", length(template_data$inputs$ace_editors), "ace editor inputs\n")
        for (ace_id in names(template_data$inputs$ace_editors)) {
          ace_value <- template_data$inputs$ace_editors[[ace_id]]
          if (!is.null(ace_value) && nzchar(ace_value)) {
            tryCatch({
              updateAceEditor(session, ace_id, value = as.character(ace_value))
              cat("  Updated ace editor:", ace_id, "\n")
            }, error = function(e) {
              cat("  Failed to update ace editor", ace_id, ":", e$message, "\n")
            })
          }
        }
      }
      
      # Apply plotter inputs (these should work now that modules exist)
      if (!is.null(template_data$inputs$plotters)) {
        cat("Applying", length(template_data$inputs$plotters), "plotter inputs\n")
        successful_count <- 0
        
        for (input_id in names(template_data$inputs$plotters)) {
          if (safe_update_input(input_id, template_data$inputs$plotters[[input_id]])) {
            successful_count <- successful_count + 1
          }
        }
        
        cat("Successfully applied", successful_count, "out of", length(template_data$inputs$plotters), "plotter inputs\n")
      }
      
      showNotification(paste("Template loaded successfully:", importer_count_needed, "importers,", 
                             plotter_count_needed, "plotters"), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Failed to load template:", e$message), type = "error", duration = 10)
      cat("Template load error:", e$message, "\n")
    })
  })

  # Global File Management for Input Data Tab
  global_files <- reactiveVal(list())
  
  # File upload handler for individual files
  observeEvent(input$global_file_upload, {
    req(input$global_file_upload)
    
    current_files <- global_files()
    new_files <- input$global_file_upload
    
    for (i in 1:nrow(new_files)) {
      file_name <- new_files$name[i]
      file_path <- new_files$datapath[i]
      
      # Check if file already exists
      if (file_name %in% names(current_files) && !input$overwrite_files) {
        showNotification(
          paste("File", file_name, "already exists. Enable overwrite to replace it."),
          type = "warning"
        )
        next
      }
      
      # Store file info
      current_files[[file_name]] <- list(
        name = file_name,
        path = file_path,
        size = file.info(file_path)$size,
        uploaded = Sys.time(),
        type = tools::file_ext(file_name),
        upload_type = "individual"
      )
    }
    
    global_files(current_files)
    showNotification(paste("Uploaded", nrow(new_files), "individual file(s)"), type = "message")
  })
  
  # Reactive values to track batched folder upload
  folder_upload_state <- reactiveValues(
    total_files_expected = 0,
    files_processed = 0,
    files_skipped = 0,
    files_filtered = 0,
    upload_in_progress = FALSE,
    batches_received = 0,
    total_batches = 0
  )
  
  # Batched folder upload handler - receives file data from JavaScript in chunks
  observeEvent(input$global_folder_upload_files_batch, {
    req(input$global_folder_upload_files_batch)
    
    batch_data <- input$global_folder_upload_files_batch
    batch_number <- batch_data$batchNumber
    is_last_batch <- batch_data$isLastBatch
    total_batches <- batch_data$totalBatches
    
    cat("=== FOLDER UPLOAD BATCH", batch_number + 1, "of", total_batches, "RECEIVED ===\n")
    cat("Number of files in this batch:", length(batch_data$files), "\n")
    cat("Is last batch:", is_last_batch, "\n")
    
    # Initialize or update upload state
    if (batch_number == 0) {
      folder_upload_state$upload_in_progress <- TRUE
      folder_upload_state$total_batches <- total_batches
      folder_upload_state$batches_received <- 0
      cat("Starting new folder upload session\n")
    }
    
    folder_upload_state$batches_received <- folder_upload_state$batches_received + 1
    
    current_files <- global_files()
    batch_files_processed <- 0
    batch_files_skipped <- 0
    
    # Process each file in this batch
    for (file_info in batch_data$files) {
      file_name <- file_info$name
      data_url <- file_info$dataURL
      original_path <- file_info$originalPath
      
      cat("Processing folder file:", file_name, "(original:", original_path, ")\n")
      
      # Check if file already exists
      if (file_name %in% names(current_files) && !input$overwrite_files) {
        cat("File already exists, skipping:", file_name, "\n")
        batch_files_skipped <- batch_files_skipped + 1
        next
      }
      
      # Decode the data URL and save to temp file
      tryCatch({
        # Remove data URL prefix
        data_part <- sub("^data:[^,]*,", "", data_url)
        
        # Decode base64
        file_content <- base64enc::base64decode(data_part)
        
        # Create temporary file
        temp_file <- tempfile(fileext = paste0(".", tools::file_ext(file_name)))
        writeBin(file_content, temp_file)
        
        # Store file info
        current_files[[file_name]] <- list(
          name = file_name,
          path = temp_file,
          size = length(file_content),
          uploaded = Sys.time(),
          type = tools::file_ext(file_name),
          upload_type = "folder",
          original_path = original_path
        )
        
        batch_files_processed <- batch_files_processed + 1
        cat("Successfully processed:", file_name, "\n")
        
      }, error = function(e) {
        cat("Error processing file", file_name, ":", e$message, "\n")
        showNotification(
          paste("Error processing", file_name, ":", e$message),
          type = "error"
        )
      })
    }
    
    # Update cumulative counters
    folder_upload_state$files_processed <- folder_upload_state$files_processed + batch_files_processed
    folder_upload_state$files_skipped <- folder_upload_state$files_skipped + batch_files_skipped
    
    global_files(current_files)
    
    # Show progress notification
    if (!is_last_batch) {
      showNotification(
        paste("Processed batch", batch_number + 1, "of", total_batches, 
              "(", batch_files_processed, "files in this batch)"),
        type = "message", duration = 2
      )
    } else {
      # Final batch - show summary
      folder_upload_state$upload_in_progress <- FALSE
      
      summary_msg <- paste("Folder upload complete:", 
                           folder_upload_state$files_processed, "files processed")
      if (folder_upload_state$files_skipped > 0) {
        summary_msg <- paste0(summary_msg, ", ", folder_upload_state$files_skipped, 
                             " files skipped (already exist)")
      }
      
      showNotification(summary_msg, type = "message", duration = 5)
      cat("Folder upload complete:", folder_upload_state$files_processed, "processed,", 
          folder_upload_state$files_skipped, "skipped across", total_batches, "batches\n")
    }
    
    cat("Batch", batch_number + 1, "complete:", batch_files_processed, "processed,", 
        batch_files_skipped, "skipped\n")
  })
  
  # Remove selected files
  observeEvent(input$remove_selected_files, {
    req(input$global_file_list_rows_selected)
    
    current_files <- global_files()
    selected_indices <- input$global_file_list_rows_selected
    file_names <- names(current_files)
    
    if (length(selected_indices) > 0 && length(file_names) >= max(selected_indices)) {
      files_to_remove <- file_names[selected_indices]
      for (file_name in files_to_remove) {
        current_files[[file_name]] <- NULL
      }
      global_files(current_files)
      showNotification(paste("Removed", length(files_to_remove), "file(s)"), type = "message")
    }
  })
  
  # Clear all files
  observeEvent(input$clear_all_files, {
    global_files(list())
    showNotification("All files cleared", type = "message")
  })
  
  # File list display
  output$global_file_list <- DT::renderDT({
    current_files <- global_files()
    
    if (length(current_files) == 0) {
      data.frame(
        Name = character(0),
        Type = character(0),
        Size = character(0),
        Source = character(0),
        Original_Path = character(0),
        Uploaded = character(0)
      )
    } else {
      file_df <- data.frame(
        Name = names(current_files),
        Type = sapply(current_files, function(x) x$type),
        Size = sapply(current_files, function(x) {
          size_mb <- round(x$size / 1024^2, 2)
          paste(size_mb, "MB")
        }),
        Source = sapply(current_files, function(x) {
          if (!is.null(x$upload_type)) {
            if (x$upload_type == "folder") "📁 Folder" else "📄 Individual"
          } else {
            "📄 Individual"  # Default for existing files
          }
        }),
        Original_Path = sapply(current_files, function(x) {
          if (!is.null(x$original_path)) {
            x$original_path
          } else {
            x$name  # Fallback to file name
          }
        }),
        Uploaded = sapply(current_files, function(x) {
          format(x$uploaded, "%Y-%m-%d %H:%M:%S")
        }),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(
        file_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          columnDefs = list(
            list(width = '200px', targets = 0),  # Name column
            list(width = '300px', targets = 4)   # Original_Path column
          )
        ),
        selection = "multiple",
        rownames = FALSE
      )
    }
  })
  
  # File statistics
  output$file_stats <- renderText({
    current_files <- global_files()
    
    if (length(current_files) == 0) {
      "No files uploaded"
    } else {
      total_size <- sum(sapply(current_files, function(x) x$size))
      total_size_mb <- round(total_size / 1024^2, 2)
      
      file_types <- table(sapply(current_files, function(x) x$type))
      type_summary <- paste(names(file_types), ":", file_types, collapse = ", ")
      
      paste(
        "Total files:", length(current_files), "\n",
        "Total size:", total_size_mb, "MB\n",
        "File types:", type_summary
      )
    }
  })

  # Dynamic Plotter Management
  plotter_instances <- reactiveVal(list())
  plotter_counter <- reactiveVal(0)

  # Dynamic Data Import Management
  importer_instances <- reactiveVal(list())
  importer_counter <- reactiveVal(0)
  
  # Store custom tab names for importers
  importer_tab_names <- reactiveVal(list())
  # Also store current displayed titles to help with repeated renames
  importer_current_titles <- reactiveVal(list())

  # Data Import and Combination Logic
  # Create reactive list of data frames from all active importers
  list_of_df_reactives_for_combiner <- reactive({
    current_importers <- importer_instances()
    if (length(current_importers) == 0) {
      return(list())
    }
    
    # Extract the 'df' reactive from each importer instance
    df_reactives <- lapply(current_importers, function(instance) {
      if (!is.null(instance) && "df" %in% names(instance)) {
        return(instance$df)
      }
      return(NULL)
    })
    
    # Filter out any NULL values
    Filter(Negate(is.null), df_reactives)
  })
  
  data_combiner <- server_data_combiner(
    "combiner", 
    list_of_df_reactives_for_combiner
  )

  # Dynamic Data Import Creation
  observeEvent(input$insert_importer, {
    current_count <- isolate(importer_counter()) + 1
    importer_counter(current_count)
    import_id <- paste0("data_import_module_", current_count)

    # Check if there's a custom name for this tab
    current_names <- importer_tab_names()
    tab_title <- if (!is.null(current_names[[import_id]])) {
      current_names[[import_id]]
    } else {
      paste("Import", current_count)
    }

    nav_insert(
      id = "mainmenu",
      target = "Combined Data",
      position = "before",
      nav = nav_panel(
        title = tab_title,
        ui_data_importer(import_id)
      ),
      session = session
    )
    
    # Call the server module for the new importer, passing global files
    importer_module_output <- server_data_import(import_id, global_files)
    
    # Store the module's output
    current_instances <- importer_instances()
    current_instances[[import_id]] <- importer_module_output
    importer_instances(current_instances)
    
    # Observe tab rename trigger from this importer module
    observeEvent(importer_module_output$tab_rename_trigger(), {
      req(importer_module_output$tab_rename_trigger())
      
      rename_data <- importer_module_output$tab_rename_trigger()
      cat("Manual importer tab rename triggered:", rename_data$newName, "\n")
      
      # Send message to JavaScript to update the tab
      session$sendCustomMessage("updateNavTabTitle", list(
        moduleId = rename_data$moduleId,
        newTitle = rename_data$newName,
        oldTitle = rename_data$currentTitle
      ))
    })
    
    showNotification(
      paste("Added data import tab:", current_count), 
      type = "message"
    )
  })

  # Initialize with one default data import tab
  observeEvent(session$clientData, {
    if (isolate(importer_counter()) == 0) {
      # Trigger the insert_importer logic programmatically
      current_count <- 1
      importer_counter(current_count)
      import_id <- paste0("data_import_module_", current_count)

      # Check for custom name
      current_names <- importer_tab_names()
      tab_title <- if (!is.null(current_names[[import_id]])) {
        current_names[[import_id]]
      } else {
        paste("Import", current_count)
      }

      nav_insert(
        id = "mainmenu",
        target = "Combined Data",
        position = "before",
        nav = nav_panel(
          title = tab_title,
          ui_data_importer(import_id)
        ),
        session = session
      )
      
      # Call the server module for the initial importer, passing global files
      importer_module_output <- server_data_import(import_id, global_files)
      
      # Store the module's output
      current_instances <- list()
      current_instances[[import_id]] <- importer_module_output
      importer_instances(current_instances)
      
      # Observe tab rename trigger from this default importer module
      observeEvent(importer_module_output$tab_rename_trigger(), {
        req(importer_module_output$tab_rename_trigger())
        
        rename_data <- importer_module_output$tab_rename_trigger()
        cat("Default importer tab rename triggered:", rename_data$newName, "\n")
        
        # Send message to JavaScript to update the tab
        session$sendCustomMessage("updateNavTabTitle", list(
          moduleId = rename_data$moduleId,
          newTitle = rename_data$newName,
          oldTitle = rename_data$currentTitle
        ))
      })
    }
  }, once = TRUE)

  # Dynamic Plotter Creation
  observeEvent(input$insert_plot, {
    current_count <- isolate(plotter_counter()) + 1
    plotter_counter(current_count)
    plot_id <- paste0("plotter_", current_count)

    nav_insert(
      id = "mainmenu",
      target = "Analysis",
      position = "before",
      nav = nav_panel(
        title = plot_id,
        ui_plotter(plot_id)
      ),
      session = session
    )
    
    # Call the server module for the new plotter
    plotter_module_output <- server_plotter(plot_id, data_combiner$df, input)
    
    # Store the module's output
    current_instances <- plotter_instances()
    current_instances[[plot_id]] <- plotter_module_output
    plotter_instances(current_instances)
    
    # Observe plot rename trigger from this plotter module
    observeEvent(plotter_module_output$plot_rename_trigger(), {
      req(plotter_module_output$plot_rename_trigger())
      
      rename_data <- plotter_module_output$plot_rename_trigger()
      cat("Manual plotter tab rename triggered:", rename_data$newName, "\n")
      
      # Send message to JavaScript to update the tab
      session$sendCustomMessage("updateNavTabTitle", list(
        moduleId = rename_data$moduleId,
        newTitle = rename_data$newName,
        oldTitle = rename_data$currentTitle
      ))
    })
    
    showNotification(paste("Added plotter tab:", plot_id), type = "message")
  })

  # Helper & Downloader R Code Execution
  ace_server_functions("helper_input")
  observeEvent(input$helper_input, {
    output$helper_output <- renderUI({
      spsComps::shinyCatch({
        eval(
          parse(text = input$helper_input), 
          envir = new.env(parent = globalenv())
        )
      })
    })
  }, ignoreInit = FALSE, ignoreNULL = FALSE)

  ace_server_functions("downloader_input")
  observeEvent(input$downloader_input, {
    output$downloader_output <- renderUI({
      spsComps::shinyCatch({
        # Prepare the list of plot reactives for the downloader code
        plots_to_download_map <- lapply(plotter_instances(), function(instance) {
          if (!is.null(instance) && "plot" %in% names(instance) && 
              is.function(instance$plot)) {
            return(instance$plot)
          }
          return(NULL)
        })
        plots_to_download_map <- Filter(Negate(is.null), plots_to_download_map)

        # Environment for downloader code
        downloader_env <- new.env(parent = globalenv())
        downloader_env$dynamic_plots_map <- plots_to_download_map

        eval(parse(text = input$downloader_input), envir = downloader_env)
      })
    })
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
}

# Shiny App Definition
shinyApp(ui = ui, server = server) 