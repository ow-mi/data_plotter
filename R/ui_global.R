ui_global <- function() {





page_navbar(
  theme = bs_theme(bootswatch = "minty", version = 5),
  title = "Data Plotter v5",
  id = "mainmenu",

  # Input Data Management Tab
  nav_panel(
    title = "Input Files",
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

          # Batch download controls
          div(class = "mb-3",
            h6("Quick Batch Download"),
            p(class = "text-muted small", "Download all plots as a ZIP archive with current settings"),
            actionButton(
              "quick_batch_download",
              "Download All Plots (ZIP)",
              icon = icon("file-archive"),
              class = "btn-primary w-100 mb-2"
            ),
            downloadButton("batch_download_output", "Hidden Download", class = "d-none")
          ),

          hr(),

          # Advanced configuration
          h6("Advanced Configuration"),
          aceEditor_pre("downloader_input", value = downloader_code_refactored)
        ),
        div(class = "p-3",
          h5("Download Interface"),

          # Progress indicator for batch downloads
          uiOutput("batch_download_progress"),

          # Existing downloader output
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
  
  # Automation Menu
  nav_menu(
    title = "Automation",
    icon = icon("play-circle"),
    align = "right",
    nav_item(
      div(class = "px-3 py-2",
        h6("Quick Actions", class = "text-muted mb-2"),
        p(class = "text-muted small mb-3", "Fast way to regenerate processing and plots with new data"),
        
        actionButton(
          "automation_upload_folder",
          "Upload Folder",
          icon = icon("folder-open"),
          class = "btn-primary w-100 mb-2",
          style = "font-weight: 500;"
        ) |> tooltip("Trigger folder upload (same as Input Files tab)"),
        
        actionButton(
          "automation_process_data",
          "Process All Data",
          icon = icon("cogs"),
          class = "btn-warning w-100 mb-2",
          style = "font-weight: 500;"
        ) |> tooltip("Process all files in all importer tabs"),
        
        actionButton(
          "automation_generate_plots",
          "Generate All Plots",
          icon = icon("chart-line"),
          class = "btn-success w-100 mb-2",
          style = "font-weight: 500;"
        ) |> tooltip("Process data and create plots in all plotter tabs"),
        
        actionButton(
          "automation_download_plots",
          "Download All Plots",
          icon = icon("download"),
          class = "btn-info w-100 mb-3",
          style = "font-weight: 500;"
        ) |> tooltip("Download plots from all plotter tabs with their current settings"),
        
        # Runs Section
        h6("Batch Runs", class = "text-muted mb-2 mt-2"),
        p(class = "text-muted small mb-2", "Run multiple filter combinations automatically"),

        textInput(
          "automation_filters",
          "Filters (runs)",
          placeholder = "run:1;run:2;series:temp",
          width = "100%"
        ) |> tooltip("Semicolon-separated list of filters. Each will be applied sequentially with plot generation and download."),
        
        actionButton(
          "automation_run_batches",
          "Run All Batches",
          icon = icon("play-circle"),
          class = "btn-danger w-100",
          style = "font-weight: 500;"
        ) |> tooltip("Execute multiple filter combinations: apply filter → generate plots → download → repeat")
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
  ),
  
  # Add JavaScript and CSS
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
      
      // Also ensure plot output containers get proper height
      const plotOutputSelectors = [
        '.uiOutput',
        '.shiny-html-output',
        '.dataTables_wrapper',
        '.plotly'
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
      
      // Apply heights to plot output containers
      plotOutputSelectors.forEach(selector => {
        const elements = document.querySelectorAll(selector);
        elements.forEach(el => {
          // Only apply to elements within cards, not nested ones
          if (el.closest('.card') && !el.closest('.tab-pane')) {
            el.style.height = '100%';
            el.style.minHeight = '400px';
          }
        });
      });
      
      // Also resize plot outputs specifically
      setTimeout(resizePlotOutputs, 100);
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
    
    // Function to resize plot outputs specifically
    function resizePlotOutputs() {
      const plotOutputs = document.querySelectorAll('.uiOutput, .shiny-html-output, .dataTables_wrapper');
      plotOutputs.forEach(output => {
        if (output.closest('.card')) {
          const card = output.closest('.card');
          const cardHeight = card.offsetHeight;
          const headerHeight = card.querySelector('.card-header')?.offsetHeight || 0;
          const bodyPadding = 20; // Approximate padding
          const availableHeight = cardHeight - headerHeight - bodyPadding;
          
          if (availableHeight > 300) {
            output.style.height = availableHeight + 'px';
            output.style.minHeight = '400px';
          }
        }
      });
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
        // Also resize plot outputs after a short delay
        setTimeout(resizePlotOutputs, 200);
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
    
    // Handle automation: trigger folder upload
    Shiny.addCustomMessageHandler('triggerFolderUpload', function(data) {
      console.log('Automation: Triggering folder upload');
      var folderInput = document.getElementById('global_folder_upload');
      if (folderInput) {
        // Programmatically click the folder input
        folderInput.click();
        console.log('Folder upload dialog opened via automation');
      } else {
        console.error('Folder upload input not found');
      }
    });
    
    // Handle automation: trigger button click
    Shiny.addCustomMessageHandler('triggerButton', function(data) {
      console.log('Automation: Triggering button click for ID:', data.buttonId);
      var button = document.getElementById(data.buttonId);
      if (button) {
        // Programmatically click the button
        button.click();
        console.log('Button clicked via automation:', data.buttonId);
      } else {
        console.error('Button not found with ID:', data.buttonId);
        // Try alternative selectors
        var altButton = document.querySelector('[data-value="' + data.buttonId + '"]');
        if (altButton) {
          altButton.click();
          console.log('Button clicked via alternative selector:', data.buttonId);
        } else {
          console.error('Button not found with any selector for ID:', data.buttonId);
        }
      }
    });
    
    // Handle automation: trigger button click with delay
    Shiny.addCustomMessageHandler('triggerButtonDelayed', function(data) {
      console.log('Automation: Scheduling delayed button click for ID:', data.buttonId, 'delay:', data.delay + 'ms');
      
      setTimeout(function() {
        console.log('Automation: Executing delayed button click for ID:', data.buttonId);
        var button = document.getElementById(data.buttonId);
        if (button) {
          button.click();
          console.log('Delayed button clicked via automation:', data.buttonId);
        } else {
          console.error('Delayed button not found with ID:', data.buttonId);
          // Try alternative selectors
          var altButton = document.querySelector('[data-value="' + data.buttonId + '"]');
          if (altButton) {
            altButton.click();
            console.log('Delayed button clicked via alternative selector:', data.buttonId);
          } else {
            console.error('Delayed button not found with any selector for ID:', data.buttonId);
          }
        }
      }, data.delay);
    });
    
    // Handle automation: trigger download
    Shiny.addCustomMessageHandler('triggerDownload', function(data) {
      console.log('Automation: Triggering download for plotter:', data.plotterName, 'button ID:', data.buttonId);
      
      // Add a small delay to spread out downloads and prevent browser blocking
      setTimeout(function() {
        var downloadButton = document.getElementById(data.buttonId);
        if (downloadButton) {
          downloadButton.click();
          console.log('Download triggered via automation for:', data.plotterName);
        } else {
          console.error('Download button not found with ID:', data.buttonId);
          // Try alternative approach - look for download buttons in the plotter
          var plotterSection = document.querySelector('[id*="' + data.plotterName + '"]');
          if (plotterSection) {
            var downloadBtn = plotterSection.querySelector('a[download], .btn[id*="download"]');
            if (downloadBtn) {
              downloadBtn.click();
              console.log('Download triggered via alternative selector for:', data.plotterName);
            } else {
              console.error('No download button found in plotter section for:', data.plotterName);
            }
          }
        }
      }, Math.random() * 2000 + 500); // Random delay between 0.5-2.5 seconds to prevent browser blocking
    });
    
    // Handle automation: batch runs
    Shiny.addCustomMessageHandler('triggerBatchRun', function(data) {
      console.log('Automation: Batch run', data.runNumber, 'phase:', data.phase);

      if (data.phase === 'generate_plots') {
        // Trigger generate all plots (now called after combiner processing is complete)
        console.log('Batch run', data.runNumber, '- combiner processing complete, triggering plot generation');

        // Small delay to ensure combiner data has propagated to plotters
        setTimeout(function() {
          Shiny.setInputValue('automation_generate_plots', Math.random());
          console.log('Batch run', data.runNumber, '- plot generation triggered');

          // Schedule download after plot generation
          setTimeout(function() {
            console.log('Batch run', data.runNumber, '- scheduling download after plot generation');
            Shiny.setInputValue('automation_download_plots', Math.random());
            console.log('Batch run', data.runNumber, '- download triggered');

            // Schedule next batch run after download (no long timeout)
            setTimeout(function() {
              console.log('=== SENDING COMPLETION SIGNAL ===');
              console.log('Batch run', data.runNumber, '- download should be complete, proceeding to next run');
              console.log('Sending completion signal for run:', data.runNumber);

              // Trigger next run by sending input value that JavaScript will handle
              var completionData = {
                completedRun: data.runNumber,
                timestamp: Date.now()
              };

              console.log('Sending completion data:', completionData);
              Shiny.setInputValue('batch_run_complete', completionData);

              console.log('Completion signal sent via setInputValue');

              // Fallback: If input event doesn't work, trigger next run after longer delay
              setTimeout(function() {
                console.log('=== FALLBACK: Checking if batch_run_complete event was handled ===');
                // The input event should have been handled by now, but if not, we can add logic here
              }, 10000); // 10 second fallback check
            }, 3000); // 3 seconds for download completion

          }, 6000); // 6 seconds for plot generation
        }, 1000); // 1 second delay to let reactive updates propagate

      } else if (data.phase === 'download') {
        // Trigger download all plots
        Shiny.setInputValue('automation_download_plots', Math.random());
        console.log('Batch run', data.runNumber, '- plots generated and downloaded');
      }
    });

    // Handle new batch download system
    Shiny.addCustomMessageHandler('triggerBatchDownload', function(data) {
      console.log('New batch download: Triggering ZIP download for', data.plotter_count, 'plotters');

      // Click the hidden download button to start the ZIP download
      var downloadButton = document.getElementById('batch_download_output');
      if (downloadButton) {
        downloadButton.click();
        console.log('ZIP download triggered');
      } else {
        console.error('Batch download button not found');
      }
    });

    // Handle automation: delayed batch run
    Shiny.addCustomMessageHandler('triggerBatchRunDelayed', function(data) {
      console.log('Automation: Scheduling delayed batch run', data.runNumber, 'phase:', data.phase, 'delay:', data.delay + 'ms');
      
      setTimeout(function() {
        if (data.phase === 'download') {
          Shiny.setInputValue('automation_download_plots', Math.random());
          console.log('Batch run', data.runNumber, '- delayed download triggered');
        }
      }, data.delay);
    });
    
    // Handle automation: update combiner filters
    Shiny.addCustomMessageHandler('updateCombinerFilters', function(data) {
      console.log('Automation: Updating combiner filters for run', data.runNumber);
      console.log('Filter:', data.filterString);

      // Find and update the combiner custom filter input (combiner uses single filter input)
      var customFilterInput = document.getElementById('combiner-custom_filter');

      if (customFilterInput) {
        // Use the filter string directly without any formatting
        customFilterInput.value = data.filterString || '';
        $(customFilterInput).trigger('change'); // Trigger Shiny update
        console.log('Updated custom filter to:', data.filterString);
      } else {
        console.error('Custom filter input not found: combiner-custom_filter');
      }

      // Apply the filters by clicking the apply button
      setTimeout(function() {
        var applyButton = document.getElementById('combiner-apply_combiner_filters');
        if (applyButton) {
          applyButton.click();
          console.log('Applied combiner filters for run', data.runNumber);
        } else {
          console.error('Apply filters button not found: combiner-apply_combiner_filters');
        }
      }, 200); // Small delay to ensure input values are updated
    });

    // Handle automation: start batch runs with event-driven scheduling
    Shiny.addCustomMessageHandler('startBatchRuns', function(data) {
      console.log('Automation: Starting batch runs with', data.maxFilters, 'filter combinations');

      var currentRun = 0;
      var batchRunsCompleted = 0;

      // Set up completion handler for input changes
      console.log('Setting up batch_run_complete input handler...');

      // Listen for changes to the batch_run_complete input
      $(document).on('shiny:inputchanged', function(event) {
        // Only log batch_run_complete events to reduce noise
        if (event.name === 'batch_run_complete') {
          console.log('=== RECEIVED COMPLETION SIGNAL VIA INPUT ===');
          console.log('Completed run:', event.value ? event.value.completedRun : 'undefined');
          console.log('Current batch index before:', currentBatchIndex);
          console.log('Event value object:', event.value);

          if (event.value && event.value.completedRun) {
            // Proceed to next batch run (completedRun is the run number, so next is completedRun)
            currentBatchIndex = event.value.completedRun;
            console.log('Updated batch index to:', currentBatchIndex);
            console.log('Calling executeBatchRun with index:', currentBatchIndex);

            try {
              executeBatchRun(currentBatchIndex);
              console.log('executeBatchRun called successfully');
            } catch (error) {
              console.error('Error calling executeBatchRun:', error);
            }
          } else {
            console.error('Invalid completion signal - missing completedRun');
          }
        }
      });

      console.log('batch_run_complete input handler set up');

      function executeBatchRun(runIndex) {
        console.log('=== EXECUTEBATCHRUN CALLED ===');
        console.log('runIndex:', runIndex, 'maxFilters:', data.maxFilters);

        if (runIndex >= data.maxFilters) {
          // All runs complete - restore original filters
          console.log('All batch runs completed. Restoring original filters.');

          // Restore original filters
          var customFilterInput = document.getElementById('combiner-custom_filter');
          if (customFilterInput) {
            customFilterInput.value = data.originalCustomFilter || '';
            $(customFilterInput).trigger('change');

            // Apply the restored filters
            setTimeout(function() {
              var applyButton = document.getElementById('combiner-apply_combiner_filters');
              if (applyButton) {
                applyButton.click();
                console.log('Original filters restored and applied.');
              }
            }, 200);
          }
          console.log('All batch runs completed - exiting executeBatchRun');
          return;
        }

        var filterString = data.filterList[runIndex];
        var runNumber = runIndex + 1;

        console.log('=== STARTING NEW BATCH RUN ===');
        console.log('runIndex:', runIndex, 'runNumber:', runNumber);
        console.log('Filter:', filterString);

        // Apply filters using updateCombinerFilters message
        // The system will now wait for combiner processing to complete before proceeding
        Shiny.setInputValue('updateFiltersForBatch', {
          filterString: filterString,
          runNumber: runNumber,
          timestamp: Date.now()
        });

        // The next run will be triggered when download completes
      }

      // Track current batch run index
      var currentBatchIndex = 0;

      // Start the first batch run
      executeBatchRun(0);
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
            var batchSize = 2; // Small batch size for stable network performance
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
                
                // Add delay to prevent overwhelming the server
                setTimeout(function() {
                  // Continue processing after delay
                }, 500); // 500ms delay between batches
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
      height: 100% !important;
      max-height: 100% !important;
      overflow: auto !important;
    }
    
    .plotly,
    .shiny-plot-output {
      height: 100% !important;
      max-height: 100% !important;
      overflow: auto !important;
    }
    
    /* Ensure plot output containers fill their cards */
    .card-body .uiOutput,
    .card-body .shiny-html-output {
      height: 100% !important;
      min-height: 400px !important;
    }
    
    /* Specific styling for DT tables to fill containers */
    .dataTables_wrapper .dataTable {
      width: 100% !important;
    }
    
    /* Ensure ggplot outputs fill their containers */
    .shiny-plot-output img {
      max-width: 100% !important;
      height: auto !important;
    }
    
    /* Ensure card bodies take full height */
    .card-body {
      display: flex !important;
      flex-direction: column !important;
      height: 100% !important;
    }
    
    /* Ensure plot output cards fill available space */
    .card[full_screen='true'] {
      height: 100% !important;
      display: flex !important;
      flex-direction: column !important;
    }
    
    .card[full_screen='true'] .card-body {
      flex: 1 !important;
      min-height: 0 !important;
    }
    
    /* Ensure uiOutput elements fill their containers */
    .uiOutput {
      height: 100% !important;
      min-height: 400px !important;
    }
    
    /* Specific styling for plotter output cards */
    .module-container .card[full_screen='true'] {
      height: 100% !important;
      display: flex !important;
      flex-direction: column !important;
    }
    
    .module-container .card[full_screen='true'] .card-body {
      flex: 1 !important;
      min-height: 0 !important;
      display: flex !important;
      flex-direction: column !important;
    }
    
    .module-container .card[full_screen='true'] .uiOutput {
      flex: 1 !important;
      min-height: 0 !important;
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
    
    /* Floating Create Plot button styling */
    .module-container .btn-lg {
      font-weight: 600;
      border-radius: 25px;
      padding: 12px 24px;
      transition: all 0.3s ease;
    }
    
    .module-container .btn-lg:hover {
      transform: translateY(-2px);
      box-shadow: 0 6px 12px rgba(0,0,0,0.3);
    }
  "))
)

}