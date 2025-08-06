# ShinyLive Export Guide

This guide explains how to export your dataPlotter package to a static web application using ShinyLive.

## What is ShinyLive?

ShinyLive allows you to run Shiny applications entirely in the browser without requiring a server. This makes it perfect for:

- **Sharing applications** with others who don't have R installed
- **Deploying to static hosting** services like GitHub Pages, Netlify, or Vercel
- **Creating standalone applications** that work offline
- **Reducing server costs** by running everything client-side

## Quick Start

### 1. Basic Export

```r
# Run the basic export script
source("shinylive_export.R")
```

This will:
- Export your app to `shinylive_export/app/`
- Create serve scripts for local testing
- Generate a README with instructions

### 2. Advanced Export

```r
# Run the advanced export script with more options
source("shinylive_export_advanced.R")
```

This includes:
- Comprehensive error handling
- Detailed logging
- Deployment configurations
- Performance optimizations

### 3. Custom Configuration

```r
# Load and modify configuration
source("shinylive_config.R")

# Modify settings
export_config$export_dir <- "my_custom_export"
export_config$port <- 9000
export_config$app_title <- "My Custom Data Plotter"

# Run export with custom config
source("shinylive_export_advanced.R")
```

## Configuration Options

### Basic Settings

| Setting | Description | Default |
|---------|-------------|---------|
| `export_dir` | Directory for exported files | `"shinylive_export"` |
| `subdir` | Subdirectory for app files | `"app"` |
| `port` | Port for local server | `8000` |
| `include_templates` | Load template files | `TRUE` |
| `verbose` | Show detailed output | `TRUE` |

### Deployment Settings

| Setting | Description | Default |
|---------|-------------|---------|
| `create_serve_scripts` | Create local server scripts | `TRUE` |
| `create_deployment_configs` | Create deployment configs | `TRUE` |
| `minify` | Minify output for production | `FALSE` |

### Performance Settings

| Setting | Description | Default |
|---------|-------------|---------|
| `enable_compression` | Enable gzip compression | `TRUE` |
| `enable_caching` | Enable browser caching | `TRUE` |
| `cache_duration` | Cache duration in seconds | `3600` |

## Export Process

### Step 1: Dependencies Check

The script checks for required packages:
- `shiny`
- `shinylive`

If missing, it provides installation instructions.

### Step 2: Package Loading

```r
# Load the package
devtools::load_all()

# Load templates (if enabled)
dataPlotter::load_templates_ordered()
```

### Step 3: App Creation

```r
# Create the Shiny app object
app <- shiny::shinyApp(
  ui = dataPlotter::ui_global(),
  server = dataPlotter::server_global
)
```

### Step 4: Export

```r
# Export to static files
shinylive::export(
  app,
  export_dir,
  subdir = "app"
)
```

### Step 5: Additional Files

The script creates:
- Serve scripts (`serve.sh`, `serve.bat`, `serve.js`)
- Deployment configs (`netlify.toml`, `vercel.json`)
- Documentation (`README.md`, `SUMMARY.md`)

## Local Testing

### Option 1: Python Server

```bash
cd shinylive_export
./serve.sh          # Unix/Linux/macOS
serve.bat           # Windows
```

### Option 2: Node.js Server

```bash
cd shinylive_export
node serve.js
```

### Option 3: Manual Server

```bash
cd shinylive_export
python -m http.server 8000
```

Then open: http://localhost:8000/app/

## Deployment Options

### GitHub Pages

1. Push the `app/` folder to a GitHub repository
2. Enable GitHub Pages in repository settings
3. Set source to `/docs` or `/` (root)

### Netlify

1. Drag and drop the `app/` folder to Netlify
2. Or connect your GitHub repository
3. The `netlify.toml` file will configure the deployment

### Vercel

1. Install Vercel CLI: `npm i -g vercel`
2. Run: `vercel` in the export directory
3. The `vercel.json` file will configure the deployment

### AWS S3

1. Create an S3 bucket
2. Upload the contents of the `app/` folder
3. Configure static website hosting
4. Set up CloudFront for HTTPS

## File Structure

```
shinylive_export/
├── app/                    # Static app files
│   ├── index.html         # Main entry point
│   ├── app.js            # Application logic
│   ├── app.css           # Styles
│   └── assets/           # Static assets
├── serve.sh              # Unix/Linux/macOS serve script
├── serve.bat             # Windows serve script
├── serve.js              # Node.js serve script
├── netlify.toml          # Netlify configuration
├── vercel.json           # Vercel configuration
├── README.md             # Documentation
└── SUMMARY.md            # Export summary
```

## Browser Compatibility

ShinyLive works in modern browsers that support:
- WebAssembly
- ES6 modules
- Fetch API
- Service Workers (optional)

**Supported browsers:**
- Chrome 67+
- Firefox 60+
- Safari 11.1+
- Edge 79+

## Performance Considerations

### Initial Load Time
- **First visit:** 5-10 seconds (depends on connection)
- **Subsequent visits:** 1-2 seconds (cached)

### File Sizes
- **Total size:** ~20-50MB
- **Memory usage:** ~50-100MB
- **Network transfer:** ~10-30MB (compressed)

### Optimization Tips

1. **Enable compression** in your web server
2. **Set appropriate cache headers** for static assets
3. **Use a CDN** for faster global delivery
4. **Consider lazy loading** for large applications

## Troubleshooting

### Common Issues

**Export fails with dependency errors:**
```r
# Install missing packages
install.packages(c("shiny", "shinylive"))
```

**App doesn't load in browser:**
- Check browser console for errors
- Ensure all files are present in `app/` directory
- Try a different browser

**Slow performance:**
- Close other browser tabs
- Restart the browser
- Check available memory

**Upload not working:**
- Check browser permissions
- Try a different browser
- Ensure files are not too large

### Debug Mode

Enable debug mode in the configuration:

```r
export_config$verbose <- TRUE
advanced_config$debug_mode <- TRUE
```

This will provide more detailed output during the export process.

## Customization

### Custom Styling

Add custom CSS to the exported app:

```r
export_config$custom_css <- "
.my-custom-class {
  background-color: #f0f0f0;
  padding: 10px;
}
"
```

### Custom JavaScript

Add custom JavaScript:

```r
export_config$custom_js <- "
console.log('Custom JavaScript loaded');
"
```

### Custom Favicon

```r
export_config$favicon <- "path/to/favicon.ico"
```

### Custom Title and Description

```r
export_config$app_title <- "My Custom Data Plotter"
export_config$app_description <- "A custom data visualization tool"
```

## Security Considerations

### Content Security Policy

The exported app includes a basic CSP:

```html
<meta http-equiv="Content-Security-Policy" content="default-src 'self'; script-src 'self' 'unsafe-inline' 'unsafe-eval'; style-src 'self' 'unsafe-inline';">
```

### File Upload Security

- Files are processed client-side
- No data is sent to external servers
- File type validation is enabled by default

### HTTPS

Enable HTTPS redirect for production:

```r
security_config$https_redirect <- TRUE
```

## Best Practices

### 1. Test Locally First

Always test the exported app locally before deployment:

```bash
cd shinylive_export
./serve.sh
```

### 2. Use Version Control

Commit your export configuration to version control:

```bash
git add shinylive_config.R
git commit -m "Add ShinyLive export configuration"
```

### 3. Optimize for Production

For production deployments:

```r
export_config$minify <- TRUE
export_config$verbose <- FALSE
performance_config$enable_compression <- TRUE
```

### 4. Monitor Performance

Use browser developer tools to monitor:
- Load times
- Memory usage
- Network requests

### 5. Regular Updates

Keep your export scripts updated with the latest ShinyLive features.

## Examples

### Minimal Export

```r
# Simple export with minimal configuration
export_config$export_dir <- "minimal_export"
export_config$create_serve_scripts <- FALSE
export_config$create_deployment_configs <- FALSE
source("shinylive_export.R")
```

### Production Export

```r
# Production-ready export
export_config$minify <- TRUE
export_config$verbose <- FALSE
export_config$app_title <- "Data Plotter Pro"
performance_config$enable_compression <- TRUE
security_config$https_redirect <- TRUE
source("shinylive_export_advanced.R")
```

### Custom Deployment

```r
# Custom deployment configuration
deployment_config$github_pages$enabled <- TRUE
deployment_config$github_pages$branch <- "main"
deployment_config$netlify$enabled <- FALSE
source("shinylive_export_advanced.R")
```

## Support

For issues with the export process:

1. Check the console output for error messages
2. Verify all dependencies are installed
3. Ensure the package loads correctly
4. Test with a minimal configuration first

The ShinyLive export creates a fully functional static web application that can be deployed anywhere static files are supported. 