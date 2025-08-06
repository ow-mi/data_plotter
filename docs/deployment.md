# Deployment Guide

## Overview

This guide covers various deployment options for the Plotter App, from local development to production server deployment.

## Local Development Setup

### Prerequisites

1. **R Installation**: Version 4.0.0 or higher
2. **RStudio**: Recommended IDE (optional but helpful)
3. **System Requirements**:
   - Minimum 8GB RAM (16GB+ recommended for large datasets)
   - 2GB available disk space
   - Modern web browser (Chrome, Firefox, Safari, Edge)

### Package Installation

Run the following script to install all required packages:

```r
# Install required packages
required_packages <- c(
  # Core Shiny packages
  "shiny", "shinyTime", "bslib", "shinyAce", 
  "DT", "spsComps",
  
  # Data handling
  "stringr", "data.table", "lubridate", 
  "jsonlite", "fasttime", "readxl", "fst", "nanoparquet", 
  "tools", "purrr", "skimr",
  
  # Visualization
  "ggplot2", "plotly", "scattermore", "htmlwidgets",
  
  # Processing and utilities
  "future", "base64enc"
)

# Check which packages are already installed
installed_packages <- rownames(installed.packages())
packages_to_install <- required_packages[!required_packages %in% installed_packages]

# Install missing packages
if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, dependencies = TRUE)
  cat("Installed packages:", paste(packages_to_install, collapse = ", "), "\n")
} else {
  cat("All required packages are already installed.\n")
}

# Verify installation
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  cat("WARNING: The following packages failed to install:", paste(missing_packages, collapse = ", "), "\n")
} else {
  cat("All packages successfully installed!\n")
}
```

### Running Locally

1. **Download the application**:
   ```bash
   git clone <repository-url>
   cd plotter_app
   ```

2. **Set working directory** in R/RStudio:
   ```r
   setwd("path/to/plotter_app")
   ```

3. **Run the application**:
   ```r
   shiny::runApp("app.R")
   ```

4. **Access the app**: Open browser to `http://127.0.0.1:PORT` (port shown in console)

### Development Configuration

For development, you may want to adjust these settings in `app.R`:

```r
# Development settings
options(
  shiny.maxRequestSize = 500 * 1024^2,  # 500MB for testing
  shiny.error = browser,                # Debug on error
  shiny.trace = TRUE,                   # Trace reactive execution
  shiny.autoreload = TRUE              # Auto-reload on file changes
)
```

## Server Deployment

### Shiny Server Open Source

#### Installation

1. **Install R** on your server:
   ```bash
   # Ubuntu/Debian
   sudo apt update
   sudo apt install r-base r-base-dev
   
   # CentOS/RHEL
   sudo yum install R
   ```

2. **Install Shiny Server**:
   ```bash
   # Download and install Shiny Server
   wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.20.1002-amd64.deb
   sudo dpkg -i shiny-server-1.5.20.1002-amd64.deb
   ```

3. **Install required R packages** system-wide:
   ```bash
   sudo su - -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
   # Repeat for all required packages
   ```

#### Deployment

1. **Copy app files** to Shiny Server directory:
   ```bash
   sudo cp -R /path/to/plotter_app /srv/shiny-server/
   ```

2. **Set permissions**:
   ```bash
   sudo chown -R shiny:shiny /srv/shiny-server/plotter_app
   sudo chmod -R 755 /srv/shiny-server/plotter_app
   ```

3. **Configure Shiny Server** (`/etc/shiny-server/shiny-server.conf`):
   ```
   # Define a server
   server {
     listen 3838;
     
     # Define a location at the base URL
     location /plotter {
       site_dir /srv/shiny-server/plotter_app;
       log_dir /var/log/shiny-server;
       
       # Host the directory of Shiny Apps stored in this directory
       directory_index on;
     }
   }
   ```

4. **Restart Shiny Server**:
   ```bash
   sudo systemctl restart shiny-server
   ```

### RStudio Connect (Commercial)

1. **Prepare deployment bundle**:
   ```r
   library(rsconnect)
   
   # Create manifest
   rsconnect::writeManifest(".")
   
   # Deploy to RStudio Connect
   rsconnect::deployApp(
     appDir = ".",
     appName = "plotter-app",
     account = "your-account",
     server = "your-connect-server"
   )
   ```

2. **Configure resource limits** in RStudio Connect dashboard:
   - Memory: 4GB minimum, 8GB+ recommended
   - CPU: 2+ cores for parallel processing
   - Timeout: 300+ seconds for large file processing

### shinyapps.io

1. **Prepare for deployment**:
   ```r
   library(rsconnect)
   
   # Authenticate (first time only)
   rsconnect::setAccountInfo(
     name = "your-account",
     token = "your-token",
     secret = "your-secret"
   )
   
   # Deploy
   rsconnect::deployApp(
     appDir = ".",
     appName = "plotter-app"
   )
   ```

2. **Configuration considerations**:
   - Free tier: Limited memory and execution time
   - Paid tiers: Adjust instance size based on data requirements
   - File upload limits: May need to reduce `shiny.maxRequestSize`

## Docker Deployment

### Dockerfile

Create a `Dockerfile` for containerized deployment:

```dockerfile
FROM rocker/shiny:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinyTime', 'shinyjqui', 'bslib', 'shinyAce', 'shinyFiles', 'stringr', 'data.table', 'DT', 'R.utils', 'ggplot2', 'plotly', 'lubridate', 'jsonlite', 'fasttime', 'readxl', 'fst', 'nanoparquet', 'tools', 'scattermore', 'purrr', 'promises', 'future', 'rstudioapi', 'skimr', 'htmlwidgets', 'spsComps'), repos='https://cran.rstudio.com/')"

# Copy application files
COPY . /srv/shiny-server/plotter_app/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server/plotter_app

# Expose port
EXPOSE 3838

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]
```

### Docker Compose

For a complete deployment with volume mounts:

```yaml
version: '3.8'

services:
  plotter-app:
    build: .
    ports:
      - "3838:3838"
    volumes:
      - ./data:/srv/shiny-server/plotter_app/data
      - ./logs:/var/log/shiny-server
    environment:
      - SHINY_LOG_LEVEL=INFO
    restart: unless-stopped
    
  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
      - ./ssl:/etc/nginx/ssl
    depends_on:
      - plotter-app
    restart: unless-stopped
```

### Build and Run

```bash
# Build the Docker image
docker build -t plotter-app .

# Run the container
docker run -d -p 3838:3838 --name plotter-app plotter-app

# Or use Docker Compose
docker-compose up -d
```

## Production Configuration

### Performance Optimization

#### Memory Management

```r
# In app.R, configure for production use
options(
  shiny.maxRequestSize = 2000 * 1024^2,  # 2GB max file size
  future.globals.maxSize = 4 * 1024^3,   # 4GB for parallel processing
  DT.options = list(pageLength = 25)     # Reduce default table size
)

# Configure garbage collection
gc()  # Force garbage collection on app start
```

#### Parallel Processing

```r
# Configure future for parallel processing
library(future)
plan(multisession, workers = 4)  # Adjust workers based on CPU cores
```

### Security Configuration

#### File Upload Security

```r
# Restrict file types
accepted_extensions <- c(".csv", ".csv.gz", ".xlsx", ".fst", ".parquet")

# Validate file extensions in upload handlers
validate_file_extension <- function(filename) {
  ext <- tools::file_ext(tolower(filename))
  paste0(".", ext) %in% accepted_extensions
}
```

#### Environment Isolation

Ensure user code execution is properly sandboxed:

```r
# Create restricted environment
create_safe_env <- function() {
  env <- new.env(parent = baseenv())  # More restrictive parent
  
  # Add only necessary functions
  env$fread <- data.table::fread
  env$setDT <- data.table::setDT
  env$ggplot <- ggplot2::ggplot
  
  return(env)
}
```

### Monitoring and Logging

#### Application Logging

Add logging to critical functions:

```r
library(logger)

# Configure logging
log_threshold(INFO)
log_appender(appender_file("plotter_app.log"))

# Add to critical functions
log_info("Starting file processing for: {filename}")
log_error("Failed to process file: {filename}, Error: {error_message}")
```

#### Health Checks

Create a simple health check endpoint:

```r
# Add to server function
output$health_check <- renderText({
  paste("Status: OK, Time:", Sys.time())
})
```

### Reverse Proxy Configuration

#### Nginx Configuration

```nginx
server {
    listen 80;
    server_name your-domain.com;
    
    # Redirect HTTP to HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name your-domain.com;
    
    # SSL configuration
    ssl_certificate /etc/nginx/ssl/cert.pem;
    ssl_certificate_key /etc/nginx/ssl/key.pem;
    
    # Security headers
    add_header X-Frame-Options DENY;
    add_header X-Content-Type-Options nosniff;
    add_header X-XSS-Protection "1; mode=block";
    
    location / {
        proxy_pass http://localhost:3838;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_cache_bypass $http_upgrade;
        
        # Increase timeouts for large file uploads
        proxy_connect_timeout 300;
        proxy_send_timeout 300;
        proxy_read_timeout 300;
        
        # Increase max body size for file uploads
        client_max_body_size 2G;
    }
}
```

## Backup and Maintenance

### Data Backup

Create regular backups of uploaded data and configurations:

```bash
#!/bin/bash
# backup_script.sh

BACKUP_DIR="/path/to/backups"
APP_DIR="/srv/shiny-server/plotter_app"
DATE=$(date +%Y%m%d_%H%M%S)

# Create backup directory
mkdir -p "$BACKUP_DIR/$DATE"

# Backup application data
tar -czf "$BACKUP_DIR/$DATE/app_data.tar.gz" "$APP_DIR/data" "$APP_DIR/logs"

# Keep only last 30 days of backups
find "$BACKUP_DIR" -type d -mtime +30 -exec rm -rf {} \;
```

### Application Updates

1. **Backup current version**
2. **Test updates in staging environment**
3. **Deploy with minimal downtime**:

```bash
#!/bin/bash
# deploy_update.sh

# Stop services
sudo systemctl stop shiny-server

# Backup current version
sudo cp -R /srv/shiny-server/plotter_app /srv/shiny-server/plotter_app_backup

# Deploy new version
sudo cp -R /path/to/new/version/* /srv/shiny-server/plotter_app/

# Set permissions
sudo chown -R shiny:shiny /srv/shiny-server/plotter_app

# Start services
sudo systemctl start shiny-server

# Verify deployment
curl -f http://localhost:3838/plotter_app || {
  echo "Deployment failed, rolling back..."
  sudo rm -rf /srv/shiny-server/plotter_app
  sudo mv /srv/shiny-server/plotter_app_backup /srv/shiny-server/plotter_app
  sudo systemctl restart shiny-server
}
```

## Troubleshooting

### Common Deployment Issues

1. **Package Installation Failures**:
   - Install system dependencies: `libcurl4-openssl-dev`, `libssl-dev`, `libxml2-dev`
   - Use binary packages where available
   - Check CRAN mirrors and repository access

2. **Memory Issues**:
   - Monitor memory usage with `htop` or `free -h`
   - Adjust `future.globals.maxSize` and worker limits
   - Implement data chunking for large files

3. **Permission Problems**:
   - Ensure shiny user has read/write access to app directory
   - Check SELinux settings on RHEL/CentOS systems
   - Verify file ownership and permissions

4. **Network/Proxy Issues**:
   - Configure firewall rules for port 3838
   - Check reverse proxy configuration
   - Verify SSL certificate validity

### Performance Monitoring

Use these commands to monitor application performance:

```bash
# Monitor system resources
htop
iostat -x 1
df -h

# Monitor Shiny Server logs
tail -f /var/log/shiny-server/plotter_app-shiny-*.log

# Check network connections
netstat -tlnp | grep :3838

# Monitor Docker containers (if using Docker)
docker stats plotter-app
```

This deployment guide provides comprehensive instructions for various deployment scenarios. Choose the approach that best fits your infrastructure and requirements. 