# Docker Deployment Guide for dataPlotter

This guide explains how to deploy the dataPlotter application using Docker.

## 🐳 Prerequisites

### 1. Install Docker
- **Windows/macOS**: Install [Docker Desktop](https://www.docker.com/products/docker-desktop)
- **Linux**: Install Docker Engine and Docker Compose
  ```bash
  # Ubuntu/Debian
  sudo apt-get update
  sudo apt-get install docker.io docker-compose
  
  # Add user to docker group
  sudo usermod -aG docker $USER
  ```

### 2. Verify Installation
```bash
docker --version
docker-compose --version
```

## 🚀 Quick Start

### Option 1: Using Deployment Scripts

#### Linux/macOS:
```bash
chmod +x deploy_docker.sh
./deploy_docker.sh
```

#### Windows:
```cmd
deploy_docker.bat
```

### Option 2: Manual Deployment

1. **Build the image:**
   ```bash
   docker-compose build
   ```

2. **Run the application:**
   ```bash
   docker-compose up
   ```

3. **Access the application:**
   Open your browser to: http://localhost:3838

## 📁 Project Structure

```
dataPlotter/
├── Dockerfile              # Docker configuration
├── docker-compose.yml      # Docker Compose configuration
├── .dockerignore          # Files to exclude from Docker build
├── deploy_docker.sh       # Linux/macOS deployment script
├── deploy_docker.bat      # Windows deployment script
├── data/                  # Optional: Mounted data directory
└── docs/
    └── docker-deployment.md  # This guide
```

## 🔧 Configuration

### Dockerfile Details

The Dockerfile:
- Uses the official `rocker/shiny:latest` base image
- Installs system dependencies for R packages
- Installs required R packages (shiny, plotly, DT, etc.)
- Installs the dataPlotter package
- Exposes port 3838
- Creates a startup script

### Docker Compose Configuration

The `docker-compose.yml` includes:
- Port mapping: `3838:3838`
- Volume mounting for data persistence
- Health checks
- Automatic restart policy

## 📊 Usage

### Starting the Application

```bash
# Start in foreground
docker-compose up

# Start in background
docker-compose up -d

# View logs
docker-compose logs -f
```

### Stopping the Application

```bash
# Stop gracefully
docker-compose down

# Stop and remove volumes
docker-compose down -v
```

### Accessing the Application

- **URL**: http://localhost:3838
- **Default port**: 3838
- **Container name**: dataplotter-app

## 🔍 Troubleshooting

### Common Issues

1. **Port already in use:**
   ```bash
   # Check what's using port 3838
   lsof -i :3838
   
   # Change port in docker-compose.yml
   ports:
     - "8080:3838"  # Use port 8080 instead
   ```

2. **Permission denied:**
   ```bash
   # Add user to docker group
   sudo usermod -aG docker $USER
   # Log out and back in
   ```

3. **Build fails:**
   ```bash
   # Clean build
   docker-compose build --no-cache
   ```

4. **Container won't start:**
   ```bash
   # Check logs
   docker-compose logs dataplotter
   
   # Check container status
   docker-compose ps
   ```

### Debugging Commands

```bash
# Enter the container
docker-compose exec dataplotter bash

# Check R installation
docker-compose exec dataplotter R --version

# Check package installation
docker-compose exec dataplotter R -e "library(dataPlotter)"

# View application logs
docker-compose logs -f dataplotter
```

## 🔄 Development Workflow

### Making Changes

1. **Modify your code**
2. **Rebuild the image:**
   ```bash
   docker-compose build
   ```
3. **Restart the application:**
   ```bash
   docker-compose up
   ```

### Hot Reloading (Development)

For development with hot reloading, you can mount the source code:

```yaml
# In docker-compose.yml
volumes:
  - .:/app
  - ./data:/app/data:ro
```

## 🌐 Production Deployment

### Environment Variables

Set production environment variables:

```bash
# Create .env file
R_ENV=production
SHINY_HOST=0.0.0.0
SHINY_PORT=3838
```

### Security Considerations

1. **Use specific port mapping:**
   ```yaml
   ports:
     - "127.0.0.1:3838:3838"  # Only localhost access
   ```

2. **Add resource limits:**
   ```yaml
   deploy:
     resources:
       limits:
         memory: 2G
         cpus: '1.0'
   ```

3. **Use secrets for sensitive data:**
   ```yaml
   secrets:
     - db_password
   ```

### Reverse Proxy

For production, use a reverse proxy (nginx, Apache):

```nginx
server {
    listen 80;
    server_name your-domain.com;
    
    location / {
        proxy_pass http://localhost:3838;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

## 📈 Monitoring

### Health Checks

The container includes health checks:

```yaml
healthcheck:
  test: ["CMD", "curl", "-f", "http://localhost:3838"]
  interval: 30s
  timeout: 10s
  retries: 3
```

### Logging

View application logs:

```bash
# Follow logs
docker-compose logs -f

# View specific service logs
docker-compose logs dataplotter

# Export logs
docker-compose logs > app.log
```

## 🔧 Advanced Configuration

### Custom R Packages

Add custom R packages to the Dockerfile:

```dockerfile
# Install additional R packages
RUN R -e "install.packages(c('your-package'), repos='https://cran.rstudio.com/')"
```

### Data Persistence

Mount volumes for data persistence:

```yaml
volumes:
  - ./data:/app/data:ro
  - ./uploads:/app/uploads
  - ./logs:/app/logs
```

### Multiple Instances

Scale the application:

```bash
# Run multiple instances
docker-compose up --scale dataplotter=3
```

## 📚 Additional Resources

- [Docker Documentation](https://docs.docker.com/)
- [Docker Compose Documentation](https://docs.docker.com/compose/)
- [Rocker Shiny Image](https://hub.docker.com/r/rocker/shiny)
- [Shiny Server Documentation](https://docs.rstudio.com/shiny-server/)

## 🆘 Support

If you encounter issues:

1. Check the troubleshooting section above
2. Review Docker and application logs
3. Ensure all prerequisites are installed
4. Verify port availability
5. Check system resources

For additional help, refer to the main project documentation or create an issue in the project repository. 