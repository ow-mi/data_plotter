# PowerShell script for Windows users
# Data Plotter Compilation & Server Script

Write-Host "Data Plotter Compilation & Server Script" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Green

# Option 1: Fix packages first
Write-Host "Option 1: Fixing package versions first..." -ForegroundColor Yellow
try {
    Rscript fix_packages.R
    Write-Host "Package fixing completed" -ForegroundColor Green
} catch {
    Write-Host "Package fixing failed: $($_.Exception.Message)" -ForegroundColor Red
}

# Option 2: Try the enhanced edit script
Write-Host "Option 2: Attempting enhanced export..." -ForegroundColor Yellow
$exportSuccess = $false

try {
    Rscript edit.R
    $exportSuccess = $true
    Write-Host "Enhanced export successful!" -ForegroundColor Green
} catch {
    Write-Host "Enhanced export failed, trying simple export..." -ForegroundColor Yellow
    
    # Option 3: Fallback to simple export
    try {
        Rscript edit_simple.R
        $exportSuccess = $true
        Write-Host "Simple export successful!" -ForegroundColor Green
    } catch {
        Write-Host "Simple export also failed: $($_.Exception.Message)" -ForegroundColor Red
    }
}

# Check if export was successful
if (Test-Path "shinylive_export") {
    Write-Host "Export directory found. Starting server..." -ForegroundColor Green
    
    # Check if http-server is installed
    $httpServerExists = Get-Command http-server -ErrorAction SilentlyContinue
    if ($httpServerExists) {
        Write-Host "Starting http-server on shinylive_export..." -ForegroundColor Green
        & http-server shinylive_export -s -p 8081
    } else {
        Write-Host "http-server not found. Trying Python server..." -ForegroundColor Yellow
        
        # Try Python server as alternative
        $pythonExists = Get-Command python -ErrorAction SilentlyContinue
        if ($pythonExists) {
            Write-Host "Starting Python HTTP server on port 8080..." -ForegroundColor Green
            Set-Location shinylive_export
            & python -m http.server 8081
        } else {
            Write-Host "Neither http-server nor Python found." -ForegroundColor Red
            Write-Host "Please install one of the following:" -ForegroundColor Yellow
            Write-Host "  - npm install -g http-server" -ForegroundColor White
            Write-Host "  - Install Python 3.x" -ForegroundColor White
            Write-Host "Or serve the files in shinylive_export/ with any static file server" -ForegroundColor White
        }
    }
} else {
    Write-Host "Export failed. shinylive_export directory not found." -ForegroundColor Red
    Write-Host "Try running the scripts manually:" -ForegroundColor Yellow
    Write-Host "  Rscript fix_packages.R" -ForegroundColor White
    Write-Host "  Rscript edit.R" -ForegroundColor White
} 