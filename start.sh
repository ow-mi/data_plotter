#!/bin/bash

Rscript edit_simple.R
# Check if export was successful
if [ -d "shinylive_export" ]; then
    echo "Export directory found. Starting server..."
    
    # Check if http-server is installed
    if command -v http-server &> /dev/null; then
        echo "Starting http-server on shinylive_export..."
        http-server shinylive_export -s -p 8081
    else
        echo "http-server not found. Please install it with: npm install -g http-server"
        echo "Or serve the files in shinylive_export/ with any static file server"
        echo "You can also try: python -m http.server 8080 -d shinylive_export"
    fi
else
    echo "Export failed. shinylive_export directory not found."
    echo "You can try these options:"
    echo "  1. Run locally without shinylive: Rscript serve_local.R"
    echo "  2. Fix packages first: Rscript fix_packages.R"
    echo "  3. Try robust export: Rscript edit_robust.R"
    echo "  4. Try basic export: Rscript edit.R"
fi
