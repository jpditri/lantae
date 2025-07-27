#!/bin/bash
set -euo pipefail

# Lantae Application Setup Script
# Sets up the Lantae application, installs gems, and configures environment

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "${LOG_FILE:-install.log}"
}

show_progress() {
    echo -e "${BLUE}[APP] $1${NC}"
    log "APP: $1"
}

show_success() {
    echo -e "${GREEN}✓ [APP] $1${NC}"
    log "APP SUCCESS: $1"
}

show_warning() {
    echo -e "${YELLOW}⚠ [APP] $1${NC}"
    log "APP WARNING: $1"
}

error_exit() {
    echo -e "${RED}ERROR [APP]: $1${NC}" >&2
    log "APP ERROR: $1"
    exit 1
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Verify Ruby environment
verify_ruby_environment() {
    show_progress "Verifying Ruby environment..."
    
    if ! command_exists ruby; then
        error_exit "Ruby not found. Please ensure Ruby installation completed successfully."
    fi
    
    if ! command_exists bundle; then
        error_exit "Bundler not found. Please ensure Ruby installation completed successfully."
    fi
    
    local ruby_version
    ruby_version=$(ruby -v | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1)
    
    if [[ ! "$ruby_version" =~ ^3\. ]]; then
        error_exit "Ruby version $ruby_version does not meet requirement (3.0+)"
    fi
    
    show_success "Ruby environment verified (v$ruby_version)"
}

# Verify we're in the Lantae directory
verify_lantae_directory() {
    show_progress "Verifying Lantae project directory..."
    
    local current_dir
    current_dir=$(pwd)
    
    # Check for Lantae project files
    if [[ ! -f "Gemfile" ]]; then
        error_exit "Gemfile not found. Are you in the Lantae project directory?"
    fi
    
    if [[ ! -f "lantae" ]]; then
        error_exit "Lantae executable not found. Are you in the Lantae project directory?"
    fi
    
    if [[ ! -d "lib/ruby" ]]; then
        error_exit "lib/ruby directory not found. Are you in the Lantae project directory?"
    fi
    
    show_success "Lantae project directory verified: $current_dir"
}

# Install Ruby gems
install_gems() {
    show_progress "Installing Ruby gems..."
    
    # Check if Gemfile.lock exists and is recent
    if [[ -f "Gemfile.lock" ]]; then
        local gemfile_time gemfile_lock_time
        gemfile_time=$(stat -f "%m" Gemfile 2>/dev/null || stat -c "%Y" Gemfile 2>/dev/null || echo "0")
        gemfile_lock_time=$(stat -f "%m" Gemfile.lock 2>/dev/null || stat -c "%Y" Gemfile.lock 2>/dev/null || echo "0")
        
        if [[ "$gemfile_lock_time" -gt "$gemfile_time" ]]; then
            show_progress "Gemfile.lock is up to date, running bundle install..."
        else
            show_warning "Gemfile.lock is outdated, updating dependencies..."
        fi
    else
        show_progress "No Gemfile.lock found, installing fresh dependencies..."
    fi
    
    # Install gems with bundler
    if ! bundle check >/dev/null 2>&1; then
        show_progress "Installing missing gems..."
        
        # Set bundler configuration for better performance
        bundle config set --local deployment false
        bundle config set --local path vendor/bundle
        bundle config set --local jobs "$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)"
        
        # Install gems
        if ! bundle install; then
            show_warning "Bundle install failed, trying without path restriction..."
            bundle config unset path
            bundle install || error_exit "Failed to install Ruby gems"
        fi
    else
        show_success "All gems already installed"
    fi
    
    show_success "Ruby gems installation complete"
}

# Set file permissions
set_permissions() {
    show_progress "Setting file permissions..."
    
    # Make main executable
    chmod +x lantae || error_exit "Failed to make lantae executable"
    show_success "lantae executable permissions set"
    
    # Make bin directory executables
    if [[ -d "bin" ]]; then
        find bin -type f -name "lantae*" -exec chmod +x {} \; 2>/dev/null || true
        show_success "bin directory permissions set"
    fi
    
    # Make scripts executable
    if [[ -d "scripts" ]]; then
        find scripts -type f -name "*.sh" -exec chmod +x {} \; 2>/dev/null || true
        show_success "scripts directory permissions set"
    fi
    
    # Make any Ruby scripts executable
    find . -name "*.rb" -path "./lib/*" -exec chmod +r {} \; 2>/dev/null || true
}

# Create necessary directories
create_directories() {
    show_progress "Creating necessary directories..."
    
    # Create data directory for databases and logs
    mkdir -p data/logs
    mkdir -p data/tasks
    mkdir -p data/conversations
    
    # Create config directory for user configurations
    mkdir -p config
    
    # Create temporary directory
    mkdir -p tmp
    
    show_success "Directories created"
}

# Create basic configuration files
create_config_files() {
    show_progress "Creating basic configuration files..."
    
    # Create .env template if it doesn't exist
    if [[ ! -f ".env" ]]; then
        cat > .env << 'EOF'
# Lantae Environment Configuration
# Copy this file and customize for your setup

# API Keys (optional - can also use AWS Secrets Manager)
# OPENAI_API_KEY=your_openai_key_here
# ANTHROPIC_API_KEY=your_anthropic_key_here
# GEMINI_API_KEY=your_gemini_key_here
# MISTRAL_API_KEY=your_mistral_key_here
# PERPLEXITY_API_KEY=your_perplexity_key_here

# AWS Configuration (if using AWS Secrets Manager)
# AWS_PROFILE=your_profile
# AWS_REGION=us-east-1

# Ollama Configuration
# OLLAMA_HOST=http://localhost:11434

# Default Provider and Model
DEFAULT_PROVIDER=ollama
DEFAULT_MODEL=cogito:latest

# Logging
LOG_LEVEL=info
LOG_FILE=data/logs/lantae.log

# Database
DATABASE_PATH=data/lantae.db
EOF
        show_success ".env template created"
    else
        show_success ".env file already exists"
    fi
    
    # Create MCP servers configuration template
    if [[ ! -f "mcp_servers.yml" ]]; then
        cat > mcp_servers.yml << 'EOF'
# Lantae MCP (Model Context Protocol) Servers Configuration
# Configure external MCP servers for extended functionality

mcp_servers:
  # File system operations
  - name: filesystem
    transport: stdio
    command: npx
    args:
      - "@modelcontextprotocol/server-filesystem"
      - "."  # Current directory - adjust as needed
    description: "File system operations via MCP"
    enabled: true
    
  # Git operations
  - name: git
    transport: stdio
    command: npx
    args:
      - "@modelcontextprotocol/server-git"
    description: "Git operations via MCP"
    enabled: false
    
  # SQLite database operations
  - name: sqlite
    transport: stdio
    command: npx
    args:
      - "@modelcontextprotocol/server-sqlite"
      - "data/lantae.db"
    description: "SQLite database operations via MCP"
    enabled: false

# MCP Client Configuration
mcp_client:
  timeout: 30
  max_retries: 3
  debug: false
EOF
        show_success "mcp_servers.yml template created"
    else
        show_success "mcp_servers.yml already exists"
    fi
}

# Verify installation
verify_installation() {
    show_progress "Verifying application setup..."
    
    # Check that all required files exist
    local required_files=(
        "lantae"
        "Gemfile"
        "Gemfile.lock"
        "lib/ruby"
        ".env"
    )
    
    for file in "${required_files[@]}"; do
        if [[ ! -e "$file" ]]; then
            error_exit "Required file/directory missing: $file"
        fi
    done
    
    # Test that bundle can load the application
    show_progress "Testing gem dependencies..."
    if ! bundle exec ruby -e "require_relative 'lib/ruby/config/configuration'" >/dev/null 2>&1; then
        show_warning "Could not load configuration module (this may be normal)"
    fi
    
    # Check that lantae is executable
    if [[ ! -x "lantae" ]]; then
        error_exit "lantae is not executable"
    fi
    
    show_success "Application setup verified"
}

# Main execution
main() {
    show_progress "Setting up Lantae application..."
    
    verify_ruby_environment
    verify_lantae_directory
    install_gems
    set_permissions
    create_directories
    create_config_files
    verify_installation
    
    show_success "Lantae application setup complete"
    
    # Provide setup summary
    echo ""
    show_progress "Application setup summary:"
    echo "  • Project directory: $(pwd)"
    echo "  • Ruby gems: $(bundle list 2>/dev/null | wc -l | tr -d ' ') gems installed"
    echo "  • Configuration: .env and mcp_servers.yml templates created"
    echo "  • Executable: ./lantae ready to run"
    echo "  • Data directory: data/ created for databases and logs"
    echo ""
    show_warning "Next: Configure your preferred LLM providers"
}

main "$@"