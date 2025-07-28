#!/bin/bash
set -euo pipefail

# Raspberry Pi / ARM System Ruby Installation Script
# Uses system packages instead of compiling Ruby from source

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
LOG_FILE="${PROJECT_DIR}/install.log"

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "${LOG_FILE:-install.log}"
}

show_progress() {
    echo -e "${BLUE}[PI] $1${NC}"
    log "RASPBERRY_PI: $1"
}

show_success() {
    echo -e "${GREEN}✓ [PI] $1${NC}"
    log "RASPBERRY_PI SUCCESS: $1"
}

show_warning() {
    echo -e "${YELLOW}⚠ [PI] $1${NC}"
    log "RASPBERRY_PI WARNING: $1"
}

error_exit() {
    echo -e "${RED}ERROR [PI]: $1${NC}" >&2
    log "RASPBERRY_PI ERROR: $1"
    echo -e "\n${YELLOW}Troubleshooting tips:${NC}" >&2
    echo -e "${YELLOW}1. Run: sudo apt update && sudo apt upgrade${NC}" >&2
    echo -e "${YELLOW}2. Check available disk space: df -h${NC}" >&2
    echo -e "${YELLOW}3. Ensure internet connection is stable${NC}" >&2
    echo -e "${YELLOW}4. Try the manual installation steps in the README${NC}" >&2
    exit 1
}

# Check if running on ARM/Raspberry Pi
check_raspberry_pi() {
    show_progress "Checking system architecture..."
    
    if [[ ! -f /proc/cpuinfo ]]; then
        error_exit "Cannot detect system information"
    fi
    
    if ! grep -q "ARM\|arm\|aarch64" /proc/cpuinfo && [[ "$(uname -m)" != "aarch64" && "$(uname -m)" != "armv"* ]]; then
        show_warning "This script is optimized for Raspberry Pi/ARM systems"
        show_warning "Your system appears to be: $(uname -m)"
        read -p "Continue anyway? (y/N): " continue_choice
        if [[ "$continue_choice" != "y" && "$continue_choice" != "Y" ]]; then
            exit 0
        fi
    else
        show_success "ARM/Raspberry Pi architecture detected"
    fi
    
    # Check memory
    if [[ -f /proc/meminfo ]]; then
        local mem_gb
        mem_gb=$(awk '/MemTotal/ {printf "%.1f", $2/1024/1024}' /proc/meminfo)
        show_progress "Available memory: ${mem_gb}GB"
        
        if (( $(echo "$mem_gb < 1" | bc -l 2>/dev/null || echo "0") )); then
            show_warning "Low memory detected. Installation may be slow."
            show_warning "Consider adding swap space if installation fails."
        fi
    fi
}

# Update package lists
update_packages() {
    show_progress "Updating package lists..."
    if ! sudo apt update; then
        error_exit "Failed to update package lists"
    fi
    show_success "Package lists updated"
}

# Install system Ruby and dependencies
install_system_ruby() {
    show_progress "Installing Ruby via system packages..."
    
    local packages=(
        "ruby"
        "ruby-dev"
        "ruby-bundler"
        "build-essential"
        "git"
        "curl"
        "libssl-dev"
        "libreadline-dev"
        "zlib1g-dev"
        "libyaml-dev"
        "libffi-dev"
    )
    
    # Check which packages need to be installed
    local to_install=()
    for pkg in "${packages[@]}"; do
        if ! dpkg -l | grep -q "^ii  $pkg "; then
            to_install+=("$pkg")
        fi
    done
    
    if [[ ${#to_install[@]} -gt 0 ]]; then
        show_progress "Installing packages: ${to_install[*]}"
        if ! sudo apt install -y "${to_install[@]}"; then
            error_exit "Failed to install required packages"
        fi
    else
        show_success "All required packages already installed"
    fi
    
    # Verify Ruby installation
    if ! command -v ruby >/dev/null 2>&1; then
        error_exit "Ruby installation failed"
    fi
    
    local ruby_version
    ruby_version=$(ruby -v | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1)
    show_success "Ruby $ruby_version installed"
    
    # Check if it's Ruby 2.x and warn
    if [[ "$ruby_version" =~ ^2\. ]]; then
        show_warning "System Ruby is version 2.x"
        show_warning "Some features may require Ruby 3.x"
        show_warning "Consider upgrading your Raspberry Pi OS to get newer Ruby"
    fi
}

# Setup Lantae for system Ruby
setup_lantae() {
    show_progress "Setting up Lantae application..."
    
    cd "$PROJECT_DIR" || error_exit "Cannot access project directory"
    
    # Install bundler if not present
    if ! gem list bundler | grep -q bundler; then
        show_progress "Installing bundler..."
        if ! sudo gem install bundler --no-document; then
            error_exit "Failed to install bundler"
        fi
    fi
    
    # Create a Gemfile.lock if it doesn't exist
    if [[ ! -f Gemfile.lock ]]; then
        show_progress "Creating Gemfile.lock..."
        bundle lock --add-platform arm-linux || true
        bundle lock --add-platform aarch64-linux || true
    fi
    
    # Install gems with system Ruby
    show_progress "Installing Ruby gems (this may take a while)..."
    
    # For system Ruby, we might need sudo for gem installation
    if ! bundle install --path vendor/bundle; then
        show_warning "Bundle install failed, trying with sudo..."
        if ! sudo bundle install; then
            error_exit "Failed to install Ruby gems"
        fi
    fi
    
    show_success "Lantae application set up"
}

# Create wrapper script for system Ruby
create_wrapper() {
    show_progress "Creating Lantae wrapper script..."
    
    cat > "$PROJECT_DIR/lantae-pi" << 'EOF'
#!/bin/bash
# Lantae wrapper for Raspberry Pi with system Ruby

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Use bundler to ensure gems are loaded correctly
if [[ -d "$SCRIPT_DIR/vendor/bundle" ]]; then
    export BUNDLE_PATH="$SCRIPT_DIR/vendor/bundle"
fi

# Run Lantae with proper Ruby environment
if command -v bundle >/dev/null 2>&1; then
    cd "$SCRIPT_DIR" && bundle exec ruby "$SCRIPT_DIR/lantae" "$@"
else
    cd "$SCRIPT_DIR" && ruby "$SCRIPT_DIR/lantae" "$@"
fi
EOF
    
    chmod +x "$PROJECT_DIR/lantae-pi"
    show_success "Wrapper script created: ./lantae-pi"
}

# Verify installation
verify_installation() {
    show_progress "Verifying installation..."
    
    cd "$PROJECT_DIR" || error_exit "Cannot access project directory"
    
    # Test if we can load the application
    if ruby -c lantae >/dev/null 2>&1; then
        show_success "Lantae syntax check passed"
    else
        show_warning "Lantae syntax check failed - some gems may be missing"
    fi
    
    # Check if we can at least show help
    if ./lantae-pi --help >/dev/null 2>&1; then
        show_success "Lantae executable working"
    else
        show_warning "Lantae may have issues - check missing dependencies"
    fi
}

# Main installation
main() {
    echo -e "${PURPLE}"
    cat << 'EOF'
╔══════════════════════════════════════════════════════════════╗
║  🍓 Lantae Raspberry Pi Installation                        ║
║  Using System Ruby for ARM Compatibility                    ║
╚══════════════════════════════════════════════════════════════╝
EOF
    echo -e "${NC}"
    
    check_raspberry_pi
    update_packages
    install_system_ruby
    setup_lantae
    create_wrapper
    verify_installation
    
    echo ""
    echo -e "${GREEN}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║  🎉 Raspberry Pi Installation Complete!                      ║${NC}"
    echo -e "${GREEN}╚══════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo -e "${CYAN}To run Lantae on your Raspberry Pi:${NC}"
    echo -e "${YELLOW}  ./lantae-pi${NC}"
    echo ""
    echo -e "${CYAN}Or add to your PATH:${NC}"
    echo -e "${YELLOW}  sudo ln -s $(pwd)/lantae-pi /usr/local/bin/lantae${NC}"
    echo ""
    
    if ruby -v | grep -q "^ruby 2"; then
        echo -e "${YELLOW}⚠️  Note: Your system has Ruby 2.x${NC}"
        echo -e "${YELLOW}   Some features work best with Ruby 3.x${NC}"
        echo -e "${YELLOW}   Consider upgrading to Raspberry Pi OS Bullseye or newer${NC}"
    fi
    
    echo ""
    echo -e "${BLUE}Next steps:${NC}"
    echo "1. Set up your API keys (if using cloud providers)"
    echo "2. Install Ollama for local models (if desired)"
    echo "3. Run './lantae-pi --help' to see options"
    echo ""
}

# Check if running as root (not recommended)
if [[ $EUID -eq 0 ]]; then
    show_warning "Running as root is not recommended."
    read -p "Continue anyway? (y/N): " response
    if [[ "$response" != "y" && "$response" != "Y" ]]; then
        echo "Please run without sudo: ./install-raspberry-pi.sh"
        exit 1
    fi
fi

# Run main function
main "$@"