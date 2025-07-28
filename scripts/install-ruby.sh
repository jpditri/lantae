#!/bin/bash
set -euo pipefail

# Ruby Installation Script
# Installs Ruby 3.0+ using rbenv and configures environment

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
    echo -e "${BLUE}[RUBY] $1${NC}"
    log "RUBY: $1"
}

show_success() {
    echo -e "${GREEN}✓ [RUBY] $1${NC}"
    log "RUBY SUCCESS: $1"
}

show_warning() {
    echo -e "${YELLOW}⚠ [RUBY] $1${NC}"
    log "RUBY WARNING: $1"
}

error_exit() {
    echo -e "${RED}ERROR [RUBY]: $1${NC}" >&2
    log "RUBY ERROR: $1"
    exit 1
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Get shell config file
get_shell_config() {
    case "$SHELL" in
        */zsh)
            echo "$HOME/.zshrc"
            ;;
        */bash)
            if [[ -f "$HOME/.bashrc" ]]; then
                echo "$HOME/.bashrc"
            else
                echo "$HOME/.bash_profile"
            fi
            ;;
        */fish)
            echo "$HOME/.config/fish/config.fish"
            ;;
        *)
            echo "$HOME/.profile"
            ;;
    esac
}

# Install rbenv
install_rbenv() {
    if command_exists rbenv; then
        show_success "rbenv already installed"
        return
    fi
    
    show_progress "Installing rbenv (Ruby version manager)..."
    
    case "${OS:-unknown}" in
        "macos")
            if command_exists brew; then
                brew install rbenv ruby-build || error_exit "Failed to install rbenv via Homebrew"
            else
                # Install via git
                git clone https://github.com/rbenv/rbenv.git ~/.rbenv || error_exit "Failed to clone rbenv"
                git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build || error_exit "Failed to clone ruby-build"
            fi
            ;;
        "debian"|"redhat"|"arch")
            # Install via git for Linux
            if [[ ! -d "$HOME/.rbenv" ]]; then
                git clone https://github.com/rbenv/rbenv.git ~/.rbenv || error_exit "Failed to clone rbenv"
                git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build || error_exit "Failed to clone ruby-build"
            fi
            ;;
        *)
            error_exit "Unsupported OS for rbenv installation: ${OS:-unknown}"
            ;;
    esac
    
    show_success "rbenv installed"
}

# Configure rbenv in shell
configure_rbenv() {
    local shell_config
    shell_config=$(get_shell_config)
    
    show_progress "Configuring rbenv in shell ($shell_config)..."
    
    # Create shell config if it doesn't exist
    touch "$shell_config"
    
    # Check if rbenv is already configured
    if grep -q 'rbenv init' "$shell_config" 2>/dev/null; then
        show_success "rbenv already configured in shell"
        return
    fi
    
    # Add rbenv to PATH and initialize
    case "$SHELL" in
        */fish)
            echo 'set -gx PATH ~/.rbenv/bin $PATH' >> "$shell_config"
            echo 'status --is-interactive; and rbenv init - fish | source' >> "$shell_config"
            ;;
        *)
            echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> "$shell_config"
            echo 'eval "$(rbenv init - --no-rehash)"' >> "$shell_config"
            ;;
    esac
    
    # Source the config to make rbenv available immediately
    export PATH="$HOME/.rbenv/bin:$PATH"
    if command_exists rbenv; then
        eval "$(rbenv init - --no-rehash)"
    fi
    
    show_success "rbenv configured in shell"
}

# Get appropriate Ruby version for the system
get_latest_ruby_version() {
    # Check if we're on Raspberry Pi or ARM architecture
    if [[ -f /proc/cpuinfo ]] && grep -q "ARM\|arm" /proc/cpuinfo; then
        show_warning "ARM architecture detected (possibly Raspberry Pi)"
        # Use a more stable Ruby version for ARM
        echo "3.1.6"  # Well-tested ARM-compatible version
    elif [[ "$(uname -m)" == "aarch64" || "$(uname -m)" == "armv"* ]]; then
        show_warning "ARM architecture detected"
        echo "3.1.6"  # Well-tested ARM-compatible version
    elif command_exists rbenv; then
        # Get the latest stable Ruby 3.x version for x86/x64
        local latest_version
        latest_version=$(rbenv install --list 2>/dev/null | grep -E "^\s*3\.[0-9]+\.[0-9]+$" | tail -1 | tr -d ' ')
        if [[ -n "$latest_version" ]]; then
            echo "$latest_version"
        else
            echo "3.3.0"  # Fallback version
        fi
    else
        echo "3.3.0"  # Fallback version
    fi
}

# Install Ruby
install_ruby() {
    local ruby_version
    
    show_progress "Determining Ruby version to install..."
    
    # Check if Ruby 3.0+ is already available
    if command_exists ruby; then
        local current_version
        current_version=$(ruby -v | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1)
        if [[ -n "$current_version" ]] && [[ "$current_version" =~ ^3\. ]]; then
            show_success "Ruby $current_version already installed and meets requirements"
            return
        fi
    fi
    
    # Ensure rbenv is available
    if ! command_exists rbenv; then
        error_exit "rbenv not found in PATH. Try running: source $(get_shell_config)"
    fi
    
    ruby_version=$(get_latest_ruby_version)
    show_progress "Installing Ruby $ruby_version..."
    
    # Check if this version is already installed
    if rbenv versions 2>/dev/null | grep -q "$ruby_version"; then
        show_progress "Ruby $ruby_version already installed, setting as global..."
        rbenv global "$ruby_version" || error_exit "Failed to set Ruby $ruby_version as global"
        rbenv rehash
        show_success "Ruby $ruby_version set as global version"
        return
    fi
    
    # Install Ruby (this can take a while)
    show_progress "Compiling Ruby $ruby_version (this may take 10-15 minutes)..."
    
    # Set compilation flags based on architecture
    if [[ -f /proc/cpuinfo ]] && grep -q "ARM\|arm" /proc/cpuinfo; then
        show_progress "Configuring ARM-specific compilation flags..."
        # ARM/Raspberry Pi specific flags
        export RUBY_CONFIGURE_OPTS="--disable-install-doc --with-jemalloc=no"
        export CFLAGS="-O2 -pipe"
        export MAKE_OPTS="-j2"  # Limit parallel jobs to prevent memory issues
        
        # Check available memory
        if [[ -f /proc/meminfo ]]; then
            local mem_gb
            mem_gb=$(awk '/MemTotal/ {printf "%.1f", $2/1024/1024}' /proc/meminfo)
            if (( $(echo "$mem_gb < 2" | bc -l 2>/dev/null || echo "1") )); then
                show_warning "Low memory detected (${mem_gb}GB). Using single-threaded compilation."
                export MAKE_OPTS="-j1"
            fi
        fi
    else
        # x86/x64 flags
        export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl 2>/dev/null || echo /usr)" 2>/dev/null || true
        export CFLAGS="-O2"
    fi
    
    if ! rbenv install "$ruby_version"; then
        show_warning "Ruby compilation failed. Trying alternative installation method..."
        
        # Try with minimal flags for ARM systems
        if [[ -f /proc/cpuinfo ]] && grep -q "ARM\|arm" /proc/cpuinfo; then
            show_progress "Attempting ARM-friendly installation..."
            unset RUBY_CONFIGURE_OPTS CFLAGS MAKE_OPTS
            export RUBY_CONFIGURE_OPTS="--disable-install-doc --disable-install-rdoc --with-jemalloc=no --disable-shared"
            export MAKE_OPTS="-j1"
            rbenv install "$ruby_version" || {
                show_warning "Still failing. Trying with system Ruby packages..."
                # For Raspberry Pi, suggest using system packages
                error_exit "Ruby compilation failed. Try installing system Ruby with: sudo apt update && sudo apt install -y ruby ruby-dev ruby-bundler"
            }
        else
            # Try with different flags for other systems
            unset RUBY_CONFIGURE_OPTS CFLAGS
            rbenv install "$ruby_version" || error_exit "Failed to install Ruby $ruby_version"
        fi
    fi
    
    # Set as global version
    rbenv global "$ruby_version" || error_exit "Failed to set Ruby $ruby_version as global"
    rbenv rehash
    
    show_success "Ruby $ruby_version installed and set as global"
}

# Install Bundler gem
install_bundler() {
    show_progress "Installing Bundler gem..."
    
    # Ensure we're using the right Ruby
    if ! command_exists ruby || ! command_exists gem; then
        error_exit "Ruby/gem not found. Ensure rbenv is properly initialized."
    fi
    
    # Check if bundler is already installed
    if gem list bundler | grep -q bundler; then
        show_success "Bundler already installed"
        return
    fi
    
    # Install bundler
    gem install bundler --no-document || error_exit "Failed to install bundler"
    rbenv rehash 2>/dev/null || true
    
    show_success "Bundler installed"
}

# Verify Ruby installation
verify_ruby() {
    show_progress "Verifying Ruby installation..."
    
    # Check Ruby version
    if ! command_exists ruby; then
        error_exit "Ruby not found in PATH. Try running: source $(get_shell_config)"
    fi
    
    local ruby_version
    ruby_version=$(ruby -v | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1)
    
    if [[ -z "$ruby_version" ]]; then
        error_exit "Could not determine Ruby version"
    fi
    
    if [[ ! "$ruby_version" =~ ^3\. ]]; then
        error_exit "Ruby version $ruby_version does not meet requirement (3.0+)"
    fi
    
    # Check bundler
    if ! command_exists bundle; then
        error_exit "Bundler not found"
    fi
    
    # Check gem command
    if ! command_exists gem; then
        error_exit "gem command not found"
    fi
    
    show_success "Ruby $ruby_version verified and ready"
    show_success "Bundler $(bundle version | grep -oE '[0-9]+\.[0-9]+\.[0-9]+') verified"
}

# Main execution
main() {
    show_progress "Installing Ruby environment..."
    
    install_rbenv
    configure_rbenv
    install_ruby
    install_bundler
    verify_ruby
    
    show_success "Ruby installation complete"
    
    # Provide usage instructions
    echo ""
    show_progress "Ruby installation summary:"
    echo "  • Ruby version: $(ruby -v 2>/dev/null || echo 'Run: source $(get_shell_config)')"
    echo "  • Bundler version: $(bundle version 2>/dev/null || echo 'Run: source $(get_shell_config)')"
    echo "  • Installation path: $(which ruby 2>/dev/null || echo 'Run: source $(get_shell_config)')"
    echo ""
    show_warning "If Ruby commands are not found, run: source $(get_shell_config)"
}

main "$@"