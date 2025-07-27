#!/bin/bash
set -euo pipefail

# Node.js Installation Script
# Installs Node.js LTS using nvm for MCP servers and VS Code extension

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
    echo -e "${BLUE}[NODE] $1${NC}"
    log "NODE: $1"
}

show_success() {
    echo -e "${GREEN}✓ [NODE] $1${NC}"
    log "NODE SUCCESS: $1"
}

show_warning() {
    echo -e "${YELLOW}⚠ [NODE] $1${NC}"
    log "NODE WARNING: $1"
}

error_exit() {
    echo -e "${RED}ERROR [NODE]: $1${NC}" >&2
    log "NODE ERROR: $1"
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

# Install nvm (Node Version Manager)
install_nvm() {
    if [[ -d "$HOME/.nvm" ]] || command_exists nvm; then
        show_success "nvm already installed"
        return
    fi
    
    show_progress "Installing nvm (Node Version Manager)..."
    
    # Download and install nvm
    local nvm_version="v0.39.0"
    curl -o- "https://raw.githubusercontent.com/nvm-sh/nvm/${nvm_version}/install.sh" | bash || error_exit "Failed to install nvm"
    
    show_success "nvm installed"
}

# Configure nvm in shell
configure_nvm() {
    local shell_config
    shell_config=$(get_shell_config)
    
    show_progress "Configuring nvm in shell ($shell_config)..."
    
    # Create shell config if it doesn't exist
    touch "$shell_config"
    
    # Check if nvm is already configured
    if grep -q 'NVM_DIR' "$shell_config" 2>/dev/null; then
        show_success "nvm already configured in shell"
    else
        # Add nvm configuration
        case "$SHELL" in
            */fish)
                mkdir -p "$HOME/.config/fish"
                echo 'set -gx NVM_DIR "$HOME/.nvm"' >> "$shell_config"
                echo '[ -s "$NVM_DIR/nvm.sh" ]; and source "$NVM_DIR/nvm.sh"' >> "$shell_config"
                ;;
            *)
                echo 'export NVM_DIR="$HOME/.nvm"' >> "$shell_config"
                echo '[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"' >> "$shell_config"
                echo '[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"' >> "$shell_config"
                ;;
        esac
        show_success "nvm configured in shell"
    fi
    
    # Source nvm for current session
    export NVM_DIR="$HOME/.nvm"
    if [[ -s "$NVM_DIR/nvm.sh" ]]; then
        source "$NVM_DIR/nvm.sh"
    fi
}

# Install Node.js
install_nodejs() {
    show_progress "Installing Node.js LTS..."
    
    # Ensure nvm is available
    export NVM_DIR="$HOME/.nvm"
    if [[ -s "$NVM_DIR/nvm.sh" ]]; then
        source "$NVM_DIR/nvm.sh"
    fi
    
    if ! command_exists nvm; then
        error_exit "nvm not found. Try running: source $(get_shell_config)"
    fi
    
    # Check if Node.js is already installed
    if command_exists node; then
        local node_version
        node_version=$(node --version 2>/dev/null | sed 's/v//')
        show_progress "Node.js $node_version already installed"
        
        # Check if it's a recent LTS version (18+)
        local major_version
        major_version=$(echo "$node_version" | cut -d. -f1)
        if [[ "$major_version" -ge 18 ]]; then
            show_success "Node.js version meets requirements"
            return
        else
            show_warning "Node.js version $node_version is outdated, installing LTS..."
        fi
    fi
    
    # Install Node.js LTS
    nvm install --lts || error_exit "Failed to install Node.js LTS"
    nvm use --lts || error_exit "Failed to switch to Node.js LTS"
    nvm alias default lts/* || error_exit "Failed to set LTS as default"
    
    show_success "Node.js LTS installed and set as default"
}

# Install essential npm packages
install_npm_packages() {
    show_progress "Installing essential npm packages..."
    
    # Ensure Node.js and npm are available
    if ! command_exists npm; then
        error_exit "npm not found. Ensure Node.js is properly installed."
    fi
    
    # Update npm to latest version
    show_progress "Updating npm to latest version..."
    npm install -g npm@latest || show_warning "Failed to update npm (may not have permissions)"
    
    # Install MCP server packages (commonly used)
    local mcp_packages=(
        "@modelcontextprotocol/server-filesystem"
        "@modelcontextprotocol/server-git"
        "@modelcontextprotocol/server-sqlite"
    )
    
    show_progress "Installing MCP server packages..."
    for package in "${mcp_packages[@]}"; do
        if npm list -g "$package" >/dev/null 2>&1; then
            show_success "$package already installed"
        else
            show_progress "Installing $package..."
            npm install -g "$package" || show_warning "Failed to install $package globally (may not have permissions)"
        fi
    done
    
    show_success "npm packages installation complete"
}

# Verify Node.js installation
verify_nodejs() {
    show_progress "Verifying Node.js installation..."
    
    # Check Node.js
    if ! command_exists node; then
        error_exit "Node.js not found in PATH. Try running: source $(get_shell_config)"
    fi
    
    local node_version
    node_version=$(node --version 2>/dev/null | sed 's/v//')
    
    if [[ -z "$node_version" ]]; then
        error_exit "Could not determine Node.js version"
    fi
    
    # Check if version is 18+ (LTS requirement)
    local major_version
    major_version=$(echo "$node_version" | cut -d. -f1)
    if [[ "$major_version" -lt 18 ]]; then
        error_exit "Node.js version $node_version does not meet requirement (18+)"
    fi
    
    # Check npm
    if ! command_exists npm; then
        error_exit "npm not found"
    fi
    
    local npm_version
    npm_version=$(npm --version 2>/dev/null)
    
    show_success "Node.js v$node_version verified and ready"
    show_success "npm v$npm_version verified"
}

# Main execution
main() {
    show_progress "Installing Node.js environment..."
    
    install_nvm
    configure_nvm
    install_nodejs
    install_npm_packages
    verify_nodejs
    
    show_success "Node.js installation complete"
    
    # Provide usage instructions
    echo ""
    show_progress "Node.js installation summary:"
    echo "  • Node.js version: $(node --version 2>/dev/null || echo 'Run: source $(get_shell_config)')"
    echo "  • npm version: $(npm --version 2>/dev/null || echo 'Run: source $(get_shell_config)')"
    echo "  • Installation path: $(which node 2>/dev/null || echo 'Run: source $(get_shell_config)')"
    echo "  • MCP servers available for Lantae integration"
    echo ""
    show_warning "If Node.js commands are not found, run: source $(get_shell_config)"
}

main "$@"