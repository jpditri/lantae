#!/bin/bash
set -euo pipefail

# Lantae Uninstall Script
# Safely removes Lantae installation and optionally associated tools

VERSION="1.0.0"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_FILE="${SCRIPT_DIR}/uninstall.log"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# Logging function
log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$LOG_FILE"
}

# Error handling
error_exit() {
    echo -e "${RED}ERROR: $1${NC}" >&2
    log "ERROR: $1"
    exit 1
}

# Progress indicator
show_progress() {
    echo -e "${BLUE}[$(date '+%H:%M:%S')] $1${NC}"
    log "PROGRESS: $1"
}

# Success indicator
show_success() {
    echo -e "${GREEN}✓ $1${NC}"
    log "SUCCESS: $1"
}

# Warning indicator
show_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
    log "WARNING: $1"
}

# Info indicator
show_info() {
    echo -e "${CYAN}ℹ $1${NC}"
    log "INFO: $1"
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Display banner
show_banner() {
    echo -e "${RED}"
    cat << 'EOF'
╔══════════════════════════════════════════════════════════════╗
║  🗑️  Lantae Uninstall Wizard v1.0.0                        ║
║  🧹 Clean removal of Lantae and associated components       ║
║  ⚠️  This action cannot be undone                           ║
╚══════════════════════════════════════════════════════════════╝
EOF
    echo -e "${NC}"
}

# Uninstall options menu
select_uninstall_options() {
    echo -e "${CYAN}Select what to uninstall:${NC}" >&2
    echo "" >&2
    echo "1) 🏠 Lantae Only       - Remove Lantae app but keep Ruby, Node.js, etc." >&2
    echo "2) 🧹 Complete Cleanup  - Remove Lantae + Ruby + Node.js + Ollama" >&2
    echo "3) 🎯 Custom Selection  - Choose specific components" >&2
    echo "4) 🚫 Cancel           - Exit without changes" >&2
    echo "" >&2
    
    while true; do
        read -p "Enter your choice (1-4): " choice
        case $choice in
            1) echo "lantae-only" ;;
            2) echo "complete" ;;
            3) echo "custom" ;;
            4) echo "cancel" ;;
            *) 
                echo -e "${RED}Invalid choice. Please enter 1, 2, 3, or 4.${NC}" >&2
                continue
                ;;
        esac
        return
    done
}

# Parse command line arguments
parse_arguments() {
    UNINSTALL_TYPE=""
    SKIP_CONFIRMATION=false
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            --app-only)
                UNINSTALL_TYPE="lantae-only"
                shift
                ;;
            --complete)
                UNINSTALL_TYPE="complete"
                shift
                ;;
            --custom)
                UNINSTALL_TYPE="custom"
                shift
                ;;
            --confirm|-y)
                SKIP_CONFIRMATION=true
                shift
                ;;
            --help|-h)
                show_usage
                exit 0
                ;;
            *)
                echo -e "${RED}Unknown option: $1${NC}" >&2
                show_usage
                exit 1
                ;;
        esac
    done
}

# Show usage information
show_usage() {
    echo "Lantae Uninstall Script"
    echo ""
    echo "Usage: $0 [options]"
    echo ""
    echo "Options:"
    echo "  --app-only       Remove only Lantae application (keep Ruby, Node.js, Ollama)"
    echo "  --complete       Remove everything (Lantae + Ruby + Node.js + Ollama)"
    echo "  --custom         Interactive custom component selection"
    echo "  --confirm, -y    Skip confirmation prompts"
    echo "  --help, -h       Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                 # Interactive mode with menu"
    echo "  $0 --app-only      # Remove only Lantae"
    echo "  $0 --complete -y   # Remove everything without confirmation"
    echo ""
}

# Custom component selection
select_custom_components() {
    echo -e "${CYAN}Select components to remove:${NC}"
    echo ""
    
    REMOVE_LANTAE=false
    REMOVE_RUBY=false
    REMOVE_NODEJS=false
    REMOVE_OLLAMA=false
    REMOVE_SYSTEM_PATH=false
    
    # Lantae app
    echo -n "Remove Lantae application? (y/N): "
    read -r response
    [[ "$response" =~ ^[Yy]$ ]] && REMOVE_LANTAE=true
    
    # System PATH
    if command_exists lantae && [[ "$(which lantae)" != "$SCRIPT_DIR/lantae" ]]; then
        echo -n "Remove system PATH installation? (y/N): "
        read -r response
        [[ "$response" =~ ^[Yy]$ ]] && REMOVE_SYSTEM_PATH=true
    fi
    
    # Ruby
    if command_exists ruby && command_exists rbenv; then
        echo -n "Remove Ruby environment (rbenv + gems)? (y/N): "
        read -r response
        [[ "$response" =~ ^[Yy]$ ]] && REMOVE_RUBY=true
    fi
    
    # Node.js
    if command_exists node && [[ -d "$HOME/.nvm" ]]; then
        echo -n "Remove Node.js environment (nvm + packages)? (y/N): "
        read -r response
        [[ "$response" =~ ^[Yy]$ ]] && REMOVE_NODEJS=true
    fi
    
    # Ollama
    if command_exists ollama; then
        echo -n "Remove Ollama and all models? (y/N): "
        read -r response
        [[ "$response" =~ ^[Yy]$ ]] && REMOVE_OLLAMA=true
    fi
}

# Confirm uninstall
confirm_uninstall() {
    local uninstall_type="$1"
    
    # Skip confirmation if flag is set
    if [[ "$SKIP_CONFIRMATION" == "true" ]]; then
        show_info "Skipping confirmation (--confirm flag used)"
        return
    fi
    
    echo ""
    echo -e "${BOLD}${YELLOW}⚠️  CONFIRMATION REQUIRED${NC}"
    echo ""
    
    case "$uninstall_type" in
        "lantae-only")
            echo -e "${CYAN}This will remove:${NC}"
            echo "  • Lantae application and configuration"
            echo "  • System PATH installation (if present)"
            echo "  • Project directory contents"
            echo ""
            echo -e "${GREEN}This will keep:${NC}"
            echo "  • Ruby environment"
            echo "  • Node.js environment"
            echo "  • Ollama and models"
            ;;
        "complete")
            echo -e "${RED}This will remove EVERYTHING:${NC}"
            echo "  • Lantae application"
            echo "  • Ruby environment (rbenv + gems)"
            echo "  • Node.js environment (nvm + packages)"
            echo "  • Ollama and all models"
            echo "  • System PATH installation"
            echo "  • All configuration files"
            ;;
        "custom")
            echo -e "${CYAN}Selected for removal:${NC}"
            [[ "$REMOVE_LANTAE" == "true" ]] && echo "  • Lantae application"
            [[ "$REMOVE_SYSTEM_PATH" == "true" ]] && echo "  • System PATH installation"
            [[ "$REMOVE_RUBY" == "true" ]] && echo "  • Ruby environment"
            [[ "$REMOVE_NODEJS" == "true" ]] && echo "  • Node.js environment"
            [[ "$REMOVE_OLLAMA" == "true" ]] && echo "  • Ollama and models"
            ;;
    esac
    
    echo ""
    echo -e "${BOLD}${RED}This action cannot be undone!${NC}"
    echo ""
    
    read -p "Are you absolutely sure? Type 'YES' to continue: " confirmation
    if [[ "$confirmation" != "YES" ]]; then
        echo -e "${YELLOW}Uninstall cancelled.${NC}"
        exit 0
    fi
}

# Remove system PATH installation
remove_system_path() {
    show_progress "Removing system PATH installation..."
    
    local removed=false
    
    # Check common system locations
    local system_paths=(
        "/usr/local/bin/lantae"
        "/opt/homebrew/bin/lantae"
        "$HOME/.local/bin/lantae"
    )
    
    for path in "${system_paths[@]}"; do
        if [[ -f "$path" ]]; then
            if [[ -w "$(dirname "$path")" ]]; then
                rm -f "$path" && show_success "Removed $path"
                removed=true
            else
                sudo rm -f "$path" && show_success "Removed $path (with sudo)"
                removed=true
            fi
        fi
    done
    
    if [[ "$removed" == "false" ]]; then
        show_info "No system PATH installation found"
    fi
}

# Remove Lantae application
remove_lantae() {
    show_progress "Removing Lantae application..."
    
    # Remove configuration files
    local config_files=(
        ".env"
        "mcp_servers.yml"
        "lantae.md"
        "install.log"
        "uninstall.log"
    )
    
    for file in "${config_files[@]}"; do
        if [[ -f "$file" ]]; then
            rm -f "$file"
            show_success "Removed $file"
        fi
    done
    
    # Remove data directory
    if [[ -d "data" ]]; then
        rm -rf "data"
        show_success "Removed data directory"
    fi
    
    # Remove scripts directory
    if [[ -d "scripts" ]]; then
        rm -rf "scripts"
        show_success "Removed scripts directory"
    fi
    
    # Remove docs/providers directory
    if [[ -d "docs/providers" ]]; then
        rm -rf "docs/providers"
        show_success "Removed provider documentation"
    fi
    
    show_success "Lantae application removed"
}

# Remove Ruby environment
remove_ruby() {
    show_progress "Removing Ruby environment..."
    
    # Remove rbenv
    if [[ -d "$HOME/.rbenv" ]]; then
        rm -rf "$HOME/.rbenv"
        show_success "Removed rbenv directory"
    fi
    
    # Clean shell configuration
    local shell_configs=(
        "$HOME/.bashrc"
        "$HOME/.bash_profile"
        "$HOME/.zshrc"
        "$HOME/.profile"
    )
    
    for config in "${shell_configs[@]}"; do
        if [[ -f "$config" ]]; then
            # Remove rbenv lines
            if grep -q "rbenv" "$config" 2>/dev/null; then
                sed -i.bak '/rbenv/d' "$config" 2>/dev/null && rm -f "$config.bak"
                show_success "Cleaned rbenv from $(basename "$config")"
            fi
        fi
    done
    
    # Remove fish config if exists
    if [[ -f "$HOME/.config/fish/config.fish" ]]; then
        if grep -q "rbenv" "$HOME/.config/fish/config.fish" 2>/dev/null; then
            sed -i.bak '/rbenv/d' "$HOME/.config/fish/config.fish" 2>/dev/null && rm -f "$HOME/.config/fish/config.fish.bak"
            show_success "Cleaned rbenv from fish config"
        fi
    fi
    
    show_success "Ruby environment removed"
}

# Remove Node.js environment
remove_nodejs() {
    show_progress "Removing Node.js environment..."
    
    # Remove nvm
    if [[ -d "$HOME/.nvm" ]]; then
        rm -rf "$HOME/.nvm"
        show_success "Removed nvm directory"
    fi
    
    # Clean shell configuration
    local shell_configs=(
        "$HOME/.bashrc"
        "$HOME/.bash_profile"
        "$HOME/.zshrc"
        "$HOME/.profile"
    )
    
    for config in "${shell_configs[@]}"; do
        if [[ -f "$config" ]]; then
            # Remove nvm lines
            if grep -q "NVM_DIR\|nvm.sh" "$config" 2>/dev/null; then
                sed -i.bak '/NVM_DIR\|nvm\.sh/d' "$config" 2>/dev/null && rm -f "$config.bak"
                show_success "Cleaned nvm from $(basename "$config")"
            fi
        fi
    done
    
    show_success "Node.js environment removed"
}

# Remove Ollama
remove_ollama() {
    show_progress "Removing Ollama..."
    
    # Stop Ollama service first
    if pgrep -f "ollama serve" >/dev/null; then
        show_progress "Stopping Ollama service..."
        pkill -f "ollama serve" || true
        sleep 2
    fi
    
    # Remove via package manager or manual
    case "$(uname)" in
        "Darwin")
            if command_exists brew && brew list ollama >/dev/null 2>&1; then
                brew uninstall ollama
                show_success "Removed Ollama via Homebrew"
            else
                # Manual removal on macOS
                sudo rm -f /usr/local/bin/ollama
                rm -rf "$HOME/.ollama"
                show_success "Removed Ollama manually"
            fi
            ;;
        "Linux")
            # Try systemctl stop first
            if systemctl is-active ollama >/dev/null 2>&1; then
                sudo systemctl stop ollama
                sudo systemctl disable ollama
            fi
            
            # Remove binary and data
            sudo rm -f /usr/local/bin/ollama /usr/bin/ollama
            rm -rf "$HOME/.ollama"
            show_success "Removed Ollama from Linux"
            ;;
    esac
    
    show_success "Ollama removed"
}

# Generate uninstall report
generate_report() {
    echo ""
    echo -e "${BOLD}${GREEN}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BOLD}${GREEN}║                    UNINSTALL COMPLETE                       ║${NC}"
    echo -e "${BOLD}${GREEN}╚══════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    
    echo -e "${CYAN}Summary of actions taken:${NC}"
    echo "  • Uninstall started: $(date '+%Y-%m-%d %H:%M:%S')"
    echo "  • Log file: $LOG_FILE"
    echo ""
    
    echo -e "${YELLOW}Manual cleanup (if needed):${NC}"
    echo "  • Restart terminal to clear environment variables"
    echo "  • Check ~/.bashrc, ~/.zshrc for any remaining references"
    echo "  • Remove project directory: rm -rf $(pwd)"
    echo ""
    
    echo -e "${GREEN}Thank you for using Lantae! 👋${NC}"
}

# Main execution
main() {
    # Parse command line arguments first
    parse_arguments "$@"
    
    show_banner
    
    # Initialize logging
    echo "Uninstall started at $(date)" > "$LOG_FILE"
    
    # Get uninstall preferences (if not set by command line)
    if [[ -z "$UNINSTALL_TYPE" ]]; then
        UNINSTALL_TYPE=$(select_uninstall_options)
        
        if [[ "$UNINSTALL_TYPE" == "cancel" ]]; then
            echo -e "${YELLOW}Uninstall cancelled by user.${NC}"
            exit 0
        fi
    fi
    
    # Set variables based on selection
    case "$UNINSTALL_TYPE" in
        "lantae-only")
            REMOVE_LANTAE=true
            REMOVE_SYSTEM_PATH=true
            REMOVE_RUBY=false
            REMOVE_NODEJS=false
            REMOVE_OLLAMA=false
            ;;
        "complete")
            REMOVE_LANTAE=true
            REMOVE_SYSTEM_PATH=true
            REMOVE_RUBY=true
            REMOVE_NODEJS=true
            REMOVE_OLLAMA=true
            ;;
        "custom")
            select_custom_components
            ;;
    esac
    
    # Confirm the action
    confirm_uninstall "$UNINSTALL_TYPE"
    
    echo ""
    show_progress "Starting uninstall process..."
    
    # Perform removals in safe order
    [[ "$REMOVE_SYSTEM_PATH" == "true" ]] && remove_system_path
    [[ "$REMOVE_OLLAMA" == "true" ]] && remove_ollama
    [[ "$REMOVE_NODEJS" == "true" ]] && remove_nodejs
    [[ "$REMOVE_RUBY" == "true" ]] && remove_ruby
    [[ "$REMOVE_LANTAE" == "true" ]] && remove_lantae
    
    # Generate final report
    generate_report
}

# Check if running from Lantae directory
if [[ ! -f "lantae" ]] && [[ ! -f "install.sh" ]]; then
    show_warning "This doesn't appear to be a Lantae installation directory."
    read -p "Continue anyway? (y/N): " continue_choice
    [[ "$continue_choice" != "y" && "$continue_choice" != "Y" ]] && exit 0
fi

# Run main function
main "$@"