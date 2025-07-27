#!/bin/bash
set -euo pipefail

# Lantae Installation Script
# Complete installation from basic bash to functional Lantae application

VERSION="1.0.0"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_FILE="${SCRIPT_DIR}/install.log"
SCRIPTS_DIR="${SCRIPT_DIR}/scripts"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Logging function
log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$LOG_FILE"
}

# Error handling
error_exit() {
    echo -e "${RED}ERROR: $1${NC}" >&2
    log "ERROR: $1"
    echo -e "${YELLOW}Check the log file for details: $LOG_FILE${NC}"
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

# OS Detection
detect_os() {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        echo "macos"
    elif [[ -f /etc/debian_version ]]; then
        echo "debian"
    elif [[ -f /etc/redhat-release ]]; then
        echo "redhat"
    elif [[ -f /etc/arch-release ]]; then
        echo "arch"
    else
        echo "unknown"
    fi
}

# Display banner
show_banner() {
    echo -e "${PURPLE}"
    cat << 'EOF'
╔══════════════════════════════════════════════════════════════╗
║  🔮 Lantae Installation Wizard v1.0.0                       ║
║  🚀 From Basic Bash to Functional AI Interface              ║
║  ⚡ Multi-Provider LLM Setup                                ║
╚══════════════════════════════════════════════════════════════╝
EOF
    echo -e "${NC}"
}

# Provider selection menu
select_provider_setup() {
    echo -e "${CYAN}Select your preferred LLM provider setup:${NC}" >&2
    echo "" >&2
    echo "1) 🏠 Local Only    - Ollama with local models (no API keys needed)" >&2
    echo "2) ☁️  Cloud Only    - External providers (OpenAI, Anthropic, etc.)" >&2
    echo "3) 🌐 Hybrid        - Both local and cloud providers" >&2
    echo "4) 📦 Minimal       - Just install Lantae, configure providers later" >&2
    echo "" >&2
    
    while true; do
        read -p "Enter your choice (1-4): " choice
        case $choice in
            1) 
                echo "local"
                return
                ;;
            2) 
                echo "cloud"
                return
                ;;
            3) 
                echo "hybrid"
                return
                ;;
            4) 
                echo "minimal"
                return
                ;;
            *) 
                echo -e "${RED}Invalid choice. Please enter 1, 2, 3, or 4.${NC}" >&2
                ;;
        esac
    done
}

# Component selection
select_components() {
    local provider_type="$1"
    
    echo -e "${CYAN}Select additional components to install:${NC}"
    echo ""
    echo "1) Node.js (Required for MCP servers and VS Code extension)"
    echo "2) LSP Server (Language Server Protocol support)"
    echo "3) Vim Plugin"
    echo "4) System PATH (Type 'lantae' from anywhere)"
    echo "5) All components"
    echo "6) Skip optional components"
    echo ""
    
    INSTALL_NODEJS=false
    INSTALL_LSP=false
    INSTALL_VIM=false
    INSTALL_SYSTEM_PATH=false
    
    read -p "Enter your choice (1-6): " comp_choice
    case $comp_choice in
        1) INSTALL_NODEJS=true ;;
        2) INSTALL_LSP=true ;;
        3) INSTALL_VIM=true ;;
        4) INSTALL_SYSTEM_PATH=true ;;
        5) 
            INSTALL_NODEJS=true
            INSTALL_LSP=true
            INSTALL_VIM=true
            INSTALL_SYSTEM_PATH=true
            ;;
        6) ;;
        *) show_warning "Invalid choice, skipping optional components" ;;
    esac
}

# Check if script exists and is executable
check_script() {
    local script_path="$1"
    if [[ ! -f "$script_path" ]]; then
        error_exit "Required script not found: $script_path"
    fi
    if [[ ! -x "$script_path" ]]; then
        chmod +x "$script_path" || error_exit "Cannot make script executable: $script_path"
    fi
}

# Run installation script with error handling
run_script() {
    local script_name="$1"
    local script_path="${SCRIPTS_DIR}/${script_name}"
    
    show_progress "Running $script_name..."
    check_script "$script_path"
    
    if "$script_path" >> "$LOG_FILE" 2>&1; then
        show_success "$script_name completed"
    else
        error_exit "$script_name failed. Check log for details."
    fi
}

# Main installation function
main() {
    # Initialize
    show_banner
    mkdir -p "$SCRIPTS_DIR"
    echo "Installation started at $(date)" > "$LOG_FILE"
    
    # Detect OS
    OS=$(detect_os)
    show_progress "Detected OS: $OS"
    
    if [[ "$OS" == "unknown" ]]; then
        show_warning "Unknown OS detected. Installation may not work correctly."
        read -p "Continue anyway? (y/N): " continue_choice
        [[ "$continue_choice" != "y" && "$continue_choice" != "Y" ]] && exit 0
    fi
    
    # Export variables for scripts
    export OS
    export SCRIPT_DIR
    export LOG_FILE
    
    # Get user preferences
    PROVIDER_TYPE=$(select_provider_setup)
    select_components "$PROVIDER_TYPE"
    
    echo ""
    show_progress "Starting installation with provider type: $PROVIDER_TYPE"
    echo ""
    
    # Core installation steps
    show_progress "Step 1/6: Installing system dependencies..."
    run_script "install-system-dependencies.sh"
    
    show_progress "Step 2/6: Installing Ruby..."
    run_script "install-ruby.sh"
    
    # Optional Node.js
    if [[ "$INSTALL_NODEJS" == "true" ]]; then
        show_progress "Step 3/6: Installing Node.js..."
        run_script "install-nodejs.sh"
    else
        show_progress "Step 3/6: Skipping Node.js installation"
    fi
    
    show_progress "Step 4/6: Setting up Lantae application..."
    run_script "setup-application.sh"
    
    show_progress "Step 5/6: Configuring providers..."
    export PROVIDER_TYPE
    run_script "setup-providers.sh"
    
    # Optional system PATH installation
    if [[ "$INSTALL_SYSTEM_PATH" == "true" ]]; then
        show_progress "Step 6/7: Installing system PATH..."
        export INSTALL_SYSTEM_PATH
        run_script "install-system-path.sh"
        STEP_COUNT="Step 7/7"
    else
        STEP_COUNT="Step 6/6"
    fi
    
    show_progress "$STEP_COUNT: Verifying installation..."
    run_script "verify-and-launch.sh"
    
    # Installation complete
    echo ""
    echo -e "${GREEN}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║  🎉 Lantae Installation Complete!                           ║${NC}"
    echo -e "${GREEN}╚══════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    
    case $PROVIDER_TYPE in
        "local")
            echo -e "${CYAN}Your Lantae installation is ready with local Ollama support!${NC}"
            if [[ "$INSTALL_SYSTEM_PATH" == "true" ]]; then
                echo -e "${YELLOW}To start: lantae (from anywhere)${NC}"
            else
                echo -e "${YELLOW}To start: ./lantae${NC}"
            fi
            ;;
        "cloud")
            echo -e "${CYAN}Your Lantae installation is ready with cloud providers!${NC}"
            if [[ "$INSTALL_SYSTEM_PATH" == "true" ]]; then
                echo -e "${YELLOW}Set your API keys and run: lantae${NC}"
            else
                echo -e "${YELLOW}Set your API keys and run: ./lantae${NC}"
            fi
            ;;
        "hybrid")
            echo -e "${CYAN}Your Lantae installation supports both local and cloud providers!${NC}"
            if [[ "$INSTALL_SYSTEM_PATH" == "true" ]]; then
                echo -e "${YELLOW}To start: lantae (from anywhere)${NC}"
            else
                echo -e "${YELLOW}To start: ./lantae${NC}"
            fi
            ;;
        "minimal")
            echo -e "${CYAN}Your minimal Lantae installation is ready!${NC}"
            if [[ "$INSTALL_SYSTEM_PATH" == "true" ]]; then
                echo -e "${YELLOW}Configure your preferred provider and run: lantae${NC}"
            else
                echo -e "${YELLOW}Configure your preferred provider and run: ./lantae${NC}"
            fi
            ;;
    esac
    
    echo ""
    echo -e "${BLUE}Next steps:${NC}"
    echo "1. Review the configuration in your home directory"
    echo "2. Set up any additional API keys if needed"
    echo "3. Run './lantae --help' to see all options"
    echo "4. Start with './lantae' for interactive mode"
    echo ""
    echo -e "${GREEN}Installation log saved to: $LOG_FILE${NC}"
}

# Check if running as root (not recommended)
if [[ $EUID -eq 0 ]]; then
    show_warning "Running as root is not recommended. Continue anyway? (y/N)"
    read -r response
    if [[ "$response" != "y" && "$response" != "Y" ]]; then
        echo "Exiting..."
        exit 1
    fi
fi

# Run main function
main "$@"