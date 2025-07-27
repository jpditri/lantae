#!/bin/bash
set -euo pipefail

# Verification and Launch Script
# Verifies complete installation and provides launch guidance

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "${LOG_FILE:-install.log}"
}

show_progress() {
    echo -e "${BLUE}[VERIFY] $1${NC}"
    log "VERIFY: $1"
}

show_success() {
    echo -e "${GREEN}✓ [VERIFY] $1${NC}"
    log "VERIFY SUCCESS: $1"
}

show_warning() {
    echo -e "${YELLOW}⚠ [VERIFY] $1${NC}"
    log "VERIFY WARNING: $1"
}

show_error() {
    echo -e "${RED}✗ [VERIFY] $1${NC}"
    log "VERIFY ERROR: $1"
}

show_info() {
    echo -e "${CYAN}ℹ [VERIFY] $1${NC}"
    log "VERIFY INFO: $1"
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Verify system dependencies
verify_system() {
    show_progress "Verifying system dependencies..."
    
    local issues=0
    local required_commands=("git" "curl" "wget" "make" "sqlite3")
    
    for cmd in "${required_commands[@]}"; do
        if command_exists "$cmd"; then
            show_success "$cmd found"
        else
            show_error "$cmd not found"
            ((issues++))
        fi
    done
    
    return $issues
}

# Verify Ruby environment
verify_ruby() {
    show_progress "Verifying Ruby environment..."
    
    local issues=0
    
    if command_exists ruby; then
        local ruby_version
        ruby_version=$(ruby -v | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1)
        
        if [[ "$ruby_version" =~ ^3\. ]]; then
            show_success "Ruby $ruby_version (meets requirement)"
        else
            show_error "Ruby $ruby_version (requires 3.0+)"
            ((issues++))
        fi
    else
        show_error "Ruby not found"
        ((issues++))
    fi
    
    if command_exists bundle; then
        local bundler_version
        bundler_version=$(bundle version | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1)
        show_success "Bundler $bundler_version"
    else
        show_error "Bundler not found"
        ((issues++))
    fi
    
    return $issues
}

# Verify Node.js environment (optional)
verify_nodejs() {
    show_progress "Verifying Node.js environment (optional)..."
    
    local issues=0
    
    if command_exists node; then
        local node_version
        node_version=$(node --version | sed 's/v//')
        local major_version
        major_version=$(echo "$node_version" | cut -d. -f1)
        
        if [[ "$major_version" -ge 18 ]]; then
            show_success "Node.js v$node_version (meets requirement)"
        else
            show_warning "Node.js v$node_version (recommended: 18+)"
        fi
    else
        show_warning "Node.js not found (optional for MCP features)"
    fi
    
    if command_exists npm; then
        local npm_version
        npm_version=$(npm --version)
        show_success "npm v$npm_version"
    else
        show_warning "npm not found (optional for MCP features)"
    fi
    
    return $issues
}

# Verify Lantae application
verify_lantae() {
    show_progress "Verifying Lantae application..."
    
    local issues=0
    
    # Check for required files
    local required_files=(
        "lantae"
        "Gemfile"
        "Gemfile.lock"
        "lib/ruby"
        ".env"
    )
    
    for file in "${required_files[@]}"; do
        if [[ -e "$file" ]]; then
            show_success "$file exists"
        else
            show_error "$file missing"
            ((issues++))
        fi
    done
    
    # Check executable permissions
    if [[ -x "lantae" ]]; then
        show_success "lantae is executable"
    else
        show_error "lantae is not executable"
        ((issues++))
    fi
    
    # Check gem dependencies
    if [[ -f "Gemfile.lock" ]]; then
        if bundle check >/dev/null 2>&1; then
            show_success "All gems installed"
        else
            show_error "Missing gem dependencies"
            ((issues++))
        fi
    fi
    
    return $issues
}

# Verify provider configuration
verify_providers() {
    local provider_type="${PROVIDER_TYPE:-minimal}"
    show_progress "Verifying provider configuration ($provider_type)..."
    
    local issues=0
    
    case "$provider_type" in
        "local"|"hybrid")
            if command_exists ollama; then
                show_success "Ollama installed"
                
                # Check if Ollama is running
                if pgrep -f "ollama serve" >/dev/null; then
                    show_success "Ollama service running"
                    
                    # Check available models
                    if ollama list >/dev/null 2>&1; then
                        local model_count
                        model_count=$(ollama list | grep -c ":" 2>/dev/null || echo "0")
                        if [[ "$model_count" -gt 0 ]]; then
                            show_success "$model_count Ollama models available"
                        else
                            show_warning "No Ollama models installed"
                        fi
                    else
                        show_warning "Cannot list Ollama models"
                    fi
                else
                    show_warning "Ollama service not running"
                fi
            else
                show_error "Ollama not found (required for local provider)"
                ((issues++))
            fi
            ;;
    esac
    
    case "$provider_type" in
        "cloud"|"hybrid")
            show_info "Cloud providers configured (API keys needed)"
            
            # Check for common API key environment variables
            local api_keys_found=0
            local api_keys=("OPENAI_API_KEY" "ANTHROPIC_API_KEY" "GEMINI_API_KEY" "MISTRAL_API_KEY" "PERPLEXITY_API_KEY")
            
            for key in "${api_keys[@]}"; do
                if [[ -n "${!key:-}" ]]; then
                    show_success "$key set"
                    ((api_keys_found++))
                fi
            done
            
            if [[ $api_keys_found -eq 0 ]]; then
                show_warning "No API keys found in environment (set them to use cloud providers)"
            fi
            ;;
    esac
    
    return $issues
}

# Test basic functionality
test_lantae() {
    show_progress "Testing Lantae basic functionality..."
    
    local issues=0
    
    # Test help command
    if ./lantae --help >/dev/null 2>&1; then
        show_success "Help command works"
    else
        show_error "Help command failed"
        ((issues++))
    fi
    
    # Test version command
    if ./lantae --version >/dev/null 2>&1; then
        show_success "Version command works"
    else
        show_error "Version command failed"
        ((issues++))
    fi
    
    return $issues
}

# Verify system PATH installation
verify_system_path() {
    show_progress "Verifying system PATH installation..."
    
    local issues=0
    
    # Check if system PATH was requested
    if [[ "${INSTALL_SYSTEM_PATH:-false}" != "true" ]]; then
        show_info "System PATH installation not requested"
        return 0
    fi
    
    # Check if lantae command is available globally
    if command_exists lantae; then
        # Test that global command works
        if lantae --version >/dev/null 2>&1; then
            show_success "Global 'lantae' command works"
            
            # Show installation location
            local lantae_path
            lantae_path=$(which lantae)
            show_success "Installed at: $lantae_path"
        else
            show_error "Global 'lantae' command found but not working"
            ((issues++))
        fi
    else
        show_error "Global 'lantae' command not found in PATH"
        show_warning "You may need to restart your terminal or run: source ~/.bashrc"
        ((issues++))
    fi
    
    return $issues
}

# Generate verification report
generate_report() {
    local total_issues=$1
    
    echo ""
    echo -e "${BOLD}${PURPLE}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BOLD}${PURPLE}║                    VERIFICATION REPORT                      ║${NC}"
    echo -e "${BOLD}${PURPLE}╚══════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    
    if [[ $total_issues -eq 0 ]]; then
        echo -e "${GREEN}🎉 All verification checks passed!${NC}"
        echo -e "${GREEN}✓ Lantae is ready to use${NC}"
    elif [[ $total_issues -le 2 ]]; then
        echo -e "${YELLOW}⚠ Minor issues found ($total_issues)${NC}"
        echo -e "${YELLOW}✓ Lantae should work with limitations${NC}"
    else
        echo -e "${RED}❌ Significant issues found ($total_issues)${NC}"
        echo -e "${RED}✗ Lantae may not work properly${NC}"
    fi
    
    echo ""
}

# Show usage instructions
show_usage() {
    local provider_type="${PROVIDER_TYPE:-minimal}"
    
    echo -e "${BOLD}${CYAN}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BOLD}${CYAN}║                     USAGE INSTRUCTIONS                      ║${NC}"
    echo -e "${BOLD}${CYAN}╚══════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    
    case "$provider_type" in
        "local")
            echo -e "${GREEN}🏠 Local Setup Complete${NC}"
            echo ""
            echo -e "${CYAN}Start using Lantae:${NC}"
            if [[ "${INSTALL_SYSTEM_PATH:-false}" == "true" ]] && command_exists lantae; then
                echo "  lantae                             # Interactive mode with cogito model"
                echo "  lantae \"Hello, how are you?\"       # Single prompt"
                echo "  lantae -m qwq:32b \"Explain AI\"     # Different model"
                echo "  lantae --help                     # See all options"
            else
                echo "  ./lantae                           # Interactive mode with cogito model"
                echo "  ./lantae \"Hello, how are you?\"     # Single prompt"
                echo "  ./lantae -m qwq:32b \"Explain AI\"   # Different model"
                echo "  ./lantae --help                   # See all options"
            fi
            ;;
        "cloud")
            echo -e "${BLUE}☁️ Cloud Setup Complete${NC}"
            echo ""
            echo -e "${CYAN}Set up API keys first:${NC}"
            echo "  export OPENAI_API_KEY='your-key'"
            echo "  export ANTHROPIC_API_KEY='your-key'"
            echo ""
            echo -e "${CYAN}Then start using Lantae:${NC}"
            if [[ "${INSTALL_SYSTEM_PATH:-false}" == "true" ]] && command_exists lantae; then
                echo "  lantae -p openai -m gpt-4o        # OpenAI GPT-4"
                echo "  lantae -p anthropic               # Anthropic Claude"
                echo "  lantae -p gemini                  # Google Gemini"
            else
                echo "  ./lantae -p openai -m gpt-4o      # OpenAI GPT-4"
                echo "  ./lantae -p anthropic             # Anthropic Claude"
                echo "  ./lantae -p gemini                # Google Gemini"
            fi
            ;;
        "hybrid")
            echo -e "${PURPLE}🌐 Hybrid Setup Complete${NC}"
            echo ""
            echo -e "${CYAN}Local usage (no API keys needed):${NC}"
            echo "  ./lantae                           # Default cogito model"
            echo "  ./lantae -m qwq:32b               # Different local model"
            echo ""
            echo -e "${CYAN}Cloud usage (set API keys first):${NC}"
            echo "  ./lantae -p openai -m gpt-4o      # OpenAI GPT-4"
            echo "  ./lantae -p anthropic             # Anthropic Claude"
            ;;
        "minimal")
            echo -e "${YELLOW}📦 Minimal Setup Complete${NC}"
            echo ""
            echo -e "${CYAN}Choose your setup:${NC}"
            echo ""
            echo -e "${YELLOW}For local usage:${NC}"
            echo "  curl -fsSL https://ollama.com/install.sh | sh"
            echo "  ollama serve"
            echo "  ollama pull cogito:latest"
            echo "  ./lantae"
            echo ""
            echo -e "${YELLOW}For cloud usage:${NC}"
            echo "  export OPENAI_API_KEY='your-key'"
            echo "  ./lantae -p openai -m gpt-4o"
            ;;
    esac
    
    echo ""
    echo -e "${CYAN}Additional features:${NC}"
    echo "  ./lantae --auto-accept             # Auto-confirm actions"
    echo "  ./lantae --planning-mode           # Detailed task planning"
    echo "  ./lantae --enable-mcp              # Enable MCP servers"
    echo "  ./lantae --enable-lsp              # Enable LSP support"
    echo ""
    echo -e "${CYAN}Interactive commands (inside Lantae):${NC}"
    echo "  /help                              # Show available commands"
    echo "  /model <name>                      # Switch models"
    echo "  /provider <name>                   # Switch providers"
    echo "  /tool <name> <args>                # Use tools"
    echo "  /clear                             # Clear history"
    echo ""
}

# Main execution
main() {
    show_progress "Starting verification and launch preparation..."
    
    local total_issues=0
    
    # Run all verification checks
    verify_system || ((total_issues += $?))
    verify_ruby || ((total_issues += $?))
    verify_nodejs || ((total_issues += $?))
    verify_lantae || ((total_issues += $?))
    verify_providers || ((total_issues += $?))
    test_lantae || ((total_issues += $?))
    verify_system_path || ((total_issues += $?))
    
    # Generate report
    generate_report $total_issues
    
    # Show usage instructions
    show_usage
    
    # Final message
    echo -e "${BOLD}${GREEN}🚀 Installation verification complete!${NC}"
    echo ""
    echo -e "${CYAN}Next steps:${NC}"
    echo "1. Review any warnings above"
    echo "2. Set up API keys if using cloud providers"
    if [[ "${INSTALL_SYSTEM_PATH:-false}" == "true" ]] && command_exists lantae; then
        echo "3. Run: lantae"
    else
        echo "3. Run: ./lantae"
    fi
    echo ""
    echo -e "${YELLOW}For support: https://github.com/jpditri/lantae${NC}"
    echo -e "${YELLOW}Documentation: README.md${NC}"
    
    return $total_issues
}

main "$@"