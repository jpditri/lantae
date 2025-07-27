#!/bin/bash
set -euo pipefail

# Provider Setup Script
# Configures LLM providers based on user choice (local/cloud/hybrid/minimal)

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "${LOG_FILE:-install.log}"
}

show_progress() {
    echo -e "${BLUE}[PROVIDER] $1${NC}"
    log "PROVIDER: $1"
}

show_success() {
    echo -e "${GREEN}✓ [PROVIDER] $1${NC}"
    log "PROVIDER SUCCESS: $1"
}

show_warning() {
    echo -e "${YELLOW}⚠ [PROVIDER] $1${NC}"
    log "PROVIDER WARNING: $1"
}

show_info() {
    echo -e "${CYAN}ℹ [PROVIDER] $1${NC}"
    log "PROVIDER INFO: $1"
}

error_exit() {
    echo -e "${RED}ERROR [PROVIDER]: $1${NC}" >&2
    log "PROVIDER ERROR: $1"
    exit 1
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Install Ollama
install_ollama() {
    show_progress "Installing Ollama..."
    
    if command_exists ollama; then
        show_success "Ollama already installed"
        return
    fi
    
    case "${OS:-unknown}" in
        "macos")
            if command_exists brew; then
                brew install ollama || error_exit "Failed to install Ollama via Homebrew"
            else
                # Use official installer
                curl -fsSL https://ollama.com/install.sh | sh || error_exit "Failed to install Ollama"
            fi
            ;;
        "debian"|"redhat"|"arch")
            # Use official installer for Linux
            curl -fsSL https://ollama.com/install.sh | sh || error_exit "Failed to install Ollama"
            ;;
        *)
            error_exit "Unsupported OS for Ollama installation: ${OS:-unknown}"
            ;;
    esac
    
    show_success "Ollama installed"
}

# Start Ollama service
start_ollama() {
    show_progress "Starting Ollama service..."
    
    if pgrep -f "ollama serve" >/dev/null; then
        show_success "Ollama service already running"
        return
    fi
    
    case "${OS:-unknown}" in
        "macos")
            # On macOS, start as background service
            ollama serve >/dev/null 2>&1 &
            sleep 3
            
            if pgrep -f "ollama serve" >/dev/null; then
                show_success "Ollama service started"
            else
                error_exit "Failed to start Ollama service"
            fi
            ;;
        "debian"|"redhat"|"arch")
            # On Linux, try systemd first, then manual start
            if systemctl is-enabled ollama >/dev/null 2>&1; then
                sudo systemctl start ollama || error_exit "Failed to start Ollama service"
                show_success "Ollama service started via systemd"
            else
                # Manual start in background
                nohup ollama serve >/dev/null 2>&1 &
                sleep 3
                
                if pgrep -f "ollama serve" >/dev/null; then
                    show_success "Ollama service started manually"
                else
                    error_exit "Failed to start Ollama service"
                fi
            fi
            ;;
    esac
}

# Install Ollama models
install_ollama_models() {
    show_progress "Installing Ollama models..."
    
    # Wait for Ollama to be ready
    local max_attempts=30
    local attempt=1
    
    while ! ollama list >/dev/null 2>&1; do
        if [[ $attempt -ge $max_attempts ]]; then
            error_exit "Ollama service not responding after $max_attempts attempts"
        fi
        
        show_progress "Waiting for Ollama service (attempt $attempt/$max_attempts)..."
        sleep 2
        ((attempt++))
    done
    
    # Install cogito model (default reasoning model)
    show_progress "Installing cogito:latest model (this may take several minutes)..."
    if ollama list | grep -q "cogito:latest"; then
        show_success "cogito:latest already installed"
    else
        if ollama pull cogito:latest; then
            show_success "cogito:latest model installed"
        else
            show_warning "Failed to install cogito:latest, trying alternative models..."
            
            # Try alternative models
            local alt_models=("qwq:32b" "llama3.1-intuitive-thinker" "qwen2.5:3b")
            for model in "${alt_models[@]}"; do
                show_progress "Trying alternative model: $model..."
                if ollama pull "$model" 2>/dev/null; then
                    show_success "Alternative model $model installed"
                    break
                fi
            done
        fi
    fi
    
    # Install a lightweight model for quick responses
    show_progress "Installing lightweight model..."
    if ollama list | grep -q "qwen2.5:1.5b"; then
        show_success "qwen2.5:1.5b already installed"
    else
        ollama pull qwen2.5:1.5b || show_warning "Failed to install lightweight model (optional)"
    fi
    
    show_success "Ollama models installation complete"
}

# Configure environment for cloud providers
configure_cloud_providers() {
    show_progress "Configuring cloud provider environment..."
    
    # Update .env file with cloud-specific settings
    if [[ -f ".env" ]]; then
        # Uncomment cloud provider API key lines
        sed -i.bak 's/# \(OPENAI_API_KEY\)/\1/' .env 2>/dev/null || true
        sed -i.bak 's/# \(ANTHROPIC_API_KEY\)/\1/' .env 2>/dev/null || true
        sed -i.bak 's/# \(GEMINI_API_KEY\)/\1/' .env 2>/dev/null || true
        sed -i.bak 's/# \(MISTRAL_API_KEY\)/\1/' .env 2>/dev/null || true
        sed -i.bak 's/# \(PERPLEXITY_API_KEY\)/\1/' .env 2>/dev/null || true
        
        # Set default provider to OpenAI
        sed -i.bak 's/DEFAULT_PROVIDER=ollama/DEFAULT_PROVIDER=openai/' .env 2>/dev/null || true
        sed -i.bak 's/DEFAULT_MODEL=cogito:latest/DEFAULT_MODEL=gpt-4o/' .env 2>/dev/null || true
        
        rm -f .env.bak 2>/dev/null || true
    fi
    
    show_info "Cloud provider configuration updated in .env file"
    echo ""
    echo -e "${CYAN}To use cloud providers, set your API keys:${NC}"
    echo "  export OPENAI_API_KEY='your-key-here'"
    echo "  export ANTHROPIC_API_KEY='your-key-here'"
    echo "  export GEMINI_API_KEY='your-key-here'"
    echo ""
    echo -e "${CYAN}Or edit the .env file and uncomment the appropriate lines.${NC}"
}

# Setup local provider (Ollama)
setup_local_provider() {
    show_progress "Setting up local provider (Ollama)..."
    
    install_ollama
    start_ollama
    install_ollama_models
    
    # Verify Ollama setup
    if ollama list >/dev/null 2>&1; then
        local model_count
        model_count=$(ollama list | grep -c ":" | tail -1)
        show_success "Local provider setup complete ($model_count models available)"
    else
        error_exit "Ollama setup verification failed"
    fi
    
    echo ""
    show_info "Local provider ready! You can now use Lantae with local models."
    echo -e "${GREEN}  Default model: cogito:latest${NC}"
    echo -e "${GREEN}  Alternative models available via: ./lantae -m <model-name>${NC}"
}

# Setup cloud providers
setup_cloud_providers() {
    show_progress "Setting up cloud providers..."
    
    configure_cloud_providers
    
    show_success "Cloud provider setup complete"
    echo ""
    show_warning "Remember to set your API keys before using cloud providers!"
    echo ""
    echo -e "${YELLOW}Quick setup:${NC}"
    echo "  1. Get API keys from your preferred providers"
    echo "  2. Set them as environment variables or edit .env file"
    echo "  3. Run: ./lantae -p <provider> -m <model>"
    echo ""
    echo -e "${CYAN}Supported providers:${NC}"
    echo "  • OpenAI (gpt-4o, gpt-4o-mini, etc.)"
    echo "  • Anthropic (claude-3-5-sonnet, claude-3-haiku, etc.)"
    echo "  • Google Gemini (gemini-pro, gemini-flash, etc.)"
    echo "  • Mistral (mistral-large, mistral-medium, etc.)"
    echo "  • Perplexity (llama-3.1-sonar, etc.)"
}

# Setup hybrid configuration
setup_hybrid_providers() {
    show_progress "Setting up hybrid provider configuration..."
    
    # Setup both local and cloud
    setup_local_provider
    configure_cloud_providers
    
    show_success "Hybrid provider setup complete"
    echo ""
    echo -e "${PURPLE}🌐 Hybrid Setup Complete!${NC}"
    echo ""
    echo -e "${CYAN}You now have access to both:${NC}"
    echo "  • 🏠 Local models via Ollama (no API costs)"
    echo "  • ☁️  Cloud providers (when API keys are set)"
    echo ""
    echo -e "${YELLOW}Usage examples:${NC}"
    echo "  ./lantae                           # Use default (Ollama cogito)"
    echo "  ./lantae -p openai -m gpt-4o      # Use OpenAI GPT-4"
    echo "  ./lantae -p ollama -m qwq:32b     # Use different local model"
}

# Setup minimal configuration
setup_minimal_providers() {
    show_progress "Setting up minimal provider configuration..."
    
    # Just ensure the .env file exists with all options commented
    if [[ ! -f ".env" ]]; then
        error_exit ".env file not found. Run setup-application.sh first."
    fi
    
    show_success "Minimal provider setup complete"
    echo ""
    echo -e "${BLUE}📦 Minimal Setup Complete!${NC}"
    echo ""
    echo -e "${CYAN}Next steps to configure providers:${NC}"
    echo ""
    echo -e "${YELLOW}For local usage:${NC}"
    echo "  1. Install Ollama: curl -fsSL https://ollama.com/install.sh | sh"
    echo "  2. Start service: ollama serve"
    echo "  3. Pull a model: ollama pull cogito:latest"
    echo ""
    echo -e "${YELLOW}For cloud usage:${NC}"
    echo "  1. Get API keys from your preferred provider"
    echo "  2. Set environment variables or edit .env file"
    echo "  3. Run: ./lantae -p <provider> -m <model>"
}

# Main execution
main() {
    local provider_type="${PROVIDER_TYPE:-minimal}"
    
    show_progress "Setting up providers for type: $provider_type"
    
    case "$provider_type" in
        "local")
            setup_local_provider
            ;;
        "cloud")
            setup_cloud_providers
            ;;
        "hybrid")
            setup_hybrid_providers
            ;;
        "minimal")
            setup_minimal_providers
            ;;
        *)
            error_exit "Unknown provider type: $provider_type"
            ;;
    esac
    
    show_success "Provider setup complete for: $provider_type"
}

main "$@"