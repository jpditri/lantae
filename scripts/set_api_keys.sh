#!/bin/bash
# Helper script to set API keys for lantae

echo "🔑 Lantae API Key Setup"
echo "======================"
echo ""
echo "This script will help you set up API keys for lantae."
echo "Your keys will be exported as environment variables."
echo ""

# Function to read API key with optional skip
read_api_key() {
    local provider=$1
    local env_var="${provider^^}_API_KEY"
    
    if [ -n "${!env_var}" ]; then
        echo "✓ $env_var is already set"
        return
    fi
    
    echo -n "Enter your $provider API key (or press Enter to skip): "
    read -s api_key
    echo ""
    
    if [ -n "$api_key" ]; then
        export $env_var="$api_key"
        echo "✓ $env_var set"
    else
        echo "⚠️  Skipped $provider"
    fi
}

# Read API keys for each provider
read_api_key "anthropic"
read_api_key "openai"
read_api_key "gemini"
read_api_key "mistral"
read_api_key "perplexity"

echo ""
echo "🎯 API keys have been set for this session."
echo ""
echo "To make them permanent, add the following to your ~/.bashrc or ~/.zshrc:"
echo ""
echo "# Lantae API Keys"
[ -n "$ANTHROPIC_API_KEY" ] && echo "export ANTHROPIC_API_KEY='$ANTHROPIC_API_KEY'"
[ -n "$OPENAI_API_KEY" ] && echo "export OPENAI_API_KEY='$OPENAI_API_KEY'"
[ -n "$GEMINI_API_KEY" ] && echo "export GEMINI_API_KEY='$GEMINI_API_KEY'"
[ -n "$MISTRAL_API_KEY" ] && echo "export MISTRAL_API_KEY='$MISTRAL_API_KEY'"
[ -n "$PERPLEXITY_API_KEY" ] && echo "export PERPLEXITY_API_KEY='$PERPLEXITY_API_KEY'"
echo ""
echo "Then run: source ~/.bashrc (or source ~/.zshrc)"
echo ""
echo "You can now run lantae with: ./lantae"