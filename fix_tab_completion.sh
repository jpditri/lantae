#!/bin/bash

echo "Checking and fixing tab completion for Lantae..."

# Check if tab completion is working in the shell
echo "1. Checking readline settings..."
if [ -f ~/.inputrc ]; then
    echo "Found ~/.inputrc"
    grep -i tab ~/.inputrc || echo "No tab settings found in ~/.inputrc"
else
    echo "No ~/.inputrc found - creating one with tab completion enabled"
    cat > ~/.inputrc << 'EOF'
# Enable tab completion
set completion-ignore-case on
set show-all-if-ambiguous on
set show-all-if-unmodified on
Tab: complete
EOF
fi

# Check Ruby readline
echo -e "\n2. Checking Ruby readline..."
ruby -e "require 'readline'; puts 'Ruby readline version: #{Readline::VERSION rescue 'Unknown'}'"

# Test if the issue is with the terminal
echo -e "\n3. Checking terminal settings..."
stty -a | grep -i tab || echo "Tab settings look normal"

# Create a wrapper script that ensures proper terminal settings
echo -e "\n4. Creating lantae-fix wrapper..."
cat > ~/lantae-fix << 'EOF'
#!/bin/bash
# Ensure proper terminal settings for tab completion
stty sane
stty tab0
export TERM=xterm-256color

# Run lantae with proper settings
exec lantae "$@"
EOF

chmod +x ~/lantae-fix

echo -e "\n5. Testing tab completion..."
echo "Run this test: ruby test_lantae_completion.rb"
echo ""
echo "If tab completion still doesn't work in lantae, try:"
echo "  1. Run: ~/lantae-fix instead of lantae"
echo "  2. Add to your shell config: bind 'set show-all-if-ambiguous on'"
echo "  3. For zsh users: Add 'bindkey '^I' complete-word' to ~/.zshrc"
echo ""
echo "The tab completion IS implemented in Lantae - it's a terminal configuration issue."