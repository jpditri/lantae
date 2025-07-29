#!/bin/bash
# Lantae LISP Quick Start Script

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}Starting Lantae LISP...${NC}"

# Check if SBCL is installed
if ! command -v sbcl &> /dev/null; then
    echo -e "${RED}Error: SBCL is not installed${NC}"
    echo "Please install SBCL first:"
    echo "  macOS:  brew install sbcl"
    echo "  Ubuntu: sudo apt-get install sbcl"
    echo "  Arch:   sudo pacman -S sbcl"
    exit 1
fi

# Check if Quicklisp is installed
if [ ! -f "$HOME/quicklisp/setup.lisp" ]; then
    echo -e "${YELLOW}Quicklisp not found. Installing...${NC}"
    curl -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
    sbcl --non-interactive \
         --load /tmp/quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql:add-to-init-file)' \
         --quit
    rm -f /tmp/quicklisp.lisp
    echo -e "${GREEN}✓ Quicklisp installed${NC}"
fi

# Start Lantae LISP
echo -e "${GREEN}Loading Lantae LISP...${NC}"
sbcl --load load-lantae.lisp \
     --eval '(lantae:start-repl)' \
     --quit