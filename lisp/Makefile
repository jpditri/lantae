# Makefile for Lantae LISP implementation

# Variables
LISP ?= sbcl
QUICKLISP_URL = https://beta.quicklisp.org/quicklisp.lisp
BUILD_DIR = build
SRC_DIR = src
EXECUTABLE = lantae-lisp

# Color output
RED = \033[0;31m
GREEN = \033[0;32m
YELLOW = \033[1;33m
BLUE = \033[0;34m
NC = \033[0m # No Color

.PHONY: all deps quicklisp build clean test run help install

# Default target
all: deps build

# Help target
help:
	@echo "$(BLUE)Lantae LISP Makefile$(NC)"
	@echo ""
	@echo "Available targets:"
	@echo "  $(GREEN)make all$(NC)        - Install dependencies and build"
	@echo "  $(GREEN)make deps$(NC)       - Install Common Lisp dependencies"
	@echo "  $(GREEN)make quicklisp$(NC)  - Install Quicklisp package manager"
	@echo "  $(GREEN)make build$(NC)      - Build standalone executable"
	@echo "  $(GREEN)make run$(NC)        - Run Lantae LISP REPL"
	@echo "  $(GREEN)make test$(NC)       - Run tests"
	@echo "  $(GREEN)make clean$(NC)      - Clean build artifacts"
	@echo "  $(GREEN)make install$(NC)    - Install to system"

# Check if SBCL is installed
check-sbcl:
	@command -v $(LISP) >/dev/null 2>&1 || { \
		echo "$(RED)Error: $(LISP) is not installed$(NC)"; \
		echo "Please install SBCL:"; \
		echo "  macOS:  brew install sbcl"; \
		echo "  Ubuntu: sudo apt-get install sbcl"; \
		echo "  Arch:   sudo pacman -S sbcl"; \
		exit 1; \
	}
	@echo "$(GREEN)✓ SBCL found$(NC)"

# Install Quicklisp if not present
quicklisp: check-sbcl
	@if [ ! -f ~/quicklisp/setup.lisp ]; then \
		echo "$(YELLOW)Installing Quicklisp...$(NC)"; \
		curl -o /tmp/quicklisp.lisp $(QUICKLISP_URL); \
		echo '(load "/tmp/quicklisp.lisp")' | $(LISP) --non-interactive; \
		echo '(quicklisp-quickstart:install)' | $(LISP) --non-interactive; \
		echo '(ql:add-to-init-file)' | $(LISP) --non-interactive; \
		rm -f /tmp/quicklisp.lisp; \
		echo "$(GREEN)✓ Quicklisp installed$(NC)"; \
	else \
		echo "$(GREEN)✓ Quicklisp already installed$(NC)"; \
	fi

# Install dependencies
deps: quicklisp
	@echo "$(YELLOW)Installing Common Lisp dependencies...$(NC)"
	@$(LISP) --non-interactive --eval '(ql:quickload :drakma :silent t)' --quit || echo "$(YELLOW)Warning: drakma not installed$(NC)"
	@$(LISP) --non-interactive --eval '(ql:quickload :cl-json :silent t)' --quit || echo "$(YELLOW)Warning: cl-json not installed$(NC)"
	@$(LISP) --non-interactive --eval '(ql:quickload :flexi-streams :silent t)' --quit || echo "$(YELLOW)Warning: flexi-streams not installed$(NC)"
	@$(LISP) --non-interactive --eval '(ql:quickload :bordeaux-threads :silent t)' --quit || echo "$(YELLOW)Warning: bordeaux-threads not installed$(NC)"
	@$(LISP) --non-interactive --eval '(ql:quickload :cl-ppcre :silent t)' --quit || echo "$(YELLOW)Warning: cl-ppcre not installed$(NC)"
	@echo "$(GREEN)✓ Dependencies installed$(NC)"

# Create ASDF system definition
lantae.asd:
	@echo "$(YELLOW)Creating ASDF system definition...$(NC)"
	@cat > lantae.asd << 'EOF'
;;;; lantae.asd - ASDF system definition for Lantae LISP

(asdf:defsystem #:lantae
  :description "Multi-Provider LLM Interface - LISP Implementation"
  :author "Lantae Team"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:drakma
               #:cl-json
               #:flexi-streams
               #:bordeaux-threads
               #:cl-ppcre)
  :components ((:module "src"
                :components
                ((:module "utils"
                  :components
                  ((:file "utils")
                   (:file "http-client")
                   (:file "colors")
                   (:file "mission-abort")))
                 (:module "config"
                  :components
                  ((:file "config")))
                 (:module "providers"
                  :components
                  ((:file "providers")
                   (:file "ollama-provider")))
                 (:module "cli"
                  :components
                  ((:file "commands")))))
               (:file "lantae")))
EOF
	@echo "$(GREEN)✓ ASDF system definition created$(NC)"

# Build executable
build: deps lantae.asd
	@echo "$(YELLOW)Building Lantae LISP executable...$(NC)"
	@mkdir -p $(BUILD_DIR)
	@$(LISP) --non-interactive \
		--eval '(require :asdf)' \
		--eval '(push #p"./" asdf:*central-registry*)' \
		--eval '(ql:quickload :lantae :silent t)' \
		--eval '(sb-ext:save-lisp-and-die "$(BUILD_DIR)/$(EXECUTABLE)" :toplevel (function lantae:main) :executable t)' \
		|| { echo "$(RED)Build failed$(NC)"; exit 1; }
	@echo "$(GREEN)✓ Built $(BUILD_DIR)/$(EXECUTABLE)$(NC)"

# Run in development mode
run: check-sbcl
	@echo "$(BLUE)Starting Lantae LISP REPL...$(NC)"
	@$(LISP) --load quicklisp.lisp --load lantae.lisp --eval '(lantae:start-repl)'

# Run tests
test: deps
	@echo "$(YELLOW)Running tests...$(NC)"
	@if [ -f run-tests.lisp ]; then \
		$(LISP) --non-interactive --load run-tests.lisp; \
	else \
		echo "$(YELLOW)No tests found$(NC)"; \
	fi

# Clean build artifacts
clean:
	@echo "$(YELLOW)Cleaning build artifacts...$(NC)"
	@rm -rf $(BUILD_DIR)
	@rm -f *.fasl */*.fasl */*/*.fasl
	@rm -f lantae.asd
	@echo "$(GREEN)✓ Clean complete$(NC)"

# Install to system
install: build
	@echo "$(YELLOW)Installing Lantae LISP...$(NC)"
	@sudo cp $(BUILD_DIR)/$(EXECUTABLE) /usr/local/bin/
	@sudo chmod +x /usr/local/bin/$(EXECUTABLE)
	@echo "$(GREEN)✓ Installed to /usr/local/bin/$(EXECUTABLE)$(NC)"

# Development helpers
repl: check-sbcl
	@$(LISP)

format:
	@echo "$(YELLOW)Formatting Lisp files...$(NC)"
	@find . -name "*.lisp" -exec $(LISP) --non-interactive \
		--eval '(require :format)' \
		--eval '(format-file "{}")' \
		--quit \; 2>/dev/null || echo "$(YELLOW)Formatting not available$(NC)"

# Quick start script
quickstart:
	@echo "#!/bin/bash" > quickstart.sh
	@echo "# Lantae LISP Quick Start" >> quickstart.sh
	@echo "" >> quickstart.sh
	@echo "echo 'Setting up Lantae LISP...'" >> quickstart.sh
	@echo "make deps" >> quickstart.sh
	@echo "make build" >> quickstart.sh
	@echo "echo ''" >> quickstart.sh
	@echo "echo 'Lantae LISP is ready!'" >> quickstart.sh
	@echo "echo 'Run ./build/lantae-lisp to start'" >> quickstart.sh
	@chmod +x quickstart.sh
	@echo "$(GREEN)✓ Created quickstart.sh$(NC)"

.SILENT: help check-sbcl