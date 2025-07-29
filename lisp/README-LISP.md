# Lantae LISP Implementation

This is the Common Lisp implementation of Lantae, providing a functional programming approach to multi-provider LLM interaction.

## ✨ Features

### Implemented
- ✅ **Interactive REPL** with S-expression support
- ✅ **Ollama Provider** with full API integration
- ✅ **HTTP Client** with retry logic and error handling
- ✅ **Command System** with slash commands
- ✅ **Colored Output** with ANSI escape codes
- ✅ **Configuration Management** with plist-based config
- ✅ **Conversation History** tracking

### In Progress
- 🔄 **Additional Providers** (OpenAI, Anthropic)
- 🔄 **Tool Integration** (file operations, code execution)
- 🔄 **Tab Completion** for commands
- 🔄 **Streaming Responses**

### Planned
- 📋 **MCP Protocol** support
- 📋 **LSP Integration**
- 📋 **Planning Agent**
- 📋 **Enhanced UI Mode**

## 🚀 Quick Start

### Prerequisites
- **SBCL** (Steel Bank Common Lisp) or **CCL**
- **Quicklisp** package manager
- **Ollama** running locally (for local models)

### Installation

#### Option 1: Quick Start Script
```bash
# Clone and run
git clone -b lisp-implementation https://github.com/jpditri/lantae-cli.git
cd lantae-cli/lisp
./start.sh
```

#### Option 2: Using Make
```bash
# Install dependencies and build
make deps
make run
```

#### Option 3: Manual Setup
```bash
# Install SBCL (if not installed)
brew install sbcl  # macOS
# or
sudo apt-get install sbcl  # Ubuntu

# Install Quicklisp
curl -o quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
* (quicklisp-quickstart:install)
* (ql:add-to-init-file)
* (quit)

# Load Lantae
sbcl --load load-lantae.lisp
* (lantae:start-repl)
```

## 📖 Usage

### Interactive REPL
```lisp
;; Start REPL
(lantae:start-repl)

;; Or from command line
./start.sh
```

### Available Commands
- `/help` - Show available commands
- `/provider [name]` - Switch provider
- `/model [name]` - Switch model
- `/models` - List available models
- `/clear` - Clear conversation history
- `/info` - Show current configuration
- `/env` - Check environment variables
- `/history` - Show conversation history
- `/temperature [0.0-2.0]` - Set response temperature
- `/quit` or `/exit` - Exit REPL

### S-Expression Support
```lisp
;; Execute LISP code directly
> (+ 1 2 3)
6

;; Access configuration
> (lantae:get-config :model)
"cogito:latest"

;; Set configuration
> (lantae:set-config :temperature 0.5)
```

### Chat Examples
```
> What is functional programming?
🤖 Thinking...
Functional programming is a programming paradigm that treats computation as the evaluation of mathematical functions...

> /model llama3:latest
Switched to model: llama3:latest

> Explain LISP macros
🤖 Thinking...
LISP macros are one of the most powerful features of the language...
```

## 🏗️ Architecture

### Module Structure
```
lisp/
├── lantae.lisp          # Main entry point
├── load-lantae.lisp     # Module loader
├── Makefile            # Build system
├── start.sh            # Quick start script
└── src/
    ├── utils/
    │   ├── utils.lisp       # General utilities
    │   ├── http-client.lisp # HTTP client wrapper
    │   ├── colors.lisp      # ANSI color support
    │   └── mission-abort.lisp
    ├── config/
    │   └── config.lisp      # Configuration management
    ├── providers/
    │   ├── providers.lisp   # Provider abstraction
    │   └── ollama-provider.lisp
    └── cli/
        └── commands.lisp    # Command system
```

### Key Design Principles
1. **Functional First** - Pure functions, immutable data
2. **Monadic Error Handling** - Result types for error propagation
3. **Macro-based DSL** - Lisp macros for configuration and commands
4. **Provider Abstraction** - Higher-order functions for providers
5. **Lazy Evaluation** - Stream processing where appropriate

## 🔧 Development

### Running Tests
```bash
make test
```

### Building Executable
```bash
make build
# Creates build/lantae-lisp executable
```

### Adding New Providers
```lisp
(defun make-my-provider (&key api-key)
  (lantae-providers:create-provider
   :name "myprovider"
   :chat-fn (lambda (model messages temperature)
             (my-chat-implementation api-key model messages temperature))
   :models-fn #'my-list-models
   :config `(:api-key ,api-key)))
```

### Adding New Commands
```lisp
(lantae-commands:register-command "mycommand" #'my-command-fn
  :description "My custom command"
  :usage "/mycommand [args]"
  :completions-fn #'my-completions-fn)
```

## 🐛 Troubleshooting

### Common Issues

1. **Quicklisp not found**
   ```bash
   curl -o quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
   sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)'
   ```

2. **Dependencies not loading**
   ```lisp
   (ql:quickload '(:drakma :cl-json :flexi-streams))
   ```

3. **Ollama connection failed**
   - Ensure Ollama is running: `ollama serve`
   - Check connection: `curl http://localhost:11434/api/tags`

4. **Colors not working**
   - Check terminal support: `echo $TERM`
   - Force colors: `export COLORTERM=1`

## 🎯 LISP-Specific Features

### S-Expression Configuration
```lisp
;; Configure via S-expressions
(lantae:with-config ((:temperature 0.8)
                    (:model "codellama:latest"))
  (lantae:process-chat-message "Write a quicksort function"))
```

### Functional Provider Composition
```lisp
;; Chain providers with fallback
(defparameter *my-provider*
  (lantae-providers:fallback-provider "openai" "ollama"))

;; Add logging to provider
(defparameter *logged-provider*
  (lantae-providers:with-logging "ollama"))
```

### Macro-based Extensions
```lisp
;; Define custom command macro
(defmacro defcommand (name args &body body)
  `(lantae-commands:register-command 
    ,(string-downcase (string name))
    (lambda ,args ,@body)))

(defcommand test (args)
  (format t "Test command with args: ~A~%" args))
```

## 🔗 Resources

- [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/)
- [Quicklisp Libraries](https://www.quicklisp.org/beta/releases.html)
- [SBCL Manual](http://www.sbcl.org/manual/)
- [Practical Common Lisp](http://www.gigamonkeys.com/book/)