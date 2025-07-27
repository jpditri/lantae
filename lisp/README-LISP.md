# Lantae LISP Implementation

This branch contains the LISP implementation of Lantae, providing a functional programming approach to multi-provider LLM interaction.

## 🔮 LISP Features

### Current Implementation Status
- ✅ **Basic Provider Support**: Ollama integration
- ✅ **REPL Interface**: Interactive LISP environment
- ✅ **Mission Abort System**: Safety mechanisms
- ✅ **Configuration Management**: Provider settings
- ⚠️ **Partial Feature Parity**: See [Feature Parity Status](#feature-parity-status)

### LISP-Specific Advantages
- **Functional Programming**: Pure functions and immutable data
- **S-Expression Syntax**: Natural for AI command processing
- **Macro System**: Extensible command definitions
- **Interactive Development**: Live code modification

## 🚀 Quick Start

### Prerequisites
- **Steel Bank Common Lisp (SBCL)** or compatible LISP implementation
- **Quicklisp** for dependency management
- **Ollama** running locally (for default provider)

### Installation
```bash
# Clone the LISP implementation
git clone -b lisp-implementation https://github.com/jpditri/lantae-cli.git
cd lantae-cli/lisp

# Start LISP REPL
sbcl --load start-repl.lisp

# Or run interactively
sbcl --load run-interactive.lisp
```

## 📖 Usage

### Basic Commands
```lisp
;; Start Lantae LISP environment
(load "lantae.lisp")

;; Set provider
(set-provider :ollama)

;; Send prompt
(chat "Hello, how are you?")

;; Interactive mode
(start-repl)
```

### LISP-Specific Features
```lisp
;; Functional composition
(-> "Explain quantum computing"
    (with-provider :ollama)
    (with-model "cogito:latest")
    (send-prompt))

;; S-expression commands
(lantae-command 
  :provider :ollama
  :model "cogito:latest"
  :prompt "Write a LISP function")

;; Macro-based shortcuts
(quick-ask "What is the meaning of life?")
```

## 🔧 Configuration

### LISP Configuration File
```lisp
;; config/config.lisp
(defparameter *default-provider* :ollama)
(defparameter *default-model* "cogito:latest")
(defparameter *ollama-host* "http://localhost:11434")

;; Provider configurations
(defparameter *providers*
  '((:ollama 
     :host "http://localhost:11434"
     :timeout 30)
    (:openai
     :api-key-env "OPENAI_API_KEY"
     :base-url "https://api.openai.com/v1")))
```

## 🏗️ Architecture

### File Structure
```
lisp/
├── lantae.lisp           # Main entry point
├── start-repl.lisp       # REPL startup
├── run-interactive.lisp  # Interactive runner
└── src/
    ├── cli/
    │   └── commands.lisp  # Command definitions
    ├── config/
    │   └── config.lisp    # Configuration management
    ├── providers/
    │   └── providers.lisp # Provider implementations
    └── utils/
        ├── mission-abort.lisp # Safety systems
        └── utils.lisp     # Utility functions
```

### Core Modules
- **Commands**: LISP command processor
- **Providers**: Multi-provider interface
- **Config**: Configuration management
- **Utils**: Utility functions and mission abort
- **Mission Abort**: Safety and control systems

## 🔄 Feature Parity Status

See the main [Feature Parity Document](../docs/FEATURE_PARITY.md) for detailed status compared to other implementations.

### LISP Implementation Checklist
- [ ] **Multi-Provider Support** (Ollama ✅, OpenAI ❌, Anthropic ❌, Gemini ❌)
- [ ] **Interactive REPL** (Basic ✅, Advanced ❌)
- [ ] **Tool Integration** (❌)
- [ ] **MCP Support** (❌)
- [ ] **LSP Integration** (❌)
- [ ] **System PATH Installation** (❌)
- [ ] **Configuration Management** (Basic ✅, Advanced ❌)
- [ ] **Uninstall System** (❌)
- [ ] **Planning Agent** (❌)
- [ ] **Streaming Responses** (❌)

## 🛠️ Development

### Adding New Features
```lisp
;; Define new command
(defun new-command (args)
  "New command implementation"
  (format t "Executing new command with args: ~A~%" args))

;; Register command
(register-command 'new-command "new" "New command description")
```

### Testing
```lisp
;; Load test suite
(load "tests/test-suite.lisp")

;; Run tests
(run-all-tests)
```

## 🤝 Contributing

### LISP-Specific Guidelines
1. **Follow Common Lisp conventions**
2. **Use meaningful function names**
3. **Document all public functions**
4. **Maintain functional programming principles**
5. **Update feature parity document**

### Sync with Main Implementation
Before adding features, check the [Feature Parity Document](../docs/FEATURE_PARITY.md) to ensure consistency across language implementations.

## 📚 Resources

- [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)
- [Practical Common Lisp](http://www.gigamonkeys.com/book/)
- [SBCL Manual](http://www.sbcl.org/manual/)
- [Quicklisp](https://www.quicklisp.org/)

---

*This LISP implementation aims for feature parity with the main Ruby implementation while embracing LISP's unique strengths.*