# 🔧 Lantae LSP Implementation Guide

## Overview

The Lantae Language Server Protocol (LSP) implementation provides intelligent code assistance for all Lantae-generated code and enhances the development experience with AI-powered features.

## Architecture

```
┌─────────────────┐     JSON-RPC      ┌─────────────────┐
│   Editor/IDE    │ ←---------------→ │   LSP Server    │
│  (VS Code, Vim) │                   │  (lantae-lsp)   │
└─────────────────┘                   └─────────────────┘
         ↑                                     ↓
         │                            ┌─────────────────┐
         │                            │ Code Action     │
         │                            │ Provider        │
         │                            └─────────────────┘
         │                                     ↓
         │                            ┌─────────────────┐
         └───────────────────────────→│ AI Providers    │
                                      │ (Ollama, etc)   │
                                      └─────────────────┘
```

## Features

### 1. **Intelligent Code Completion**
- Context-aware completions for all supported languages
- Special AI-powered completions for Lantae-generated files
- Snippet support with placeholders

### 2. **Code Actions**
- **AI-Powered Actions** (for Lantae files):
  - 🤖 Refactor with AI
  - 🚀 Optimize performance
  - 🧪 Generate tests
  - 📝 Add documentation
  - 🔍 Analyze complexity
- **Standard Refactoring**:
  - Extract method
  - Extract variable
  - Rename symbol

### 3. **Diagnostics**
- Real-time syntax checking
- Security issue detection
- Code style violations
- Missing dependencies

### 4. **Hover Information**
- Language-specific documentation
- Lantae generation metadata
- Type information
- Usage examples

### 5. **Document Formatting**
- Language-specific formatters
- Configurable style options
- Preserve Lantae metadata headers

## Usage

### Starting the LSP Server

#### Standalone Mode
```bash
# Direct execution
./bin/lantae-lsp

# With logging
LANTAE_LSP_LOG=/tmp/lantae-lsp.log ./bin/lantae-lsp
```

#### Integrated with Lantae CLI
```bash
# Enable LSP in REPL mode
./lantae --enable-lsp

# With both LSP and MCP
./lantae --enable-lsp --enable-mcp
```

### LSP Commands in REPL

```bash
# Check LSP status
/lsp status

# Analyze a file
/lsp analyze path/to/file.rb

# Format a file
/lsp format path/to/file.py

# Get completions (coming soon)
/lsp complete path/to/file.js:10:5
```

## Editor Configuration

### VS Code

Create `.vscode/settings.json`:
```json
{
  "lantae.lsp.enable": true,
  "lantae.lsp.path": "/path/to/lantae/bin/lantae-lsp",
  "lantae.ai.provider": "ollama",
  "lantae.ai.model": "cogito:latest"
}
```

### Neovim (with nvim-lspconfig)

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

-- Define Lantae LSP
if not configs.lantae then
  configs.lantae = {
    default_config = {
      cmd = {'/path/to/lantae/bin/lantae-lsp'},
      filetypes = {'ruby', 'python', 'javascript', 'typescript', 'go', 'rust', 'java'},
      root_dir = function(fname)
        return lspconfig.util.find_git_ancestor(fname) or vim.fn.getcwd()
      end,
      settings = {},
    },
  }
end

-- Setup
lspconfig.lantae.setup{}
```

### Vim (with coc.nvim)

Add to `coc-settings.json`:
```json
{
  "languageserver": {
    "lantae": {
      "command": "ruby",
      "args": ["/path/to/lantae/bin/lantae-lsp"],
      "filetypes": ["ruby", "python", "javascript", "typescript", "go", "rust", "java"],
      "rootPatterns": [".git/", ".lantae-generated.json"],
      "initializationOptions": {
        "aiProvider": "ollama",
        "aiModel": "cogito:latest"
      }
    }
  }
}
```

### Emacs (with lsp-mode)

```elisp
(use-package lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(ruby-mode . "ruby"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ruby" "/path/to/lantae/bin/lantae-lsp"))
    :activation-fn (lsp-activate-on "ruby" "python" "javascript" "go" "rust" "java")
    :server-id 'lantae-lsp)))
```

## AI Integration

### Refactoring with AI

When you trigger "Refactor with AI" on selected code:

1. Code is sent to the configured AI provider
2. AI analyzes the code for improvements
3. Suggestions include:
   - Better variable names
   - Extracted helper methods
   - Simplified logic
   - Performance optimizations

### Test Generation

The "Generate tests" action:

1. Analyzes the selected code/file
2. Identifies test scenarios
3. Generates comprehensive test suite
4. Uses language-appropriate testing framework:
   - Ruby: RSpec
   - Python: pytest
   - JavaScript: Jest
   - Go: built-in testing
   - Rust: built-in tests

### Performance Optimization

The "Optimize performance" action:

1. Identifies performance bottlenecks
2. Suggests algorithmic improvements
3. Recommends better data structures
4. Provides benchmarking code

## Lantae File Detection

Files are recognized as Lantae-generated when they contain:

1. **Header Comments**:
   ```ruby
   # Generated by Lantae AI v1.0.0 - 2024-01-01T00:00:00Z
   ```

2. **Metadata Markers**:
   ```python
   # _lantae_metadata = {"version": "1.0.0", "tool": "create_file"}
   ```

3. **Context Markers**:
   ```javascript
   // Context: {"tool":"lantae","timestamp":"2024-01-01T00:00:00Z"}
   ```

4. **Metadata File**:
   - Presence of `.lantae-generated.json` in project root

## Security Features

The LSP implementation includes security checks:

1. **Sensitive Data Detection**:
   - Warns about logging passwords/tokens
   - Detects hardcoded credentials
   - Flags insecure practices

2. **Path Validation**:
   - Prevents directory traversal
   - Validates file paths
   - Checks file permissions

3. **Command Injection Prevention**:
   - Sanitizes tool arguments
   - Validates command parameters
   - Restricts executable paths

## Performance Considerations

1. **Lazy Loading**:
   - AI providers loaded on-demand
   - Documents parsed incrementally
   - Diagnostics computed asynchronously

2. **Caching**:
   - Parsed ASTs cached
   - AI responses cached for identical requests
   - Completion items pre-computed

3. **Resource Limits**:
   - Maximum document size: 10MB
   - Concurrent AI requests: 3
   - Request timeout: 30s

## Troubleshooting

### Common Issues

1. **LSP Server Won't Start**:
   ```bash
   # Check if port is in use
   lsof -i :7658
   
   # Enable debug logging
   LANTAE_LSP_LOG=/tmp/lantae-lsp.log ./bin/lantae-lsp
   ```

2. **No AI Features Available**:
   - Ensure AI provider is configured
   - Check API keys are set
   - Verify Ollama is running (for local models)

3. **Slow Performance**:
   - Reduce AI model size
   - Enable response caching
   - Limit concurrent operations

### Debug Mode

Enable verbose logging:
```bash
export LANTAE_LSP_DEBUG=1
export LANTAE_LSP_LOG=/tmp/lantae-lsp-debug.log
./bin/lantae-lsp
```

## Development

### Adding New Language Support

1. Update language detection in `client.rb`:
   ```ruby
   when '.xyz' then 'xyz-lang'
   ```

2. Add syntax patterns in `server.rb`:
   ```ruby
   'xyz-lang' => {
     keywords: ['keyword1', 'keyword2'],
     function_pattern: /pattern/,
     comment_pattern: /comment/
   }
   ```

3. Implement language-specific features:
   - Completions
   - Hover providers
   - Diagnostics

### Adding New Code Actions

1. Define action in `code_actions.rb`:
   ```ruby
   {
     title: '🎯 New Action',
     kind: 'source.newAction',
     command: {
       title: 'New Action',
       command: 'lantae.newAction',
       arguments: [uri, range]
     }
   }
   ```

2. Implement handler:
   ```ruby
   when 'lantae.newAction'
     handle_new_action(arguments[0], arguments[1])
   ```

## Future Enhancements

1. **Semantic Highlighting**:
   - AI-identified code patterns
   - Complexity visualization
   - Security issue highlighting

2. **AI-Powered Debugging**:
   - Suggest breakpoints
   - Explain errors
   - Fix suggestions

3. **Project-Wide Refactoring**:
   - Rename across files
   - Extract modules
   - Restructure projects

4. **Real-Time Collaboration**:
   - Share AI suggestions
   - Collaborative refactoring
   - Code review integration