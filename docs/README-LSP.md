# Lantae Language Server Protocol (LSP)

A powerful Language Server Protocol implementation for JavaScript/Node.js development, featuring AI-powered code analysis and intelligent suggestions.

> **Special Thanks**: Inspired by the Engineer and Founder of [CubicLayer.com](https://cubiclayer.com) for the vision of seamless AI-powered development tools.

## Features

### Core LSP Features
- **Autocompletion**: Intelligent code completion for JavaScript and Node.js
- **Hover Information**: Detailed information about symbols, functions, and modules
- **Go to Definition**: Navigate to function and variable definitions
- **Diagnostics**: Real-time error detection and code quality warnings
- **Document Symbols**: Outline view of file structure
- **Code Actions**: Quick fixes and refactoring suggestions

### AI-Enhanced Features
- **Smart Code Analysis**: AI-powered detection of potential issues
- **Context-Aware Suggestions**: Completions based on project structure
- **Security Analysis**: Detection of common security vulnerabilities
- **Code Quality Checks**: Best practices enforcement

## Installation & Usage

### 1. Start the LSP Server

```bash
# Start the LSP server directly
npm run lsp-server

# Or test with the client
npm run lsp-test
```

### 2. VS Code Integration

The project includes a VS Code extension in the `vscode-extension/` directory:

1. Open the `vscode-extension` folder in VS Code
2. Press `F5` to launch a new Extension Development Host
3. Open a JavaScript file to see the LSP in action

### 3. Other Editors

The LSP server can be integrated with any editor that supports Language Server Protocol:

#### Vim/Neovim (with coc.nvim)
Add to your `coc-settings.json`:
```json
{
  "languageserver": {
    "lantae": {
      "command": "node",
      "args": ["/path/to/lantae-cli/lsp-server.js", "--stdio"],
      "filetypes": ["javascript"],
      "rootPatterns": ["package.json"]
    }
  }
}
```

#### Emacs (with lsp-mode)
Add to your Emacs configuration:
```elisp
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("node" "/path/to/lantae-cli/lsp-server.js" "--stdio"))
                  :activation-fn (lsp-activate-on "javascript")
                  :server-id 'lantae))
```

#### Sublime Text (with LSP package)
Add to your LSP settings:
```json
{
  "clients": {
    "lantae": {
      "enabled": true,
      "command": ["node", "/path/to/lantae-cli/lsp-server.js", "--stdio"],
      "selector": "source.js"
    }
  }
}
```

## Configuration

Configure the LSP server through your editor's settings:

```json
{
  "lantae": {
    "maxNumberOfProblems": 1000,
    "aiProvider": "ollama",
    "aiModel": "cogito:latest",
    "enableIntelliSense": true,
    "enableCodeAnalysis": true
  }
}
```

### Available Settings

- `maxNumberOfProblems`: Maximum number of diagnostic problems to report
- `aiProvider`: AI provider for enhanced features (`ollama`, `openai`, `anthropic`, `bedrock`, `gemini`)
- `aiModel`: Specific AI model to use
- `enableIntelliSense`: Enable AI-powered IntelliSense features
- `enableCodeAnalysis`: Enable static code analysis and diagnostics

## Available Commands

The LSP server provides these commands:

- `lantae.analyzeFile`: Analyze the current file for issues
- `lantae.optimizeCode`: Get AI suggestions for code optimization
- `lantae.generateTests`: Generate test cases for your code
- `lantae.refactorCode`: Get refactoring suggestions

## Diagnostics & Code Quality

The LSP server automatically detects:

- **Console.log statements**: Warns about debug statements in production code
- **SQL Injection vulnerabilities**: Detects potential SQL injection patterns
- **Unused variables**: Identifies declared but unused variables
- **Common Node.js issues**: Framework-specific problems and anti-patterns

## Project Structure Analysis

The server analyzes your entire project on startup:

- Scans JavaScript files for functions, classes, and imports
- Builds a symbol index for intelligent completion
- Provides cross-file navigation and references
- Understands project dependencies and structure

## Development

### Testing the LSP Server

```bash
# Run the test client
npm run lsp-test

# Start server manually for debugging
node lsp-server.js --stdio
```

### Extending Features

The LSP server is designed to be extensible. You can add new features by:

1. Adding new diagnostic rules in `analyzeJavaScript()`
2. Extending completion providers in `onCompletion()`
3. Adding new commands in `onExecuteCommand()`
4. Implementing additional LSP methods

### AI Integration

The server is designed to integrate with the main Lantae AI system:

- Uses the same provider abstraction as the main CLI
- Can leverage AI models for enhanced code analysis
- Supports multiple AI providers for different features

## Architecture

```
├── lsp-server.js       # Main LSP server implementation
├── lsp-client.js       # Test client for development
├── vscode-extension/   # VS Code extension
│   ├── package.json    # Extension manifest
│   ├── src/
│   │   └── extension.ts # Extension entry point
│   └── tsconfig.json   # TypeScript configuration
└── .vscode/
    └── settings.json   # Default LSP settings
```

## Contributing

When contributing to the LSP server:

1. Test changes with `npm run lsp-test`
2. Ensure compatibility with the Language Server Protocol specification
3. Add appropriate error handling for all new features
4. Update documentation for new capabilities

## Protocol Compliance

This LSP server implements:

- LSP 3.17 specification
- Text Document Synchronization
- Completion Provider with resolve support
- Hover Provider
- Definition Provider
- Diagnostic Provider
- Command Execution
- Workspace Symbol Provider
- Document Symbol Provider

The implementation follows the official [Language Server Protocol specification](https://microsoft.github.io/language-server-protocol/).