# Lantae Project Structure

```
lantae/
├── bin/                    # Executable files
│   ├── lantae             # Ruby CLI executable
│   └── lantae.js          # Node.js CLI executable
│
├── lib/                    # Library modules
│   ├── ruby/              # Ruby modules
│   │   ├── mcp_manager.rb # MCP management functionality
│   │   └── mcp_client.rb  # MCP client implementation
│   └── node/              # Node.js modules
│       ├── lsp-server.js  # LSP server implementation
│       └── lsp-client.js  # LSP client implementation
│
├── docs/                   # Documentation
│   ├── COVERAGE-REPORT.md # Test coverage report
│   ├── CREDITS.md         # Credits and acknowledgments
│   ├── LANTAE-LSP-ENHANCED.md # LSP feature documentation
│   ├── README-LSP.md      # LSP-specific documentation
│   └── TEST-RESULTS.md    # Test execution results
│
├── scripts/                # Utility scripts
│   ├── setup-secrets.js   # Node.js secrets setup
│   ├── setup-secrets.rb   # Ruby secrets setup
│   └── run-coverage.js    # Coverage runner
│
├── tests/                  # Test files and outputs
│   ├── test-simple.js     # Simple functionality tests
│   ├── test-lantae-lsp.js # LSP integration tests
│   ├── test-lsp-basic.js  # Basic LSP tests
│   ├── test-coverage.js   # Coverage tests
│   ├── test_mcp_simple.rb # MCP functionality tests
│   ├── coverage/          # Coverage reports
│   └── test-*/            # Test output directories
│
├── vscode-extension/       # VS Code extension
│   ├── package.json       # Extension manifest
│   └── tsconfig.json      # TypeScript configuration
│
├── lantae                  # Ruby CLI wrapper script
├── lantae.js              # Node.js CLI wrapper script
├── README.md              # Main documentation
├── PROJECT_STRUCTURE.md   # This file
├── package.json           # Node.js project manifest
├── package-lock.json      # Node.js dependency lock
├── Gemfile                # Ruby dependencies
└── Gemfile.lock           # Ruby dependency lock
```

## Directory Purposes

### `/bin`
Contains the main executable files for both Ruby and Node.js implementations.

### `/lib`
Houses reusable library modules separated by language.

### `/docs`
All documentation files except the main README.

### `/scripts`
Utility and setup scripts for project maintenance.

### `/tests`
Test files and test-related outputs, including coverage reports.

### Root Files
- Wrapper scripts (`lantae`, `lantae.js`) for easy execution
- Configuration files (package.json, Gemfile)
- Main README and project structure documentation