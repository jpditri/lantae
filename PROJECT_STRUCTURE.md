# Lantae Project Structure

```
lantae/
├── bin/                    # Executable file
│   └── lantae             # Ruby CLI executable
│
├── lib/                    # Library modules
│   └── ruby/              # Ruby modules
│       ├── planning_agent.rb  # Task planning and decomposition
│       ├── task_analyzer.rb   # Static code analysis
│       ├── execution_engine.rb # Task execution with verification
│       ├── task_database.rb   # SQLite task history tracking
│       ├── auto_fixer.rb      # Automatic code fixes
│       ├── mcp_manager.rb     # MCP management functionality
│       └── mcp_client.rb      # MCP client implementation
│
├── docs/                   # Documentation
│   ├── COVERAGE-REPORT.md # Test coverage report
│   ├── CREDITS.md         # Credits and acknowledgments
│   ├── LANTAE-LSP-ENHANCED.md # LSP feature documentation
│   ├── README-LSP.md      # LSP-specific documentation
│   └── TEST-RESULTS.md    # Test execution results
│
├── scripts/                # Utility scripts
│   └── setup-secrets.rb   # Ruby secrets setup
│
├── tests/                  # Test files and outputs
│   ├── test_mcp_simple.rb # MCP functionality tests
│   ├── coverage/          # Coverage reports
│   └── test-*/            # Test output directories
│
├── lantae                  # Main executable script
├── README.md              # Main documentation
├── PROJECT_STRUCTURE.md   # This file
├── Gemfile                # Ruby dependencies
└── Gemfile.lock           # Ruby dependency lock
```

## Directory Purposes

### `/bin`
Contains the main Ruby CLI executable.

### `/lib/ruby`
Houses all Ruby library modules including the planning agent system, MCP support, and analysis tools.

### `/docs`
All documentation files except the main README.

### `/scripts`
Utility scripts for project setup and maintenance.

### `/tests`
Test files and test-related outputs, including coverage reports.

### Root Files
- `lantae` - Main executable script
- `Gemfile` - Ruby dependency management
- `README.md` - Main project documentation