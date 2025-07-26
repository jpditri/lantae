# Lantae LISP Version Quick Start Guide

## Prerequisites

1. **SBCL (Steel Bank Common Lisp)** - Already installed on your system at `/opt/homebrew/bin/sbcl`
2. **Quicklisp** (optional) - For advanced package management

## Running the LISP Version

### Method 1: Using the Launcher Script (Recommended)

```bash
# Make the script executable (already done)
chmod +x bin/lantae-lisp

# Run the LISP version
./bin/lantae-lisp
```

### Method 2: Using the Development REPL

```bash
# Navigate to the lisp directory
cd lisp

# Start SBCL with the REPL script
sbcl --load start-repl.lisp
```

### Method 3: Direct SBCL Interactive Session

```bash
cd lisp
sbcl --load run-interactive.lisp
```

## Using the LISP REPL

Once started, you'll see:

```
╔══════════════════════════════════════════════════════════════╗
║  ██╗      █████╗ ███╗   ██╗████████╗ █████╗ ███████╗  ║
║  ...                                                        ║
║  🚀 Multi-Provider LLM Interface v1.0.0-lisp (LISP Edition)  ║
╚══════════════════════════════════════════════════════════════╝

Provider: ollama | Model: cogito:latest
Type "(help)" for commands, "(quit)" to exit

>
```

### Commands

The LISP version uses a command-based interface. Commands are invoked by typing their name:

- `help` - Show available commands
- `provider [name] [model]` - Switch provider or show current
- `model <name>` - Switch model
- `config [key] [value]` - View/set configuration
- `clear` - Clear conversation history
- `info` - Show system information
- `quit` or `exit` - Exit the REPL

### Example Session

```lisp
> help
Available commands:

  help      - Show help for commands
  provider  - Switch provider or show current provider
  model     - Switch to a different model
  config    - Show or set configuration values
  clear     - Clear conversation history
  info      - Show system information
  quit      - Exit the REPL

> provider
Current provider: ollama
Current model: cogito:latest

> model qwq:32b
Switched to model: qwq:32b

> What is the capital of France?
[AI response will appear here]

> quit
Goodbye!
```

### Development Mode

For development and testing, use the `start-repl.lisp` script which provides additional functions:

```lisp
(reload)        - Reload all modules
(test-provider) - Test provider system
(test-config)   - Test configuration system
(test-commands) - Test command system
```

## Architecture

The LISP version implements:

- **Functional Programming**: Pure functions, higher-order functions, and immutable data
- **S-expression Configuration**: Native LISP configuration format
- **Macro-based Commands**: Powerful command definition system
- **Provider Abstraction**: Functional provider interface with composition support

## Differences from Ruby Version

1. **Command Syntax**: Commands don't use `/` prefix
2. **Configuration**: Uses S-expressions instead of JSON/YAML
3. **Extension**: Uses macros instead of classes
4. **Error Handling**: Monadic patterns vs exceptions

## Troubleshooting

### Common Issues

1. **"end of file" error**: This occurs when running non-interactively. Use one of the interactive methods above.

2. **Command not found**: Commands in LISP version are direct function calls, not slash commands.

3. **Package errors**: Ensure all files are loaded in the correct order (handled by launcher scripts).

4. **Warning messages**: The LISP version may show compilation warnings on first load. These can be safely ignored or suppressed with `2>/dev/null`.

### Debug Mode

To enable debug output:

```lisp
> config debug true
Set debug = true
```

## Next Steps

- Explore the functional provider system in `lisp/src/providers/providers.lisp`
- Create custom commands using the macro system in `lisp/src/cli/commands.lisp`
- Extend configuration with S-expressions in `lisp/src/config/config.lisp`