# Enhanced UI Mode for Lantae

The enhanced UI mode provides a split-screen interface with:
- A persistent command input area at the bottom
- A command queue on the right showing pending commands
- An output panel showing results from processed commands
- **Tab completion** for commands, providers, and models

## Usage

Start lantae with the `--enhanced-ui` flag:

```bash
./lantae --enhanced-ui
```

## Features

### Split-Screen Layout
- **Left Panel (65%)**: Main interaction area with command input at bottom
- **Right Panel (35%)**: Command queue and output display

### Tab Completion
The enhanced UI supports intelligent tab completion:

#### Command Completion
- Type `/` and press Tab to see all available commands
- Type `/pr` and press Tab to complete to `/provider`
- Use arrow keys to navigate multiple matches

#### Provider Completion
- Type `/provider ` and press Tab to see all providers
- Type `/provider cl` and press Tab to complete providers starting with "cl"
- Available providers: ollama, openai, anthropic, claude, bedrock, gemini, mistral, perplexity

#### Model Completion
- Type `/model ` and press Tab to see all available models for current provider
- Type `/model claude-3` and press Tab to see matching Claude models
- Model list is dynamically fetched from the current provider

### Command Queue
- Type commands normally - they get queued for processing
- Multiple commands can be queued while others are running
- Commands are processed in parallel when possible

### Available Commands
- `/help` - Show available commands
- `/models` - List available models
- `/provider <name>` - Switch provider (Tab to complete)
- `/model <name>` - Switch model (Tab to complete)
- `/status` - Show active commands
- `/clear` - Clear output panel
- `/cancel <id>` - Cancel a running command (planned)

### Keyboard Shortcuts
- **Tab** - Trigger completion
- **↑↓** - Navigate completion menu or command history
- **←→** - Navigate within completion menu (when active)
- **Enter** - Confirm selection or submit command
- **Esc** - Cancel completion menu
- **Home/End** - Jump to start/end of input
- **Ctrl+C** - Exit

### Example Session

1. Start with enhanced UI:
   ```
   ./lantae --enhanced-ui
   ```

2. Use tab completion to switch providers:
   ```
   Type: /prov[Tab]
   Completes to: /provider 
   Type: cl[Tab]
   Shows: claude, 
   Select: claude
   Result: /provider claude
   ```

3. Use tab completion for models:
   ```
   Type: /model [Tab]
   Shows list of Claude models
   Type: sonnet[Tab]
   Completes to: claude-3-5-sonnet-20241022
   ```

4. Queue multiple commands:
   ```
   /models
   Tell me about Python
   /provider openai
   What is machine learning?
   ```

## Benefits

- **Faster Navigation**: Tab completion reduces typing
- **Discovery**: Easily see available options
- **Non-blocking**: Continue typing while commands process
- **Parallel Processing**: Multiple commands can run simultaneously
- **Visual Feedback**: See queue status and output in real-time
- **Better Organization**: Separate areas for input, queue, and output

## Notes

- Terminal must support ANSI escape codes
- Minimum terminal size: 80x24 recommended
- Resizing terminal will redraw the interface
- Tab completion menu shows up to 10 matches at a time