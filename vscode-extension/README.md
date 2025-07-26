# Lantae VS Code Extension

A powerful VS Code extension that integrates Lantae AI assistant directly into your development environment, providing intelligent code assistance, analysis, and chat capabilities.

## Features

### 🤖 AI-Powered Code Assistance
- **Code Explanation**: Select code and get instant explanations
- **Code Optimization**: Receive suggestions for improving your code
- **Test Generation**: Automatically generate comprehensive unit tests
- **Bug Detection**: AI-powered diagnostics to catch potential issues

### 💬 Integrated Chat Interface
- Chat with Lantae directly in VS Code sidebar
- Maintain conversation history
- Export conversations for later reference
- Support for multiple AI providers (Ollama, OpenAI, Anthropic, etc.)

### 🔧 Language Server Protocol Support
- Real-time code analysis and suggestions
- Intelligent auto-completion
- Quick fixes and code actions
- Multi-language support

### ⚡ Multiple Provider Support
- **Ollama**: Local AI models (free, private)
- **OpenAI**: GPT-4, GPT-3.5-turbo
- **Anthropic**: Claude 3 Opus, Sonnet, Haiku
- **Amazon Bedrock**: Enterprise AI models
- **Google Gemini**: Gemini Pro models
- **Mistral**: Mistral Large, Medium, Small

### 🎨 Custom Prompt Language
- Syntax highlighting for `.lantae` files
- Intelligent snippets and auto-completion
- Structured prompt directives (@system, @user, @context, etc.)

## Installation

### From VS Code Marketplace
1. Open VS Code
2. Go to Extensions (Ctrl+Shift+X)
3. Search for "Lantae AI Assistant"
4. Click Install

### Manual Installation
1. Download the `.vsix` file from the releases page
2. Open VS Code
3. Press Ctrl+Shift+P and type "Extensions: Install from VSIX"
4. Select the downloaded file

## Setup

### 1. Install Lantae CLI
```bash
# Install Lantae CLI (required for LSP support)
npm install -g lantae
# or
gem install lantae
```

### 2. Configure API Keys (for cloud providers)
Open VS Code settings and configure your API keys:

- **OpenAI**: Set `OPENAI_API_KEY` environment variable
- **Anthropic**: Set `ANTHROPIC_API_KEY` environment variable
- **Bedrock**: Configure AWS credentials

Alternatively, use the command palette:
1. Press Ctrl+Shift+P
2. Type "Lantae: Configure API Keys"
3. Enter your keys securely

### 3. Start Using Lantae
- Press Ctrl+Alt+L to open chat
- Select code and press Ctrl+Alt+E to explain
- Use the Lantae panel in the sidebar

## Usage

### Basic Chat
1. Open the Lantae chat panel (View → Lantae Chat)
2. Type your question or request
3. Get instant AI responses

### Code Analysis
1. Select any code in your editor
2. Right-click and choose "Explain Selected Code"
3. Or use the keyboard shortcut Ctrl+Alt+E

### Code Optimization
1. Select code you want to optimize
2. Right-click and choose "Optimize Selected Code"
3. Review the suggestions and improvements

### Test Generation
1. Select a function or class
2. Right-click and choose "Generate Tests for Code"
3. Get comprehensive unit tests

### Provider Switching
- Use Command Palette: "Lantae: Switch AI Provider"
- Or click the status bar item showing current provider
- Choose from available providers

## Configuration

### Extension Settings

| Setting | Description | Default |
|---------|-------------|---------|
| `lantae.provider` | Default AI provider | `ollama` |
| `lantae.model` | Default model to use | `cogito:latest` |
| `lantae.temperature` | Response creativity (0-2) | `0.1` |
| `lantae.enableLSP` | Enable Language Server | `true` |
| `lantae.autoStartLSP` | Auto-start LSP on activation | `true` |
| `lantae.enableDiagnostics` | AI-powered diagnostics | `true` |
| `lantae.enableCompletion` | AI code completion | `true` |
| `lantae.chatViewLocation` | Chat interface location | `sidebar` |

### Keyboard Shortcuts

| Shortcut | Command | Description |
|----------|---------|-------------|
| Ctrl+Alt+L | Chat with Lantae | Open chat interface |
| Ctrl+Alt+E | Explain Code | Explain selected code |
| Ctrl+Alt+O | Optimize Code | Optimize selected code |
| Ctrl+Alt+T | Generate Tests | Generate tests for code |

## Lantae Prompt Files

The extension supports `.lantae` files with special syntax for structured prompts:

```lantae
@provider openai
@model gpt-4
@temperature 0.1

@system
You are a helpful coding assistant.

@user
Explain this Python function:

```python
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)
```

@context
This is part of a algorithms tutorial series.
```

### Prompt Directives

- `@provider` - Set AI provider
- `@model` - Set AI model
- `@temperature` - Set response creativity
- `@system` - System message
- `@user` - User message
- `@assistant` - Assistant response
- `@context` - Additional context
- `@format` - Output format (json, markdown, etc.)
- `@style` - Response style (formal, casual, etc.)

## LSP Features

When LSP is enabled, you get:

### Real-time Analysis
- Code issue detection as you type
- Performance optimization suggestions
- Security vulnerability warnings

### Smart Completions
- Context-aware code suggestions
- AI-powered auto-completion
- Multi-language support

### Quick Fixes
- Automatic issue resolution
- Code refactoring suggestions
- Best practice recommendations

## Commands

Access via Command Palette (Ctrl+Shift+P):

- `Lantae: Chat` - Open chat interface
- `Lantae: Explain Selected Code` - Explain code
- `Lantae: Optimize Selected Code` - Optimize code
- `Lantae: Generate Tests` - Create unit tests
- `Lantae: Switch Provider` - Change AI provider
- `Lantae: Switch Model` - Change AI model
- `Lantae: Show Status` - Display connection status
- `Lantae: Start LSP Server` - Start language server
- `Lantae: Stop LSP Server` - Stop language server
- `Lantae: Restart LSP Server` - Restart language server

## Troubleshooting

### LSP Server Won't Start
1. Ensure Lantae CLI is installed: `lantae --version`
2. Check if port 7777 is available
3. Try restarting VS Code
4. Check Output panel → Lantae for error messages

### Provider Connection Issues
1. Verify API keys are set correctly
2. Check internet connection for cloud providers
3. For Ollama, ensure server is running: `ollama serve`
4. Check provider status in status bar

### Performance Issues
1. Disable real-time diagnostics if too slow
2. Reduce temperature for faster responses
3. Use local models (Ollama) for better performance
4. Limit conversation history in chat

### Common Error Messages

**"Lantae CLI not found"**
- Install Lantae CLI: `npm install -g lantae`
- Ensure CLI is in PATH

**"Provider connection failed"**
- Check API keys and network connection
- Verify provider service is running

**"LSP timeout"**
- Increase timeout in settings
- Check system resources
- Try restarting LSP server

## Contributing

We welcome contributions! See our [Contributing Guide](CONTRIBUTING.md) for details.

### Development Setup
```bash
# Clone the repository
git clone https://github.com/jpditri/lantae
cd lantae/vscode-extension

# Install dependencies
npm install

# Open in VS Code
code .

# Press F5 to run extension in debug mode
```

## Privacy

- **Local Models (Ollama)**: All processing happens locally
- **Cloud Providers**: Data is sent to respective API services
- **No Data Collection**: Extension doesn't collect or store personal data
- **Secure Storage**: API keys stored in VS Code's secure storage

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Support

- 📚 [Documentation](https://github.com/jpditri/lantae/docs)
- 🐛 [Report Issues](https://github.com/jpditri/lantae/issues)
- 💬 [Discussions](https://github.com/jpditri/lantae/discussions)
- 📧 [Email Support](mailto:support@lantae.ai)

---

**Enjoy coding with AI assistance! 🚀**