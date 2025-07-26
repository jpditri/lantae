# 🔮 Lantae - Multi-Provider LLM Interface

[![Version](https://img.shields.io/badge/version-1.0.0-blue.svg)](https://github.com/jpditri/lantae)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)
[![Ruby](https://img.shields.io/badge/ruby-3.0+-red.svg)](https://www.ruby-lang.org/)

> **🚀 A powerful CLI/REPL interface for multiple LLM providers with advanced reasoning capabilities**
>
> **Special Thanks**: LSP Enhanced features inspired by the Engineer and Founder of [CubicLayer.com](https://cubiclayer.com)

Lantae provides a unified interface to interact with various Large Language Model providers including Ollama, OpenAI, Anthropic, AWS Bedrock, Google Gemini, Mistral, and Perplexity. Built in Ruby with advanced planning agent capabilities.

## ✨ Features

### 🎯 **Core Capabilities**
- **Multi-Provider Support**: Seamlessly switch between Ollama, OpenAI, Anthropic, Bedrock, Gemini, Mistral, and Perplexity
- **Cogito Reasoning Model**: Optimized for speed (~0.35s) with excellent reasoning quality
- **Interactive REPL**: Full-featured chat interface with conversation history
- **Tool Integration**: Execute local tools (bash, git, file operations) directly from chat
- **AWS Integration**: Secure API key management via AWS Secrets Manager
- **Planning Agent**: Advanced task decomposition and execution with verification

### 🎨 **Enhanced UX**
- **Cool ASCII Banner**: Beautiful colorful startup display
- **Auto-Accept Mode** (`-y`): Automatically confirm actions and prompts
- **Planning Mode**: Force detailed task breakdown for complex requests
- **Agent Mode** (`--agent`): Autonomous task planning and execution
- **Colored Output**: ANSI color support for better readability
- **Mode Indicators**: Visual feedback for active features
- **Tab Autocomplete**: Smart command and argument completion in REPL

### 🛠️ **Available Tools**
- File operations: `cat`, `write_file`, `edit_file`, `create_file`, `delete_file`, `mkdir`
- System commands: `bash`, `pwd`, `ls`, `find`
- Development tools: `git`, `npm`/`bundle`, code execution (`python`, `ruby`, `node`)
- **MCP Integration**: Model Context Protocol support for extensible tool access

## 🚀 Quick Start

### Prerequisites
- **Ruby**: 3.0+ 
- **Ollama**: Running locally for local models
- **AWS CLI**: Configured for Secrets Manager (optional)

### Installation

```bash
# Clone the repository
git clone https://github.com/jpditri/lantae.git
cd lantae

# Install Ruby dependencies
bundle install

# Make executable
chmod +x lantae

# Run with default cogito model
./lantae
```

## 📖 Usage

### Basic Commands

#### Start Interactive Chat
```bash
./lantae
```

#### Single Prompt
```bash
./lantae "Explain quantum computing"
```

#### With Options
```bash
# Auto-accept mode with planning
./lantae --auto-accept --planning-mode "Build a web scraper"

# Different model and provider
./lantae -p openai -m gpt-4o "Analyze this code"

# Disable banner
./lantae --no-banner "Quick question"
```

### Command Line Options

| Option | Flag | Description |
|--------|------|-------------|
| Model | `-m`, `--model` | Specify model (default: cogito:latest) |
| Provider | `-p`, `--provider` | Choose provider (ollama, openai, etc.) |
| Auto-Accept | `-y`, `--auto-accept` | Auto-confirm all prompts |
| Planning Mode | `--planning-mode` | Enable detailed task planning |
| Agent Mode | `--agent` | Enable autonomous agent execution |
| No Banner | `--no-banner` | Disable startup banner |
| Temperature | `-t`, `--temperature` | Response randomness (0.0-1.0) |
| Enable MCP | `--enable-mcp` | Enable Model Context Protocol support |
| MCP Config | `--mcp-config PATH` | Path to MCP server configuration file |
| Version | `-v`, `--version` | Show version |
| Help | `-h`, `--help` | Show help message |

### Interactive Commands

Once in the REPL, use these slash commands:

| Command | Description |
|---------|-------------|
| `/help` | Show available commands |
| `/model <name>` | Switch to different model |
| `/provider <name>` | Switch provider |
| `/models` | List available models |
| `/tool <name> <args>` | Execute a local tool |
| `/tools` | List available tools |
| `/mcp <subcommand>` | MCP server management (status, health, tools, reload) |
| `/agent <subcommand>` | Agent commands (plan, execute, report, history) |
| `/clear` | Clear conversation history |
| `/info` | Show current provider/model |
| `/env` | Check environment variables |

### Tab Autocomplete

The Ruby CLI supports intelligent tab completion for faster and more accurate command entry:

**Supported Completions:**
- **Slash Commands**: Type `/` and press TAB to see available commands
- **Models**: When using `/model` or `/provider`, TAB completes available model names
- **Providers**: TAB complete provider names with `/provider` command
- **Tools**: TAB complete tool names with `/tool` command
- **File Paths**: Smart file path completion for:
  - File-based tool arguments (`/tool cat [TAB]`)
  - General file paths in conversation
- **Multi-level Completion**: Context-aware completions like `/provider openai [TAB]` for models

**Usage Examples:**
```bash
# Complete commands
> /he[TAB]     → /help

# Complete models
> /model qw[TAB]     → /model qwq:32b

# Complete file paths
> /tool cat RE[TAB]  → /tool cat README.md

# Complete provider and model
> /provider open[TAB]       → /provider openai
> /provider openai gpt[TAB] → /provider openai gpt-4o
```

## 🔧 Configuration

### Environment Variables
```bash
# API Keys (alternative to AWS Secrets Manager)
export OPENAI_API_KEY="your-key-here"
export ANTHROPIC_API_KEY="your-key-here"
export GEMINI_API_KEY="your-key-here"
export MISTRAL_API_KEY="your-key-here"
export PERPLEXITY_API_KEY="your-key-here"

# AWS Configuration
export AWS_PROFILE="your-profile"
export AWS_REGION="us-east-1"
```

### AWS Secrets Manager
Store API keys securely in AWS Secrets Manager under `lantae/api-keys`:
```json
{
  "openai": "your-openai-key",
  "anthropic": "your-anthropic-key",
  "gemini": "your-gemini-key",
  "mistral": "your-mistral-key",
  "perplexity": "your-perplexity-key"
}
```

### Ollama Setup
```bash
# Install Ollama
curl -fsSL https://ollama.com/install.sh | sh

# Pull the default reasoning model
ollama pull cogito:latest

# Start Ollama service
ollama serve
```

### MCP (Model Context Protocol) Setup

Lantae includes Model Context Protocol support for extensible tool access.

#### Configuration
Create a `mcp_servers.yml` file to configure MCP servers:
```yaml
mcp_servers:
  # File system operations
  - name: filesystem
    transport: stdio
    command: npx
    args:
      - "@modelcontextprotocol/server-filesystem"
      - "/your/safe/directory"
    description: "File system operations via MCP"
    
  # Web search capabilities
  - name: web_search
    transport: http
    url: http://localhost:3001/mcp
    timeout: 30
    auth:
      type: bearer
      token: your_api_token_here
    description: "Web search capabilities via MCP"
```

#### Usage
Enable MCP support with the `--enable-mcp` flag:
```bash
# Start with MCP support
./lantae --enable-mcp

# Use custom config file
./lantae --enable-mcp --mcp-config /path/to/mcp_servers.yml
```

#### MCP Commands
Use `/mcp <subcommand>` to manage MCP servers:
- `/mcp status` - View server connection status
- `/mcp health` - Check server health
- `/mcp tools` - List available tools from all servers
- `/mcp reload` - Reload server configuration

#### Tool Usage
MCP tools are available using the format `server__tool`:
```bash
# List available tools (includes MCP tools)
/tools

# Use an MCP tool
/tool filesystem__read_file path="/path/to/file.txt"

# Or directly in conversation
> Use the filesystem server to read the README file
```

#### Security Features
- **Path traversal protection**: Blocks attempts to access parent directories
- **Command validation**: Basic detection of potentially harmful commands
- **Argument sanitization**: Validates tool arguments before execution
- **Connection management**: Secure server discovery and connection handling

## 🧠 Reasoning Models Comparison

Based on extensive testing, here are the top reasoning models by speed and quality:

| Model | Speed (avg) | Quality | Best For |
|-------|-------------|---------|----------|
| **cogito:latest** ⭐ | 0.35s | Excellent | Default choice - best balance |
| qwq:32b | 0.34s | Excellent | Fast despite large size |
| llama3.1-intuitive-thinker | 0.36s | Very Good | Chain-of-thought reasoning |
| qwen3:14b | 9.0s | Good | Detailed analysis |
| deepseek-r1:8b | 30.9s | Excellent | Maximum reasoning detail |

## 📝 Examples

### Basic Conversation
```bash
$ ./lantae
╔══════════════════════════════════════════════════════════════╗
║  🚀 Multi-Provider LLM Interface v1.0.0                     ║
║  ⚡ Powered by Cogito Reasoning Model                        ║
╚══════════════════════════════════════════════════════════════╝

> What is 2+2?
🤖 Thinking...
2 + 2 = 4

> /model qwen3:14b
Switched to model: qwen3:14b

> exit
```

### Tool Usage
```bash
> Create a hello world Python script
🤖 Thinking...
I'll create a simple "Hello World" Python script for you.

TOOL_CALL: write_file hello.py print("Hello, World!")

Tool Result:
File hello.py written successfully

The Python script has been created! You can run it with `python hello.py`.
```

### Auto-Accept Mode
```bash
$ ./lantae --auto-accept "Set up a new Git repository"
🤖 Thinking...
I'll help you set up a new Git repository. Would you like me to initialize it in the current directory?

[AUTO-ACCEPT] Automatically confirming action...
🤖 Executing...
TOOL_CALL: bash git init
TOOL_CALL: bash git add .
TOOL_CALL: bash git commit -m "Initial commit"

Repository initialized and first commit created!
```

## 🤖 Planning Agent

The Planning Agent is an advanced feature that breaks down complex tasks into manageable subtasks, executes them with verification, and learns from successes and failures.

### Features

- **Hierarchical Task Decomposition**: Automatically breaks complex tasks into simpler subtasks
- **Complexity Assessment**: Evaluates task difficulty on a 1-10 scale
- **Static Code Analysis**: Verifies generated code for common issues
- **Auto-Fix Capabilities**: Automatically fixes common errors (EOF markers, unclosed strings, etc.)
- **Success Tracking**: Records execution results to improve future performance
- **Rollback Support**: Can revert changes if execution fails

### Usage

#### Command Line
```bash
# Execute a task with the agent
./lantae --agent "Create a Python web scraper for news articles"

# With auto-accept to skip confirmations
./lantae --agent --auto-accept "Build a REST API with authentication"
```

#### REPL Commands
```bash
# Plan without executing
> /agent plan Create a todo list application

# Execute with planning
> /agent execute Build a simple calculator

# View execution report
> /agent report

# View task history
> /agent history
```

### How It Works

1. **Task Analysis**: The agent analyzes the complexity of your request
2. **Decomposition**: Complex tasks are broken into smaller, executable subtasks
3. **Planning**: A hierarchical execution plan is created
4. **Execution**: Each subtask is executed with the appropriate LLM
5. **Verification**: Results are verified using static analysis
6. **Auto-Fix**: Common issues are automatically corrected
7. **Learning**: Success rates are tracked for continuous improvement

### Example Output
```
🤖 Agent Mode: Planning and executing task...
📋 Creating execution plan...

📊 Execution Plan:
- [○] Create a Python web scraper (complexity: 7.2)
  - [○] Set up project structure (complexity: 2.1)
  - [○] Install required dependencies (complexity: 1.8)
  - [○] Create scraper class (complexity: 4.5)
  - [○] Add error handling (complexity: 3.2)
  - [○] Create main script (complexity: 2.8)

⚙️  Executing plan...
[✓] Set up project structure
[✓] Install required dependencies
[✓] Create scraper class (1 auto-fix applied)
[✓] Add error handling
[✓] Create main script

✅ Task completed successfully!
```

## 📁 Project Structure

See [PROJECT_STRUCTURE.md](PROJECT_STRUCTURE.md) for detailed directory layout and file organization.

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Built with [Claude Code](https://claude.ai/code)
- Powered by various LLM providers
- Inspired by the need for unified AI interfaces

## 🔗 Links

- [Ollama](https://ollama.com/) - Local LLM runtime
- [OpenAI API](https://platform.openai.com/) - GPT models
- [Anthropic API](https://www.anthropic.com/) - Claude models
- [AWS Bedrock](https://aws.amazon.com/bedrock/) - Managed AI services

---

**Made with ❤️ and AI assistance**