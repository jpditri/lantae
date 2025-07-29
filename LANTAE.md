# LANTAE.md - Multi-Provider LLM CLI Interface

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 🚀 Discovery & Direct Tool Access

**New to the system?** Start here for command discovery and system navigation:

```bash
./bin/lantae help                    # Basic command help
./bin/lantae models                  # List available models by provider
./bin/lantae -p <provider> --help    # Provider-specific help
```

## 🔮 Overview

Lantae is a powerful CLI/REPL interface that provides unified access to multiple Large Language Model providers. Whether you prefer local privacy with Ollama or cloud-powered capabilities with OpenAI, Anthropic, and Gemini, Lantae offers a consistent interface for all your AI needs.

## Core Architecture

**Provider System:**
- Multi-provider support with automatic failover
- Context-aware provider selection
- Host-aware Ollama detection (local vs network)
- Unified API across all providers

**CLI Design:**
- REPL mode for interactive sessions
- One-shot command execution
- Provider switching without restart
- Tool integration and MCP protocol support

## 🔧 Development Standards

**CRITICAL DEVELOPMENT CONSTRAINTS:**
- **NEVER create files** unless absolutely necessary
- **ALWAYS prefer editing** existing files
- **ALWAYS create README.md files** for new directories to document their purpose and contents
- Do what has been asked; nothing more, nothing less

### Core Implementation Patterns

**Provider Integration:**
- Use unified provider registry pattern
- Implement proper error handling and fallbacks
- Support both local and cloud providers
- Maintain context consistency across providers

**CLI Tool Development:**
- Follow command/option pattern established in existing tools
- Include proper help text and examples
- Implement dry-run capabilities where applicable
- Add logging and metrics tracking

**Testing Approach:**
- Unit tests for core functionality
- Integration tests for provider interactions
- Manual testing scripts for full workflows

## 🚀 Quick Start

### Installation
```bash
git clone https://github.com/jpditri/lantae.git
cd lantae
chmod +x install.sh
./install.sh
```

### Basic Usage
```bash
# Local AI (privacy-focused)
lantae                              # Uses Ollama with cogito model

# Cloud AI (high-performance)  
lantae -p openai -m gpt-4o         # OpenAI GPT-4o
lantae -p anthropic                # Anthropic Claude
lantae -p gemini -m gemini-1.5-pro # Google Gemini

# Interactive mode
lantae                              # Enter REPL mode
```

## 📁 Directory Organization

### Core Directory Structure

**System Infrastructure:**
- `bin/` - Main CLI executables and command tools
- `lib/ruby/` - Ruby libraries and provider implementations
- `lib/ruby/providers/` - Provider-specific implementations
- `lib/ruby/cli/` - CLI command framework and handlers
- `docs/` - Provider documentation and guides
- `spec/` - Test specifications and integration tests

**Configuration:**
- `mcp_servers.yml` - MCP server configurations
- `.env` - Environment variables and API keys
- `LANTAE.md` - This system context file (analog to CLAUDE.md)

### Provider System Architecture

**Provider Registry:**
- `lib/ruby/providers/provider_registry.rb` - Central provider management
- `lib/ruby/providers/base_provider.rb` - Common provider interface
- Individual provider implementations (ollama, openai, anthropic, etc.)

**CLI Command System:**
- `lib/ruby/cli/command_registry.rb` - Dynamic command registration
- `lib/ruby/cli/commands/` - Individual command implementations
- Modular command structure with help and validation

## 🤖 AI Provider Integration

The system supports multiple AI providers with unified interface:

**Supported Providers:**
- **Ollama**: Local models (privacy-focused, free)
- **OpenAI**: GPT models (cloud, high-performance)
- **Anthropic**: Claude models (reasoning excellence)
- **Google**: Gemini models (speed & context)
- **Mistral**: Mistral models (European alternative)
- **Perplexity**: Online-enhanced models

**Provider Selection:**
```bash
lantae -p ollama -m cogito:latest          # Local Ollama
lantae -p openai -m gpt-4o                 # OpenAI
lantae -p anthropic -m claude-3-5-sonnet   # Anthropic
lantae -p gemini -m gemini-1.5-pro         # Google
```

**Network Detection:**
- Automatic local vs network Ollama detection
- Host information displayed in models command
- Environment variable configuration (OLLAMA_HOST)

## 🔄 Command Framework

### CLI Command Registry

All commands are registered dynamically through the command registry:

```ruby
# Adding new commands
command_registry = Lantae::CLI::CommandRegistry.new
command_registry.register_command(Commands::NewCommand.new)
```

**Command Structure:**
- Base command class with common functionality
- Help system integration
- Argument parsing and validation
- Error handling and logging

### Slash Commands

Interactive REPL supports slash commands:
```bash
/help                    # Show available commands
/models                  # List models for current provider
/provider <name>         # Switch provider
/clear                   # Clear conversation
/info                    # Show current provider/model
```

## 🧪 Testing & Quality Assurance

### Test Structure
- `spec/` - RSpec test files
- Integration tests for provider interactions
- Unit tests for core functionality
- Mock providers for testing isolation

### Quality Standards
- All provider implementations must extend BaseProvider
- Commands must include help text and examples
- Error messages should be user-friendly
- Logging for debugging and monitoring

## 🔐 Security & Configuration

### API Key Management
- Environment variables for API keys
- AWS Secrets Manager integration
- Local .env file support
- Runtime key validation

### Network Security
- HTTPS for all cloud provider communications
- Local-first design with Ollama
- No data persistence by default
- Configurable logging levels

## 📊 System Monitoring

### Metrics & Logging
- Context usage tracking
- Provider performance monitoring
- Error rate tracking
- Usage analytics (opt-in)

### Health Checks
- Provider availability checking
- Model loading verification
- Network connectivity tests
- Configuration validation

---

<!-- PROVIDER_CONTENT_START -->
<!-- This section is automatically populated from docs/providers/ -->

## 🏠 Local Provider: Ollama

*Local AI processing for privacy and cost savings*

### Quick Setup
```bash
# Install Ollama
curl -fsSL https://ollama.com/install.sh | sh

# Start service and pull default model
ollama serve
ollama pull cogito:latest

# Use with Lantae
lantae  # Uses Ollama by default
```

### Key Benefits
- **Complete Privacy**: All processing happens locally
- **No API Costs**: Free after initial setup
- **Offline Capable**: Works without internet
- **Fast Local Inference**: No network latency

### Recommended Models
- **cogito:latest** ⭐ - Best balance (0.35s avg response)
- **qwq:32b** - Fast reasoning despite size (0.34s avg)
- **qwen2.5:1.5b** - Lightweight for quick tasks

[📖 Full Ollama Documentation](docs/providers/ollama.md)

---

## ☁️ Cloud Providers

### OpenAI - Industry Leader
*GPT models with cutting-edge capabilities*

**Setup**: `export OPENAI_API_KEY="your-key"`

**Best Models**:
- **gpt-4o** - Latest, most capable
- **gpt-4o-mini** - Fast and cost-effective

**Use Cases**: General purpose, creative writing, complex reasoning

```bash
lantae -p openai -m gpt-4o "Explain quantum computing"
```

[📖 Full OpenAI Documentation](docs/providers/openai.md)

---

### Anthropic - Reasoning Excellence  
*Claude models focused on helpful, harmless, and honest AI*

**Setup**: `export ANTHROPIC_API_KEY="your-key"`

**Best Models**:
- **claude-3-5-sonnet-20241022** ⭐ - Best balance
- **claude-3-5-haiku-20241022** - Fast and economical

**Use Cases**: Code analysis, detailed reasoning, safety-critical tasks

```bash
lantae -p anthropic "Analyze this codebase for security issues"
```

[📖 Full Anthropic Documentation](docs/providers/anthropic.md)

---

### Google Gemini - Speed & Context
*Multimodal models with exceptional context length*

**Setup**: `export GEMINI_API_KEY="your-key"`

**Best Models**:
- **gemini-1.5-pro** - Most capable
- **gemini-1.5-flash** - Optimized for speed

**Use Cases**: Fast responses, long documents, multimodal tasks

```bash
lantae -p gemini -m gemini-1.5-flash "Summarize this research paper"
```

[📖 Full Gemini Documentation](docs/providers/gemini.md)

<!-- PROVIDER_CONTENT_END -->

---

## 🔄 Provider Comparison

| Provider | Speed | Quality | Cost | Privacy | Best For |
|----------|-------|---------|------|---------|----------|
| **Ollama** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | Free | ⭐⭐⭐⭐⭐ | Privacy, Learning |
| **OpenAI** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | $$$ | ⭐⭐ | General Purpose |
| **Anthropic** | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | $$$ | ⭐⭐ | Code, Reasoning |
| **Gemini** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | $$ | ⭐⭐ | Speed, Context |

[📊 Detailed Provider Comparison](docs/providers/provider-comparison.md)

---

## 🛠️ Advanced Features

### Tool Integration
Execute local tools directly from AI conversations:
```bash
lantae --enable-tools "Help me analyze my git repository"
```

### MCP Protocol Support
Extend capabilities with Model Context Protocol servers:
```bash
lantae --enable-mcp "Use the filesystem server to read my project"
```

### LSP Integration
Language Server Protocol support for code intelligence:
```bash
lantae --enable-lsp "Help me refactor this codebase"
```

### Agent Mode
Autonomous task planning and execution:
```bash
lantae --agent "Build a complete web application with tests"
```

---

## 🎯 Use Case Examples

### Code Development
```bash
# Local code review (private)
lantae "Review this function for bugs"

# Cloud-powered refactoring
lantae -p anthropic "Refactor this code for better performance"
```

### Content Creation
```bash
# Creative writing
lantae -p openai "Write a science fiction story"

# Technical documentation
lantae -p anthropic "Create API documentation for this code"
```

### Data Analysis
```bash
# Local data processing (sensitive data)
lantae "Analyze this financial dataset"

# Complex analysis
lantae -p gemini "Process this 100-page research paper"
```

### Learning & Research
```bash
# Private tutoring
lantae "Explain machine learning concepts"

# Research assistance
lantae -p anthropic "Help me understand this academic paper"
```

---

## ⚙️ Configuration

### Environment Variables
```bash
# API Keys
export OPENAI_API_KEY="your-openai-key"
export ANTHROPIC_API_KEY="your-anthropic-key"
export GEMINI_API_KEY="your-gemini-key"

# Default Settings
export DEFAULT_PROVIDER="ollama"
export DEFAULT_MODEL="cogito:latest"
```

### Configuration File
Edit `.env` in your project directory:
```env
# Provider Settings
DEFAULT_PROVIDER=ollama
DEFAULT_MODEL=cogito:latest

# API Keys (optional if using environment variables)
OPENAI_API_KEY=your-key-here
ANTHROPIC_API_KEY=your-key-here
GEMINI_API_KEY=your-key-here

# Ollama Settings
OLLAMA_HOST=http://localhost:11434
```

---

## 📚 Documentation

### Provider-Specific Guides
- [📖 Ollama Setup & Models](docs/providers/ollama.md)
- [📖 OpenAI Configuration](docs/providers/openai.md)  
- [📖 Anthropic Integration](docs/providers/anthropic.md)
- [📖 Gemini Configuration](docs/providers/gemini.md)
- [📊 Provider Comparison](docs/providers/provider-comparison.md)

### Feature Documentation
- [🏗️ Architecture Overview](ARCHITECTURE.md)
- [🛠️ Development Guide](docs/DEVELOPMENT_GUIDE.md)
- [🧪 LSP Integration](docs/LSP-IMPLEMENTATION.md)
- [📋 Project Structure](PROJECT_STRUCTURE.md)

---

## 🤝 Community & Support

### Getting Help
- **Documentation**: Check provider-specific docs above
- **Issues**: Report bugs on GitHub
- **Discussions**: Join community discussions

### Contributing
1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

### License
MIT License - see [LICENSE](LICENSE) file for details

---

## 🔮 Future Roadmap

- Additional provider integrations
- Enhanced multimodal capabilities  
- Improved agent reasoning
- Extended tool ecosystem
- Mobile companion app

---

*Documentation automatically updated from: `docs/providers/*.md`*
*Last generated: $(date '+%Y-%m-%d %H:%M:%S')*

---

## 🧠 Assistant Context & Guidelines

**For Claude Code (claude.ai/code) when working with this repository:**

### Primary Interface Guidelines
- **Prefer editing** existing files over creating new ones
- **Use provider registry** for all AI provider interactions
- **Follow command registry pattern** for new CLI commands
- **Include proper error handling** and user feedback
- **Maintain backwards compatibility** where possible

### Development Workflow
1. **Understand the provider system** - All AI interactions go through the provider registry
2. **Use existing patterns** - Look at current command implementations as examples
3. **Test with multiple providers** - Ensure changes work across Ollama, OpenAI, Anthropic, etc.
4. **Update documentation** - Keep provider docs current with changes
5. **Validate with real usage** - Test both REPL and one-shot command modes

### Key Architecture Principles
- **Provider abstraction** - Unified interface for all AI providers
- **Network awareness** - Handle local vs remote Ollama instances
- **Graceful degradation** - Fallback options when providers are unavailable
- **User experience** - Clear error messages and helpful guidance

### Important Context Files
- **LANTAE.md** - This system context file (included in all AI requests)
- **Provider docs** - `docs/providers/*.md` for specific provider guidance
- **Command registry** - `lib/ruby/cli/command_registry.rb` for command management
- **Base classes** - Provider and command base classes define interfaces

**Remember**: This LANTAE.md file is automatically included in all AI provider requests to give context about the system architecture and development standards.

# important-instruction-reminders
Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.