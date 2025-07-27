# Lantae - Multi-Provider LLM Interface

*This document is dynamically generated from provider-specific documentation files.*

---

## 🔮 Overview

Lantae is a powerful CLI/REPL interface that provides unified access to multiple Large Language Model providers. Whether you prefer local privacy with Ollama or cloud-powered capabilities with OpenAI, Anthropic, and Gemini, Lantae offers a consistent interface for all your AI needs.

## 🚀 Quick Start

### Installation
```bash
git clone https://github.com/jpditri/lantae-cli.git
cd lantae-cli
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