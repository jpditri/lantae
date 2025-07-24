# 🔮 Lantae - Multi-Provider AI CLI

A powerful CLI/REPL for interacting with multiple AI providers including Ollama, OpenAI, Anthropic, AWS Bedrock, Google Gemini, Mistral, and Perplexity.

## Features

- **7 AI Providers**: Ollama (default), OpenAI, Anthropic, Bedrock, Gemini, Mistral, Perplexity
- **Runtime Model Switching**: Switch providers and models with `/provider` and `/model` commands
- **AWS Secrets Manager Integration**: Automatically fetch API keys from AWS
- **Local Tool Execution**: Run bash, python, node, git commands directly
- **Environment Caching**: API keys cached in ENV to avoid repeated AWS calls
- **Both Node.js and Ruby**: Choose your preferred implementation

## Quick Start

### Option 1: Node.js Version

```bash
# Install dependencies
npm install

# Set up API keys from AWS Secrets Manager
npm run setup

# Start using Lantae
lantae                           # Default Ollama
lantae -p openai "Hello world"   # OpenAI
lantae -p gemini "Hello world"   # Google Gemini
```

### Option 2: Ruby Version

```bash
# Install dependencies
bundle install

# Set up API keys from AWS Secrets Manager  
./setup-secrets.rb

# Start using Lantae
./lantae.rb                           # Default Ollama
./lantae.rb -p anthropic "Hello"      # Anthropic Claude
./lantae.rb -p mistral "Hello"        # Mistral
```

## AWS Secrets Manager Setup

### 1. Create the Secret

```bash
aws secretsmanager create-secret \
  --name "lantae/api-keys" \
  --description "Lantae API Keys" \
  --secret-string '{
    "openai": "sk-your-openai-key",
    "anthropic": "sk-ant-your-anthropic-key",
    "gemini": "your-gemini-api-key", 
    "mistral": "your-mistral-api-key",
    "perplexity": "pplx-your-perplexity-key"
  }'
```

### 2. Configure AWS Credentials

```bash
aws configure
# OR set environment variables:
export AWS_ACCESS_KEY_ID=your-key
export AWS_SECRET_ACCESS_KEY=your-secret
export AWS_REGION=us-east-1
```

### 3. Run Setup Script

```bash
# Node.js
npm run setup

# Ruby  
./setup-secrets.rb
```

## Usage

### Command Line Options

```bash
lantae [options] [prompt]

Options:
  -m, --model <model>      Model to use (default: qwen2.5-coder:7b)
  -p, --provider <name>    Provider (ollama, openai, anthropic, bedrock, gemini, mistral, perplexity)
  -u, --url <url>          Ollama server URL (default: http://localhost:11434)
  -r, --region <region>    AWS region (default: us-east-1)
  -s, --secret <secret>    AWS secret name (default: lantae/api-keys)
  -t, --temperature <temp> Temperature (default: 0.1)
```

### Interactive REPL

```bash
🔮 Lantae v1.0.0
Provider: ollama | Model: qwen2.5-coder:7b
Type "/help" for commands, "exit" or "quit" to end

> /provider openai gpt-4o
Switched to provider: openai, model: gpt-4o

> /model gpt-4o-mini  
Switched to model: gpt-4o-mini

> /tool bash ls -la
total 48
drwxr-xr-x  12 user  staff   384 Jan 24 10:30 .
...

> Hello, can you help me write Python code?
```

### Slash Commands

- `/help` - Show all commands
- `/provider <name> [model]` - Switch provider (e.g., `/provider gemini`)
- `/model <name>` - Switch model (e.g., `/model gpt-4o`)
- `/models` - List available models for current provider
- `/tool <name> <args>` - Execute local tool (bash, python, git, etc.)
- `/tools` - List available tools
- `/clear` - Clear conversation history
- `/info` - Show current provider and model
- `/env` - Check API key status

## Supported Providers & Models

### Ollama (Local)
- Any locally installed model
- Default: `qwen2.5-coder:7b`

### OpenAI
- `gpt-4o`, `gpt-4o-mini`, `gpt-4-turbo`, `gpt-4`, `gpt-3.5-turbo`
- `o1-preview`, `o1-mini`

### Anthropic
- `claude-3-5-sonnet-20241022`, `claude-3-5-haiku-20241022`
- `claude-3-opus-20240229`, `claude-3-sonnet-20240229`, `claude-3-haiku-20240307`

### Google Gemini
- `gemini-1.5-pro`, `gemini-1.5-flash`, `gemini-1.0-pro`

### Mistral
- `mistral-large-latest`, `mistral-medium-latest`, `mistral-small-latest`
- `open-mistral-7b`, `open-mixtral-8x7b`, `open-mixtral-8x22b`

### Perplexity
- `llama-3.1-sonar-large-128k-online`, `llama-3.1-sonar-small-128k-online`
- `llama-3.1-sonar-large-128k-chat`, `llama-3.1-sonar-small-128k-chat`

### AWS Bedrock
- **Claude**: `claude-3-5-sonnet`, `claude-3-5-haiku`, `claude-3-sonnet`, `claude-3-haiku`, `claude-3-opus`
- **Titan**: `titan-text-g1-large`, `titan-text-g1-express`
- **Llama**: `llama2-13b`, `llama2-70b`, `llama3-8b`, `llama3-70b`
- **Cohere**: `command-text`, `command-light`

## Local Tools

Execute local commands and scripts:

```bash
> /tool bash git status
> /tool python print("Hello from Python")
> /tool node console.log("Hello from Node")
> /tool ls .
> /tool cat package.json
> /tool git log --oneline -5
```

Available tools: `bash`, `python`, `node`, `ls`, `cat`, `pwd`, `git`, `npm` (Node.js) or `ruby`, `bundle` (Ruby)

## Environment Variables

Alternative to AWS Secrets Manager:

```bash
export OPENAI_API_KEY=sk-...
export ANTHROPIC_API_KEY=sk-ant-...
export GEMINI_API_KEY=...
export MISTRAL_API_KEY=...
export PERPLEXITY_API_KEY=pplx-...
```

## Files

- `index.js` - Node.js CLI implementation
- `lantae.rb` - Ruby CLI implementation  
- `setup-secrets.js` - Node.js setup script
- `setup-secrets.rb` - Ruby setup script
- `secrets-template.json` - Template for API keys
- `lantae-secrets.json` - Local secrets file (auto-generated, git-ignored)

## Security

- All secret files are in `.gitignore`
- API keys cached in environment variables only
- AWS IAM permissions control secret access
- No API keys stored in code or version control