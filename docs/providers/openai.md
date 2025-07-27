# OpenAI Provider

## Overview
OpenAI provides access to the GPT family of models, including GPT-4o, GPT-4o-mini, and other cutting-edge language models through their API.

## Key Benefits
- **Latest Models**: Access to newest GPT models
- **High Quality**: Industry-leading response quality
- **Reliable Infrastructure**: Robust API with high uptime
- **Extensive Capabilities**: Text, code, reasoning, and creative tasks

## Setup Instructions

### API Key Setup
1. Visit [OpenAI Platform](https://platform.openai.com/api-keys)
2. Create an API key
3. Set environment variable:

```bash
export OPENAI_API_KEY="your-api-key-here"
```

Or add to `.env` file:
```bash
OPENAI_API_KEY=your-api-key-here
```

## Available Models

### GPT-4o Models (Recommended)
- **gpt-4o** - Latest and most capable model
- **gpt-4o-mini** - Faster and more cost-effective version

### GPT-4 Models
- **gpt-4-turbo** - High performance with large context
- **gpt-4** - Original GPT-4 model

### GPT-3.5 Models
- **gpt-3.5-turbo** - Fast and cost-effective

## Usage Examples

### Basic Usage
```bash
# Use default OpenAI model
lantae -p openai

# Specify model
lantae -p openai -m gpt-4o

# Single prompt
lantae -p openai "Explain quantum computing"
```

### Advanced Usage
```bash
# With temperature control
lantae -p openai -m gpt-4o -t 0.7 "Write a creative story"

# Planning mode
lantae -p openai --planning-mode "Build a web application"

# Auto-accept mode
lantae -p openai --auto-accept "Fix this code bug"
```

## Configuration

### Environment Variables
```bash
export OPENAI_API_KEY="your-api-key"
export OPENAI_ORG_ID="your-org-id"        # Optional
export OPENAI_BASE_URL="custom-endpoint"  # For proxies
```

### Lantae Configuration
Add to `.env` file:
```bash
DEFAULT_PROVIDER=openai
DEFAULT_MODEL=gpt-4o
OPENAI_API_KEY=your-api-key-here
```

## Cost Optimization

### Model Selection by Use Case
- **gpt-4o-mini**: General tasks, code completion, quick questions
- **gpt-4o**: Complex reasoning, analysis, creative writing
- **gpt-3.5-turbo**: Simple tasks, high-volume usage

### Cost-Effective Practices
1. Use appropriate model for task complexity
2. Set reasonable temperature values (0.1-0.9)
3. Use system messages to guide behavior
4. Monitor usage via OpenAI dashboard

## Model Comparison

| Model | Speed | Quality | Cost | Context | Best For |
|-------|-------|---------|------|---------|----------|
| gpt-4o | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | $$$$ | 128k | Complex reasoning |
| gpt-4o-mini | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | $$ | 128k | General purpose |
| gpt-4-turbo | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | $$$$ | 128k | Analysis tasks |
| gpt-3.5-turbo | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | $ | 16k | Simple tasks |

## Features

### Supported Capabilities
- Text generation and completion
- Code generation and debugging
- Mathematical reasoning
- Creative writing
- Language translation
- Summarization and analysis

### Integration Features
- Function calling (tools)
- JSON mode for structured output
- Vision capabilities (with gpt-4o)
- Streaming responses

## Troubleshooting

### Common Issues
- **Authentication failed**: Check API key validity
- **Rate limits**: Implement backoff or upgrade plan
- **Token limits**: Use appropriate model for context needs
- **Invalid model**: Verify model name spelling

### Error Handling
```bash
# Test API connectivity
curl -H "Authorization: Bearer $OPENAI_API_KEY" \
     https://api.openai.com/v1/models

# Verify with Lantae
lantae -p openai --version
```

## Best Practices

1. **Security**: Never commit API keys to version control
2. **Rate Limiting**: Respect API rate limits
3. **Model Selection**: Choose appropriate model for task
4. **Error Handling**: Implement retry logic for production use
5. **Monitoring**: Track usage and costs regularly

---
*Last updated: $(date '+%Y-%m-%d')*