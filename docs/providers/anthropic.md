# Anthropic Provider

## Overview
Anthropic provides access to the Claude family of models, known for their helpful, harmless, and honest responses. Claude models excel at reasoning, analysis, and following instructions precisely.

## Key Benefits
- **Safety Focused**: Designed to be helpful and harmless
- **Long Context**: Exceptional long-form reasoning capabilities
- **Code Excellence**: Superior code generation and debugging
- **Reasoning**: Strong analytical and logical reasoning

## Setup Instructions

### API Key Setup
1. Visit [Anthropic Console](https://console.anthropic.com/)
2. Create an API key
3. Set environment variable:

```bash
export ANTHROPIC_API_KEY="your-api-key-here"
```

Or add to `.env` file:
```bash
ANTHROPIC_API_KEY=your-api-key-here
```

## Available Models

### Claude 3.5 Models (Latest)
- **claude-3-5-sonnet-20241022** ⭐ - Best balance of speed and capability
- **claude-3-5-haiku-20241022** - Fastest, most cost-effective

### Claude 3 Models
- **claude-3-opus-20240229** - Most capable for complex tasks
- **claude-3-sonnet-20240229** - Balanced performance
- **claude-3-haiku-20240307** - Fast and cost-effective

## Usage Examples

### Basic Usage
```bash
# Use default Claude model
lantae -p anthropic

# Specify model
lantae -p anthropic -m claude-3-5-sonnet-20241022

# Single prompt
lantae -p anthropic "Analyze this data pattern"
```

### Advanced Usage
```bash
# Long-form analysis
lantae -p anthropic -m claude-3-opus-20240229 "Write a detailed technical analysis"

# Code review
lantae -p anthropic --planning-mode "Review this codebase for security issues"

# Creative writing
lantae -p anthropic -t 0.8 "Write a science fiction story"
```

## Configuration

### Environment Variables
```bash
export ANTHROPIC_API_KEY="your-api-key"
export ANTHROPIC_BASE_URL="custom-endpoint"  # For proxies
```

### Lantae Configuration
Add to `.env` file:
```bash
DEFAULT_PROVIDER=anthropic
DEFAULT_MODEL=claude-3-5-sonnet-20241022
ANTHROPIC_API_KEY=your-api-key-here
```

## Model Comparison

| Model | Speed | Quality | Cost | Context | Best For |
|-------|-------|---------|------|---------|----------|
| claude-3-5-sonnet | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | $$$ | 200k | General purpose |
| claude-3-5-haiku | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | $$ | 200k | Quick tasks |
| claude-3-opus | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | $$$$ | 200k | Complex reasoning |
| claude-3-sonnet | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | $$$ | 200k | Balanced use |
| claude-3-haiku | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | $ | 200k | High volume |

## Features

### Strengths
- **Code Generation**: Excellent at writing clean, well-documented code
- **Analysis**: Deep analytical thinking and reasoning
- **Safety**: Built-in safety measures and ethical considerations
- **Long Context**: Handles very long documents and conversations
- **Tool Use**: Advanced function calling capabilities

### Specialized Capabilities
- Mathematical reasoning and problem solving
- Scientific analysis and research assistance
- Creative writing and storytelling
- Code review and debugging
- Document analysis and summarization

## Cost Optimization

### Model Selection Guidelines
- **claude-3-5-haiku**: Simple tasks, high-volume usage, quick responses
- **claude-3-5-sonnet**: Most tasks, best balance of quality and cost
- **claude-3-opus**: Complex analysis, critical reasoning, highest quality needs

### Best Practices
1. Use Haiku for simple, repetitive tasks
2. Use Sonnet for most general-purpose needs
3. Reserve Opus for complex reasoning tasks
4. Monitor usage through Anthropic Console

## Integration Features

### Tool Use
Claude models excel at using tools and function calling:
```bash
# Enable tool usage
lantae -p anthropic --enable-tools "Help me analyze this codebase"
```

### Streaming
Real-time response streaming for interactive experiences:
```bash
# Streaming is enabled by default in Lantae
lantae -p anthropic "Write a long explanation"
```

## Troubleshooting

### Common Issues
- **API key invalid**: Verify key in Anthropic Console
- **Rate limits**: Check usage limits and tier
- **Model access**: Ensure model is available in your region
- **Context length**: Break down very long inputs

### Verification
```bash
# Test API connectivity
curl -H "Authorization: Bearer $ANTHROPIC_API_KEY" \
     -H "anthropic-version: 2023-06-01" \
     https://api.anthropic.com/v1/messages

# Test with Lantae
lantae -p anthropic --version
```

## Safety and Ethics

Claude models are designed with strong safety measures:
- Constitutional AI training
- Refusal to generate harmful content
- Bias mitigation efforts
- Transparent limitations

## Best Practices

1. **Clear Instructions**: Provide specific, clear prompts
2. **Context Management**: Use appropriate context length
3. **Model Selection**: Choose right model for task complexity
4. **Safety**: Leverage built-in safety features
5. **Monitoring**: Track usage and performance

## Comparison with Other Providers

### vs OpenAI
- **Longer Context**: Claude handles longer documents better
- **Safety**: More conservative and safety-focused
- **Code Quality**: Often generates cleaner, more maintainable code
- **Reasoning**: Stronger step-by-step analytical thinking

### vs Local Models
- **Quality**: Higher quality for complex reasoning
- **Speed**: Faster for complex tasks (but requires internet)
- **Cost**: Pay-per-use vs local hardware costs
- **Privacy**: Data sent to Anthropic vs local processing

---
*Last updated: $(date '+%Y-%m-%d')*