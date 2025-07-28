# Intelligent Routing System

The Lantae CLI now includes an intelligent routing system that automatically classifies prompts and routes them through the most appropriate processing pipeline.

## Overview

The intelligent routing system analyzes each prompt to determine:
- Is this an instruction or a question?
- Does it require planning and decomposition?
- Can it be handled by a single agent?
- Does it need specialized analysis?

Based on this classification, prompts are routed through different pipelines with stage-specific models and providers.

## Enabling Intelligent Routing

```bash
# Enable intelligent routing
lantae --intelligent-routing

# With custom routing configuration
lantae --intelligent-routing --routing-config ~/.lantae/my-routing.yml
```

## Routing Stages

### 1. **Classifier Stage**
- Determines prompt type using a fast, lightweight model
- Default: `ollama/llama3:latest` (temperature: 0.1)

### 2. **Planning Stage**
- Creates comprehensive plans for complex tasks
- Default: `anthropic/claude-3-5-sonnet-20241022` (temperature: 0.3)

### 3. **Squad Stage**
- Manages teams of agents for multi-faceted tasks
- Default: `anthropic/claude-3-5-sonnet-20241022` (temperature: 0.2)

### 4. **Implementation Stage**
- Executes specific tasks and instructions
- Default: `anthropic/claude-3-5-sonnet-20241022` (temperature: 0.4)

### 5. **Quick Answer Stage**
- Handles simple questions with fast responses
- Default: `ollama/cogito:latest` (temperature: 0.7)

### 6. **Analysis Stage**
- Performs code review, debugging, and evaluation
- Default: `openai/gpt-4-turbo` (temperature: 0.2)

## Routing Commands

### Show Current Configuration
```bash
> /routing show
```

### Update Stage Configuration
```bash
# Change planning stage to use GPT-4
> /routing set planning provider=openai model=gpt-4-turbo temperature=0.2

# Change classifier to use a different model
> /routing set classifier model=mistral:latest

# Configure multiple settings at once
> /routing set implementation provider=anthropic model=claude-3-opus-20240229 temperature=0.5 max_tokens=8000
```

### Reset to Defaults
```bash
> /routing reset
```

### View Routing Statistics
```bash
> /routing stats
```

## Configuration File

The routing configuration is stored in `~/.lantae/routing_config.yml`:

```yaml
classifier:
  provider: ollama
  model: llama3:latest
  temperature: 0.1
  max_tokens: 100

planning:
  provider: anthropic
  model: claude-3-5-sonnet-20241022
  temperature: 0.3
  max_tokens: 4000

squad:
  provider: anthropic
  model: claude-3-5-sonnet-20241022
  temperature: 0.2
  max_tokens: 2000

implementation:
  provider: anthropic
  model: claude-3-5-sonnet-20241022
  temperature: 0.4
  max_tokens: 8000

quick_answer:
  provider: ollama
  model: cogito:latest
  temperature: 0.7
  max_tokens: 2000

analysis:
  provider: openai
  model: gpt-4-turbo
  temperature: 0.2
  max_tokens: 4000
```

## Examples

### Planning Request
```bash
> Build a web scraping application that monitors prices on e-commerce sites

🎯 Routing through full planning pipeline...
[Uses planning model to create detailed architecture and implementation plan]
```

### Simple Question
```bash
> What is the difference between let and const in JavaScript?

💬 Quick answer routing...
[Uses fast model for immediate response]
```

### Analysis Request
```bash
> Analyze this code for potential security vulnerabilities

🔍 Analysis routing...
[Uses specialized analysis model for thorough review]
```

### Direct Implementation
```bash
> Create a Python function to calculate fibonacci numbers

🔧 Direct implementation routing...
[Uses implementation model to write code directly]
```

## Best Practices

1. **Model Selection**:
   - Use fast models (llama3, mistral) for classification and quick answers
   - Use powerful models (Claude, GPT-4) for planning and complex implementation
   - Match temperature to task type (lower for analysis, higher for creative tasks)

2. **Provider Selection**:
   - Ollama: Best for local, fast responses
   - Anthropic: Excellent for complex reasoning and code generation
   - OpenAI: Strong for analysis and general tasks
   - Gemini: Good for large context windows

3. **Cost Optimization**:
   - Route simple questions to cheaper/local models
   - Reserve expensive models for complex tasks
   - Monitor usage with `/cost status`

4. **Performance Tuning**:
   - Adjust max_tokens based on expected response size
   - Use lower temperatures for deterministic tasks
   - Enable parallel processing for independent subtasks

## Troubleshooting

### Routing Not Working
```bash
# Check if routing is enabled
> /info

# Verify configuration
> /routing show

# Reset to defaults if needed
> /routing reset
```

### Wrong Model Being Used
```bash
# Check routing statistics to see what's being used
> /routing stats

# Update specific stage configuration
> /routing set <stage> provider=<provider> model=<model>
```

### Performance Issues
- Reduce max_tokens for faster responses
- Use local models (Ollama) for classification
- Enable caching for repeated queries

## Integration with Other Features

- **Planning Mode**: When `--planning-mode` is enabled, all prompts go through planning pipeline
- **Agent Mode**: Routes through squad deployment automatically
- **Auto-Accept**: Works with all routing modes
- **Cost Tracking**: Tracks costs across all providers used in routing

## Future Enhancements

- Learning from user feedback to improve classification
- Dynamic model selection based on load and availability
- Custom routing rules and patterns
- Integration with full squad management system
- Caching frequently used plans and analyses