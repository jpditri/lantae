# Ollama Provider

## Overview
Ollama provides local AI model hosting, allowing you to run large language models on your own hardware without internet connectivity or API costs.

## Key Benefits
- **Privacy**: All processing happens locally
- **No API Costs**: Free to use once models are downloaded
- **Offline Capable**: Works without internet connection
- **Fast Response**: No network latency for inference

## Setup Instructions

### Installation
```bash
# macOS
brew install ollama

# Linux
curl -fsSL https://ollama.com/install.sh | sh

# Windows
# Download from https://ollama.com/download
```

### Starting Service
```bash
# Start Ollama service
ollama serve

# Pull recommended model
ollama pull cogito:latest
```

## Recommended Models

### Primary Reasoning Models
- **cogito:latest** ⭐ - Default choice, best balance of speed and quality (0.35s avg)
- **qwq:32b** - Fast reasoning despite large size (0.34s avg)
- **llama3.1-intuitive-thinker** - Excellent for chain-of-thought (0.36s avg)

### Lightweight Models
- **qwen2.5:1.5b** - Quick responses for simple tasks
- **qwen2.5:3b** - Good balance of size and capability

### Specialized Models
- **deepseek-r1:8b** - Maximum reasoning detail (slower but thorough)
- **qwen3:14b** - Detailed analysis capabilities

## Usage Examples

### Basic Usage
```bash
# Use default model
lantae

# Specify model
lantae -m qwq:32b

# Interactive mode with specific model
lantae -p ollama -m cogito:latest
```

### Model Management
```bash
# List available models
ollama list

# Pull new model
ollama pull llama3.1-intuitive-thinker

# Remove model
ollama rm old-model:tag
```

## Configuration

### Environment Variables
```bash
export OLLAMA_HOST=http://localhost:11434  # Default
export OLLAMA_MODELS=/path/to/models       # Custom model directory
```

### Lantae Configuration
Add to `.env` file:
```bash
DEFAULT_PROVIDER=ollama
DEFAULT_MODEL=cogito:latest
OLLAMA_HOST=http://localhost:11434
```

## Performance Tips

1. **Model Selection**: Use cogito:latest for best overall performance
2. **Hardware**: More RAM allows larger models (32GB+ recommended for 32B models)
3. **GPU**: Ollama supports GPU acceleration for faster inference
4. **Batch Processing**: Use batch mode for multiple related prompts

## Troubleshooting

### Common Issues
- **Service not running**: Check `ollama serve` is running
- **Model not found**: Use `ollama pull <model-name>` to download
- **Out of memory**: Try smaller models or increase system RAM
- **Slow responses**: Consider GPU acceleration or smaller models

### Verification
```bash
# Test Ollama service
curl http://localhost:11434/api/tags

# Test with Lantae
lantae -p ollama --version
```

## Model Comparison

| Model | Size | Speed | Quality | Memory | Use Case |
|-------|------|-------|---------|---------|----------|
| cogito:latest | ~7B | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | 8GB | Default choice |
| qwq:32b | ~32B | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | 32GB+ | Complex reasoning |
| qwen2.5:1.5b | ~1.5B | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | 2GB | Quick tasks |
| deepseek-r1:8b | ~8B | ⭐⭐ | ⭐⭐⭐⭐⭐ | 8GB | Detailed analysis |

---
*Last updated: $(date '+%Y-%m-%d')*