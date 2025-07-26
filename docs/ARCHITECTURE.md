# Lantae Architecture Documentation

## Overview

Lantae is a multi-language AI assistant framework that provides a unified interface for interacting with various language models. The system now supports both Ruby and LISP implementations, with shared architectural patterns and extensibility mechanisms.

## Architecture Principles

### 1. Multi-Language Support
- **Ruby Implementation**: Original implementation with mature feature set
- **LISP Implementation**: Functional programming approach with S-expression configuration
- **Shared Concepts**: Both implementations follow similar architectural patterns

### 2. Provider Abstraction
- Unified interface for different AI providers (Ollama, OpenAI, Anthropic, etc.)
- Provider-specific implementations hidden behind common protocol
- Easy addition of new providers through plugin architecture

### 3. Extensibility
- Plugin system for adding new features
- Hook mechanisms for customizing behavior
- Language Server Protocol (LSP) support for IDE integration

### 4. Configuration Management
- Hierarchical configuration system
- Environment variable overrides
- Hot-reload capabilities

## Core Components

### 1. CLI Interface
```
├── bin/
│   ├── lantae          # Ruby executable
│   └── lantae-lisp     # LISP executable
```

Both CLIs provide:
- Interactive REPL mode
- Single-shot command execution
- Configuration management
- Provider switching

### 2. Provider System

#### Ruby Implementation
```ruby
class Provider::Base
  def chat(messages, model: nil, temperature: nil)
  def stream(messages, model: nil, temperature: nil, &block)
  def models
end
```

#### LISP Implementation
```lisp
(defstruct provider
  name
  chat-fn
  stream-fn
  models-fn
  config)
```

### 3. Configuration System

#### Ruby Structure
```yaml
model: "cogito:latest"
provider: "ollama"
temperature: 0.1
auto_accept: false
planning_mode: false
agent_mode: false
```

#### LISP Structure
```lisp
(:model "cogito:latest"
 :provider "ollama"
 :temperature 0.1
 :auto-accept nil
 :planning-mode nil
 :agent-mode nil)
```

### 4. Plugin Architecture

#### Ruby Plugins
```ruby
module Lantae
  module Plugins
    class Base
      def initialize(context)
      def on_before_request(messages, options)
      def on_after_response(response)
    end
  end
end
```

#### LISP Extensions
- Command macros for defining new commands
- Provider middleware for adding functionality
- Hook system for lifecycle events

### 5. LSP Implementation

The Language Server Protocol implementation provides:
- Code completion for prompts
- Inline documentation
- Error diagnostics
- Quick fixes and code actions

## Data Flow

```
User Input
    ↓
CLI Interface (Ruby/LISP)
    ↓
Command Parser
    ↓
Configuration Manager
    ↓
Provider Selection
    ↓
Request Building
    ↓
Provider API Call
    ↓
Response Processing
    ↓
Output Formatting
    ↓
User Display
```

## Provider Integration

### Adding a New Provider

#### Ruby
1. Create provider class inheriting from `Provider::Base`
2. Implement required methods: `chat`, `stream`, `models`
3. Register in provider factory
4. Add configuration support

#### LISP
1. Create provider constructor function
2. Implement chat/stream/models functions
3. Register with `register-provider`
4. Add initialization logic

### Provider Features Matrix

| Provider   | Chat | Stream | Models | Embeddings | Vision |
|------------|------|--------|---------|------------|---------|
| Ollama     | ✓    | ✓      | ✓       | ✓          | ✓       |
| OpenAI     | ✓    | ✓      | ✓       | ✓          | ✓       |
| Anthropic  | ✓    | ✓      | ✓       | ✗          | ✓       |
| Bedrock    | ✓    | ✓      | ✓       | ✓          | ✓       |
| Gemini     | ✓    | ✓      | ✓       | ✓          | ✓       |

## Security Architecture

### API Key Management
- Environment variable storage
- Secure credential passing
- No hardcoded secrets

### Request Validation
- Input sanitization
- Rate limiting support
- Audit logging capabilities

### Network Security
- HTTPS enforcement for cloud providers
- Proxy support
- Certificate validation

## Performance Considerations

### Caching Strategy
- Response caching for identical requests
- Model list caching
- Configuration caching

### Connection Pooling
- HTTP connection reuse
- Provider-specific optimizations
- Concurrent request handling

### Resource Management
- Memory-efficient streaming
- Garbage collection optimization
- Process lifecycle management

## Deployment Architecture

### Local Development
```
Developer Machine
    ├── Ruby Version (via rbenv/rvm)
    ├── LISP Version (via SBCL)
    └── Configuration Files
```

### Production Deployment
```
Production Server
    ├── Container/VM
    │   ├── Lantae Binary
    │   ├── Configuration
    │   └── Logs
    └── Monitoring
        ├── Metrics
        └── Alerts
```

## Future Architecture Considerations

### Planned Enhancements
1. **Mission Abort Strategy**: Graceful degradation for local model failures
2. **Enhanced Error Recovery**: Circuit breakers and fallback mechanisms
3. **Performance Optimizations**: Advanced caching and batching
4. **Distributed Architecture**: Multi-node deployment support

### Extension Points
1. **Custom Providers**: Plugin API for third-party providers
2. **Middleware System**: Request/response interceptors
3. **Event System**: Pub/sub for system events
4. **Metrics Collection**: Pluggable metrics backends

## Architecture Decisions

### Why Multi-Language?
- **Ruby**: Rapid development, rich ecosystem, excellent string handling
- **LISP**: Functional paradigm, S-expressions, macro system, REPL-first

### Why Provider Abstraction?
- Vendor independence
- Easy provider switching
- Consistent interface
- Feature parity across providers

### Why LSP?
- IDE integration
- Language-agnostic protocol
- Rich editing features
- Industry standard

## Conclusion

The Lantae architecture is designed for flexibility, extensibility, and maintainability. The multi-language approach allows developers to choose the implementation that best fits their needs while maintaining consistent functionality across both versions.