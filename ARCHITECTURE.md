# Lantae Architecture Overview

## 🏗️ Architecture Evolution

### Original Monolithic Design
- **Single file**: 1,499 lines in `bin/lantae`
- **Mixed concerns**: Providers, CLI, config, tools all in one file
- **Hard to maintain**: Difficult to test and extend
- **Limited modularity**: No separation of responsibilities

### New Modular Architecture
- **Clean separation**: Providers, CLI, config, tools in separate modules
- **Plugin architecture**: Dynamic provider loading and registration
- **Testable design**: Dependency injection and clear interfaces
- **Extensible**: Easy to add new providers, commands, and features

## 📁 Directory Structure

```
lantae/
├── bin/
│   ├── lantae              # Original monolithic CLI
│   ├── lantae-modular      # New modular Ruby CLI
│   └── lantae-lsp          # LSP server executable
├── lib/ruby/
│   ├── providers/          # Provider abstraction layer
│   │   ├── base_provider.rb
│   │   ├── provider_registry.rb
│   │   ├── ollama_provider.rb
│   │   ├── openai_provider.rb
│   │   ├── anthropic_provider.rb
│   │   ├── gemini_provider.rb
│   │   ├── mistral_provider.rb
│   │   ├── perplexity_provider.rb
│   │   └── bedrock_provider.rb
│   ├── cli/                # Command system
│   │   ├── base_command.rb
│   │   ├── command_registry.rb
│   │   └── commands/
│   │       ├── help_command.rb
│   │       ├── provider_command.rb
│   │       └── model_command.rb
│   ├── config/             # Configuration management
│   │   └── configuration.rb
│   ├── lsp/                # Language Server Protocol
│   │   ├── server.rb
│   │   ├── client.rb
│   │   ├── code_actions.rb
│   │   └── server_runner.rb
│   └── [existing files...]
├── lisp/                   # LISP implementation
│   ├── lantae.lisp         # Main LISP entry point
│   └── src/
│       ├── providers/      # Functional provider system
│       ├── config/         # S-expression configuration
│       └── cli/            # Macro-based commands
├── spec/                   # Test suite
└── docs/                   # Documentation
```

## 🔧 Core Components

### 1. Provider System (`lib/ruby/providers/`)

**Base Provider Interface**
```ruby
class BaseProvider
  def chat(model, messages, options = {})
  def list_models
  def health_check
  def supports_streaming?
  def supports_tools?
end
```

**Provider Registry**
```ruby
class ProviderRegistry
  def register_provider(provider)
  def get_provider(name)
  def switch_provider(provider_name, model = nil)
  def health_check_all
end
```

**Benefits:**
- Consistent interface across all providers
- Easy to add new providers
- Health monitoring and capabilities reporting
- Dynamic provider discovery

### 2. Command System (`lib/ruby/cli/`)

**Base Command Interface**
```ruby
class BaseCommand
  def execute(args, context = {})
  def complete(args, context = {})
  def validate_args(args)
  def help
end
```

**Command Registry**
```ruby
class CommandRegistry
  def register_command(command)
  def execute_command(command_line, context)
  def complete_command(input, context)
end
```

**Benefits:**
- Modular command definitions
- Tab completion support
- Context-aware execution
- Automatic help generation

### 3. Configuration System (`lib/ruby/config/`)

**Configuration Management**
```ruby
class Configuration
  def get(key, default = nil)
  def set(key, value)
  def merge!(hash)
  def validate!
  def watch(pattern, &block)
end
```

**Features:**
- Nested configuration access with dot notation
- Environment variable integration
- Hot-reload capabilities
- Validation and type checking
- Change watchers for reactive updates

### 4. LISP Implementation (`lisp/`)

**Functional Provider System**
```lisp
(defun make-provider (name &key chat-fn list-models-fn ...)
(defun call-provider (provider-name model messages &rest options)
(defun provider-compose (&rest provider-names)
(defun provider-retry (provider-name &key max-retries delay)
```

**S-expression Configuration**
```lisp
(defun get-config (key &optional default)
(defun set-config (key value)
(defmacro with-config-binding (bindings &body body)
```

**Macro-based Commands**
```lisp
(defmacro defcommand (name (&rest args) description &key usage examples &body body)
(defun execute-command (command-line &optional context)
```

**Benefits:**
- Pure functional programming patterns
- S-expression native configuration
- Powerful macro system for commands
- Monadic error handling
- Immutable data structures

## 🚀 Key Improvements

### Maintainability
- **90% reduction** in single file complexity (1,499 → ~200 lines per module)
- **Clear separation** of concerns
- **Consistent interfaces** across all components
- **Comprehensive documentation** and examples

### Extensibility
- **Plugin architecture** for providers and commands
- **Dynamic registration** system
- **Hook system** for configuration changes
- **Modular design** allows easy feature additions

### Performance
- **Lazy loading** of providers and commands
- **Caching layer** for configuration and provider responses
- **Connection pooling** for HTTP requests
- **Batch processing** for multiple operations

### Testing
- **Dependency injection** enables easy mocking
- **Isolated modules** can be tested independently
- **Comprehensive test suite** with 81 LSP tests passing
- **Integration tests** for end-to-end workflows

### Error Handling
- **Graceful degradation** when providers fail
- **Circuit breaker** pattern for unreliable services
- **Retry mechanisms** with exponential backoff
- **Detailed logging** and error reporting

## 🔄 Migration Path

### From Monolithic to Modular
1. **Gradual migration**: Old CLI still works during transition
2. **Feature parity**: New modular version has all original features
3. **Enhanced functionality**: Additional capabilities through modularity
4. **Backward compatibility**: Existing configurations continue to work

### Ruby to LISP
1. **Parallel implementation**: Both versions can coexist
2. **Shared protocols**: Common communication formats
3. **Configuration sync**: Ability to share configuration between versions
4. **Feature parity**: LISP version implements all Ruby features

## 📊 Comparison Matrix

| Feature | Original | Modular Ruby | LISP Version |
|---------|----------|--------------|--------------|
| **Lines of Code** | 1,499 (single file) | ~200 per module | ~300 per module |
| **Provider System** | Embedded classes | Registry pattern | Higher-order functions |
| **Configuration** | Scattered logic | Centralized system | S-expressions |
| **Commands** | Inline handlers | Registry pattern | Macro system |
| **Error Handling** | Basic try/catch | Comprehensive system | Monadic patterns |
| **Testing** | Difficult | Easy (DI) | Easy (pure functions) |
| **Extensibility** | Limited | Plugin architecture | Macro extensibility |
| **Performance** | Basic | Optimized | Functional optimization |

## 🎯 Future Enhancements

### Short Term
- Complete test coverage for all modules
- Performance benchmarking and optimization
- Enhanced error recovery mechanisms
- VS Code extension for LSP integration

### Medium Term
- Web interface for remote access
- REST API for programmatic access
- Plugin marketplace for community extensions
- Cross-language provider sharing

### Long Term
- Distributed provider execution
- AI-powered provider optimization
- Advanced caching and persistence
- Real-time collaboration features

## 🔗 Integration Points

### LSP Integration
- **Server**: Full Language Server Protocol implementation
- **Client**: Integrated with tool management
- **Features**: Hover, completion, diagnostics, code actions
- **AI Enhancement**: AI-powered refactoring and optimization

### MCP Integration
- **Protocol**: Model Context Protocol support
- **Extensibility**: Plugin architecture for new tools
- **Security**: Sandboxed execution and validation
- **Discovery**: Automatic server discovery and registration

### Agent System
- **Planning**: Task decomposition and planning
- **Execution**: Automated task execution
- **Learning**: Success rate tracking and optimization
- **Integration**: Works with both Ruby and LISP versions

This architecture provides a solid foundation for future development while maintaining backward compatibility and enabling new innovative features.