# Lantae Migration Guide

## Overview

This guide covers migration paths between different versions of Lantae and between the Ruby and LISP implementations.

## Table of Contents

1. [Ruby to LISP Migration](#ruby-to-lisp-migration)
2. [Version Migration](#version-migration)
3. [Configuration Migration](#configuration-migration)
4. [Plugin Migration](#plugin-migration)
5. [Common Issues](#common-issues)

## Ruby to LISP Migration

### Installation Changes

#### Ruby Version
```bash
# Install Ruby dependencies
bundle install

# Run Lantae
./bin/lantae
```

#### LISP Version
```bash
# Install SBCL
brew install sbcl  # macOS
apt-get install sbcl  # Ubuntu/Debian

# Run Lantae
./bin/lantae-lisp
```

### Command Differences

#### Ruby Commands
```bash
# Interactive mode
lantae

# Single prompt
lantae "Explain Ruby blocks"

# With options
lantae --model gpt-4 --provider openai "Hello"
```

#### LISP Commands
```bash
# Interactive mode
lantae-lisp

# Single prompt
lantae-lisp "Explain LISP macros"

# With options
lantae-lisp --model gpt-4 --provider openai "Hello"
```

### REPL Differences

#### Ruby REPL
```
> /help              # Show help
> /provider openai   # Switch provider
> /model gpt-4       # Switch model
> /clear             # Clear history
> Hello, world!      # Send prompt
```

#### LISP REPL
```
> (help)             # Show help (LISP function)
> help               # Show help (command)
> provider openai    # Switch provider
> model gpt-4        # Switch model
> clear              # Clear history
> Hello, world!      # Send prompt
> (+ 1 2 3)         # Execute LISP code
```

### Configuration Migration

#### Ruby Configuration (config/config.yml)
```yaml
model: "cogito:latest"
provider: "ollama"
url: "http://localhost:11434"
temperature: 0.1
auto_accept: false
planning_mode: false
agent_mode: false
```

#### LISP Configuration (config/lantae.lisp)
```lisp
(:model "cogito:latest"
 :provider "ollama"
 :url "http://localhost:11434"
 :temperature 0.1
 :auto-accept nil
 :planning-mode nil
 :agent-mode nil)
```

### Feature Parity

| Feature | Ruby | LISP | Notes |
|---------|------|------|-------|
| Basic Chat | ✓ | ✓ | Full compatibility |
| Streaming | ✓ | ⚠️ | LISP implementation in progress |
| Multiple Providers | ✓ | ✓ | Same providers supported |
| Configuration | ✓ | ✓ | Different formats |
| Plugins | ✓ | ⚠️ | Different plugin systems |
| LSP | ✓ | ✓ | Same protocol |
| MCP | ✓ | ⚠️ | LISP implementation planned |
| Hooks | ✓ | ✓ | Different implementations |

## Version Migration

### From v0.x to v1.0

#### Breaking Changes
1. Configuration file format changed
2. Plugin API updated
3. Command line options standardized

#### Migration Steps

1. **Update Configuration**
   ```bash
   # Backup old config
   cp config/config.yml config/config.yml.backup
   
   # Generate new config
   lantae --generate-config
   ```

2. **Update Plugins**
   - Review plugin API changes
   - Update plugin initialization
   - Test plugin functionality

3. **Update Scripts**
   - Check for deprecated command line options
   - Update automation scripts

### Future Version Considerations

- Semantic versioning will be followed
- Deprecation warnings before breaking changes
- Migration tools will be provided

## Configuration Migration

### Environment Variables

Both Ruby and LISP versions support the same environment variables:

```bash
export LANTAE_MODEL="gpt-4"
export LANTAE_PROVIDER="openai"
export LANTAE_TEMPERATURE="0.7"
export OPENAI_API_KEY="your-key"
export ANTHROPIC_API_KEY="your-key"
```

### Configuration Precedence

1. Command line arguments (highest)
2. Environment variables
3. Configuration file
4. Built-in defaults (lowest)

### Provider-Specific Migration

#### Ollama
No changes required - same configuration works for both versions.

#### OpenAI
```bash
# Ruby and LISP both use:
export OPENAI_API_KEY="sk-..."
```

#### Anthropic
```bash
# Ruby and LISP both use:
export ANTHROPIC_API_KEY="sk-ant-..."
```

## Plugin Migration

### Ruby Plugins

Ruby plugins use class-based architecture:

```ruby
# plugins/my_plugin.rb
module Lantae
  module Plugins
    class MyPlugin < Base
      def initialize(context)
        @context = context
      end
      
      def on_before_request(messages, options)
        # Modify request
      end
    end
  end
end
```

### LISP Plugins

LISP uses functional approach with macros:

```lisp
;; plugins/my-plugin.lisp
(defpackage :my-plugin
  (:use :cl :lantae))

(in-package :my-plugin)

(defun before-request-hook (messages options)
  ;; Modify request
  messages)

(register-hook :before-request #'before-request-hook)
```

### Plugin Compatibility

Plugins are not directly compatible between Ruby and LISP versions due to fundamental language differences. Migration requires rewriting plugins in the target language.

## Common Issues

### Issue 1: Command Not Found

**Ruby**
```bash
# Ensure executable permissions
chmod +x bin/lantae

# Add to PATH
export PATH="$PATH:/path/to/lantae/bin"
```

**LISP**
```bash
# Ensure SBCL is installed
which sbcl

# Ensure executable permissions
chmod +x bin/lantae-lisp
```

### Issue 2: Configuration Not Loading

**Ruby**
- Check YAML syntax
- Verify file location: `config/config.yml`
- Check file permissions

**LISP**
- Check S-expression syntax
- Verify file location: `config/lantae.lisp`
- Ensure proper parentheses matching

### Issue 3: Provider Connection Issues

Both versions:
1. Verify provider is running (for Ollama)
2. Check API keys are set (for cloud providers)
3. Verify network connectivity
4. Check firewall settings

### Issue 4: Different Output Formats

The LISP version may show more diagnostic information by default. To suppress:

```bash
# Redirect stderr
lantae-lisp 2>/dev/null

# Or set quiet mode in config
(:verbose nil)
```

## Best Practices

### 1. Gradual Migration
- Run both versions in parallel initially
- Migrate configurations first
- Test thoroughly before switching

### 2. Backup Strategy
- Keep configuration backups
- Document customizations
- Version control your configs

### 3. Testing
- Test each provider after migration
- Verify plugin functionality
- Check performance differences

### 4. Documentation
- Document any custom modifications
- Keep migration notes
- Share learnings with team

## Getting Help

### Resources
- GitHub Issues: Report bugs and feature requests
- Documentation: Check `/docs` directory
- Community: Discussions and Q&A

### Debugging
```bash
# Ruby version debug
LANTAE_DEBUG=1 lantae

# LISP version debug
lantae-lisp --eval "(setf *debug* t)"
```

## Conclusion

Migration between Lantae versions and implementations is straightforward for basic usage. The main considerations are:

1. Configuration format differences
2. Plugin compatibility
3. REPL interaction differences
4. Language-specific features

Choose the implementation that best fits your workflow and environment. Both versions are actively maintained and feature-compatible for core functionality.