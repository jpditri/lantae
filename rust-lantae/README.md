# Lantae Rust Implementation

High-performance Rust implementation of Lantae with focus on speed, safety, and cross-platform compatibility.

## 🦀 Rust Features

### Implementation Status
- 🔄 **In Development** - See [Feature Parity Status](#feature-parity-status)
- 🎯 **Performance Focus** - Optimized for speed and memory efficiency
- 🛡️ **Memory Safety** - Rust's ownership system prevents crashes
- 🌐 **Cross-Platform** - Single binary for multiple platforms

### Rust-Specific Advantages
- **Zero-Cost Abstractions** - Performance without overhead
- **Memory Safety** - No null pointer exceptions or buffer overflows
- **Concurrency** - Safe parallel processing with async/await
- **Cross-Compilation** - Build for any target from any platform
- **Small Binaries** - Optimized release builds

## 🚀 Quick Start

### Prerequisites
- **Rust** 1.70+ (install via [rustup](https://rustup.rs/))
- **Cargo** (included with Rust)

### Installation
```bash
# Clone the Rust implementation
git clone -b rust-implementation https://github.com/jpditri/lantae-cli.git
cd lantae-cli/rust-lantae

# Build release version
cargo build --release

# Run Lantae
./target/release/lantae
```

### Development Build
```bash
# Quick development build
cargo run

# With arguments
cargo run -- --provider ollama "Hello world"

# Run tests
cargo test

# Check code
cargo clippy
cargo fmt
```

## 📖 Usage

### Command Line Interface
```bash
# Interactive mode
lantae

# Single prompt
lantae "Explain quantum computing"

# Specify provider and model
lantae --provider openai --model gpt-4o "Write Rust code"

# Help
lantae --help
```

### Rust-Specific Features
```bash
# High-performance mode (planned)
lantae --fast-mode

# Parallel processing (planned)
lantae --parallel --batch prompts.txt

# Cross-compilation target info
lantae --target-info
```

## 🔧 Configuration

### Configuration File
```toml
# ~/.config/lantae/config.toml
[default]
provider = "ollama"
model = "cogito:latest"

[providers.ollama]
host = "http://localhost:11434"
timeout = 30

[providers.openai]
api_key_env = "OPENAI_API_KEY"
base_url = "https://api.openai.com/v1"
timeout = 60

[ui]
colors = true
progress_bars = true
streaming = true

[performance]
max_concurrent_requests = 4
request_timeout = 120
retry_attempts = 3
```

## 🏗️ Architecture

### Project Structure
```
rust-lantae/
├── Cargo.toml              # Project configuration
├── src/
│   ├── main.rs             # Application entry point
│   ├── lib.rs              # Library root
│   ├── cli/
│   │   ├── mod.rs          # CLI module
│   │   ├── args.rs         # Argument parsing
│   │   └── repl.rs         # REPL implementation
│   ├── providers/
│   │   ├── mod.rs          # Provider traits
│   │   ├── ollama.rs       # Ollama implementation
│   │   ├── openai.rs       # OpenAI implementation
│   │   └── anthropic.rs    # Anthropic implementation
│   ├── config/
│   │   ├── mod.rs          # Configuration management
│   │   └── settings.rs     # Settings structures
│   ├── tools/
│   │   ├── mod.rs          # Tool system
│   │   └── executor.rs     # Tool execution
│   ├── agent/
│   │   ├── mod.rs          # Agent system
│   │   └── planner.rs      # Task planning
│   └── utils/
│       ├── mod.rs          # Utilities
│       ├── logging.rs      # Logging setup
│       └── errors.rs       # Error types
├── tests/                  # Integration tests
├── benches/                # Benchmarks
└── docs/                   # Rust-specific documentation
```

### Core Modules
- **CLI**: Command-line interface and REPL
- **Providers**: Multi-provider LLM interface
- **Config**: Configuration management
- **Tools**: Local tool integration
- **Agent**: Planning and task management
- **Utils**: Logging, errors, utilities

## 🔄 Feature Parity Status

See the main [Feature Parity Document](../docs/FEATURE_PARITY.md) for detailed status compared to other implementations.

### Rust Implementation Roadmap

#### Phase 1: Core Functionality
- [ ] **Basic CLI** - Argument parsing, help system
- [ ] **Ollama Provider** - Local model integration
- [ ] **Configuration** - TOML-based settings
- [ ] **REPL** - Interactive command interface
- [ ] **Error Handling** - Robust error management

#### Phase 2: Provider Expansion
- [ ] **OpenAI Provider** - GPT model support
- [ ] **Anthropic Provider** - Claude integration
- [ ] **Gemini Provider** - Google AI support
- [ ] **Provider Management** - Switching and detection
- [ ] **Streaming Responses** - Real-time output

#### Phase 3: Advanced Features
- [ ] **Tool Integration** - Local command execution
- [ ] **MCP Support** - Protocol implementation
- [ ] **Planning Agent** - Task decomposition
- [ ] **Performance Optimization** - Async processing
- [ ] **Cross-Platform Builds** - Multiple targets

#### Phase 4: Ecosystem Integration
- [ ] **Package Distribution** - Cargo crates
- [ ] **Binary Releases** - Pre-built executables
- [ ] **Integration Tests** - Comprehensive testing
- [ ] **Documentation** - Complete user guides
- [ ] **Benchmarks** - Performance measurements

## 🛠️ Development

### Building
```bash
# Debug build
cargo build

# Release build (optimized)
cargo build --release

# Cross-compile for Windows
cargo build --target x86_64-pc-windows-gnu

# Cross-compile for macOS
cargo build --target x86_64-apple-darwin
```

### Testing
```bash
# Run all tests
cargo test

# Run specific test
cargo test test_ollama_provider

# Run benchmarks
cargo bench

# Test with coverage
cargo tarpaulin --out html
```

### Code Quality
```bash
# Lint with Clippy
cargo clippy -- -D warnings

# Format code
cargo fmt

# Check for unused dependencies
cargo machete

# Security audit
cargo audit
```

## 🚀 Performance

### Optimization Features
- **Async I/O** - Non-blocking network operations
- **Connection Pooling** - Reuse HTTP connections
- **Memory Efficiency** - Zero-copy where possible
- **Parallel Processing** - Concurrent request handling
- **Optimized Builds** - LTO and strip for small binaries

### Benchmarks (Planned)
- Response time comparison with Ruby implementation
- Memory usage analysis
- Concurrent request handling
- Binary size optimization

## 🤝 Contributing

### Rust-Specific Guidelines
1. **Follow Rust conventions** - Use `cargo fmt` and `cargo clippy`
2. **Write tests** - Unit and integration tests required
3. **Document public APIs** - Use doc comments
4. **Handle errors properly** - Use `Result` types
5. **Update feature parity** - Keep documentation current

### Code Style
- Use `rustfmt` for formatting
- Follow Rust naming conventions
- Prefer explicit error handling
- Use `tracing` for logging
- Write comprehensive tests

## 📚 Resources

- [The Rust Programming Language](https://doc.rust-lang.org/book/)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/)
- [Cargo Book](https://doc.rust-lang.org/cargo/)
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)

---

*This Rust implementation prioritizes performance and safety while maintaining feature parity with the Ruby reference implementation.*