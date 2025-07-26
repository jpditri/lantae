# Lantae Vim Plugin

A comprehensive Vim/Neovim plugin that brings AI-powered assistance directly to your editor through the Lantae AI assistant.

## Features

### 🤖 AI-Powered Code Assistance
- **Code Explanation**: Get detailed explanations of your code
- **Code Optimization**: Receive suggestions for performance and style improvements  
- **Test Generation**: Auto-generate comprehensive unit tests
- **Debugging Help**: Get AI assistance with debugging issues
- **Documentation**: Generate comprehensive code documentation

### 💬 Integrated Chat Interface
- Interactive chat window within Vim
- Conversation history management
- Async response handling
- Export conversations

### 🔧 Multi-Provider Support
- **Ollama**: Local AI models (free, private)
- **OpenAI**: GPT-4, GPT-3.5-turbo
- **Anthropic**: Claude 3 models
- **Amazon Bedrock**: Enterprise AI models
- **Google Gemini**: Gemini Pro models
- **Mistral**: Mistral AI models

### 🎯 LSP Integration (Neovim)
- Real-time code analysis
- AI-powered diagnostics
- Intelligent auto-completion
- Code actions and quick fixes

### 📝 Custom Prompt Language
- `.lantae` file support with syntax highlighting
- Structured prompt directives
- Template system for common patterns

## Installation

### Prerequisites
- Vim 8.0+ or Neovim 0.5+
- Lantae CLI tool

#### Install Lantae CLI
```bash
# npm
npm install -g lantae

# Ruby gem
gem install lantae

# Or build from source
git clone https://github.com/jpditri/lantae
cd lantae && make install
```

### Plugin Installation

#### Using vim-plug
```vim
Plug 'lantae/vim-lantae'
```

#### Using Vundle
```vim
Plugin 'lantae/vim-lantae'
```

#### Using Packer (Neovim)
```lua
use 'lantae/vim-lantae'
```

#### Manual Installation
```bash
git clone https://github.com/lantae/vim-lantae ~/.vim/pack/plugins/start/lantae
```

## Configuration

Add to your `.vimrc` or `init.vim`:

```vim
" Basic configuration
let g:lantae_provider = 'ollama'          " AI provider
let g:lantae_model = 'cogito:latest'      " AI model
let g:lantae_temperature = 0.1            " Response creativity

" Chat interface
let g:lantae_chat_window_position = 'right'  " Chat window position
let g:lantae_chat_window_size = 60           " Chat window size

" LSP integration (Neovim only)
let g:lantae_enable_lsp = 1               " Enable LSP support
let g:lantae_lsp_port = 7777              " LSP server port

" Features
let g:lantae_enable_auto_complete = 1     " AI auto-completion
let g:lantae_enable_diagnostics = 1       " AI diagnostics
```

### API Keys Setup

For cloud providers, set environment variables:

```bash
# OpenAI
export OPENAI_API_KEY="sk-..."

# Anthropic
export ANTHROPIC_API_KEY="sk-ant-..."

# Google Gemini
export GOOGLE_API_KEY="..."

# For Ollama (local), no API key needed
```

## Usage

### Basic Commands

| Command | Description |
|---------|-------------|
| `:LantaeChat` | Open chat interface |
| `:LantaeExplain` | Explain selected code |
| `:LantaeOptimize` | Optimize selected code |
| `:LantaeGenerateTests` | Generate unit tests |
| `:LantaeProvider openai` | Switch to OpenAI |
| `:LantaeStatus` | Show current status |

### Default Key Mappings

```vim
" Chat
<leader>lc    " Open chat
<leader>lt    " Toggle chat
<leader>lx    " Clear chat

" Code analysis (visual mode)
<leader>le    " Explain code
<leader>lo    " Optimize code  
<leader>lg    " Generate tests
<leader>ld    " Debug code
<leader>lD    " Document code

" Configuration
<leader>lp    " List providers
<leader>lm    " List models
<leader>ls    " Show status

" LSP (Neovim)
<leader>lL    " Start LSP
<leader>lS    " Stop LSP
<leader>lR    " Restart LSP
```

### Chat Interface

1. Open chat with `:LantaeChat` or `<leader>lc`
2. Type your message and press `<CR>` to send
3. Use `i` to enter input mode for multi-line messages
4. Press `q` to close, `c` to clear history

### Code Analysis Workflow

1. Select code in visual mode
2. Press `<leader>le` to explain, `<leader>lo` to optimize, etc.
3. Results appear in chat or new buffer
4. Apply suggestions manually or use LSP quick fixes

### Prompt Files (.lantae)

Create structured prompts with special syntax:

```lantae
@provider openai
@model gpt-4  
@temperature 0.1

@system
You are a helpful coding assistant specializing in Python.

@user
Optimize this function for performance:

```python
def slow_function(data):
    result = []
    for item in data:
        if item % 2 == 0:
            result.append(item * 2)
    return result
```

@context
This function processes large datasets (1M+ items).
```

Commands in `.lantae` files:
- `<leader>le` - Execute prompt
- `<leader>lv` - Validate syntax  
- `<leader>lp` - Preview assembled prompt
- `<leader>lx` - Export to different formats

## LSP Features (Neovim)

When LSP is enabled, you get:

### Real-time Analysis
- Code issues detected as you type
- Performance suggestions
- Security vulnerability warnings

### Smart Completions  
- Context-aware suggestions
- AI-powered auto-completion
- Multi-language support

### Code Actions
- Quick fixes for common issues
- Refactoring suggestions
- Import organization

### LSP Key Mappings
```vim
K              " Hover information
gd             " Go to definition  
gr             " Find references
<leader>la     " Code actions
<leader>lf     " Format buffer
<leader>lr     " Rename symbol
[d / ]d        " Navigate diagnostics
```

## Provider Configuration

### Ollama (Local)
```vim
let g:lantae_provider = 'ollama'
let g:lantae_model = 'cogito:latest'
```

Ensure Ollama is running:
```bash
ollama serve
ollama pull cogito:latest
```

### OpenAI
```vim
let g:lantae_provider = 'openai'  
let g:lantae_model = 'gpt-4'
```

### Anthropic Claude
```vim
let g:lantae_provider = 'anthropic'
let g:lantae_model = 'claude-3-sonnet'
```

### Switch Providers Dynamically
```vim
:LantaeProvider ollama
:LantaeModel llama2:latest
```

## Advanced Usage

### Custom Commands

Create your own commands:

```vim
" Explain with context
command! -range ExplainWithContext 
  \ call lantae#chat#send('Explain this code in the context of web development: ' . 
  \ join(getline(<line1>, <line2>), "\n"))

" Code review
command! -range CodeReview
  \ call lantae#code#security_review()
```

### Integration with Other Plugins

Works well with:
- **fzf.vim**: Search through chat history
- **vim-test**: Generate and run tests
- **coc.nvim**: Additional LSP features
- **telescope.nvim**: Provider/model selection

### Scripting Examples

```vim
" Auto-generate tests on save
autocmd BufWritePost *.py call lantae#code#generate_tests()

" Show status in statusline
set statusline+=%{LantaeStatusline()}

" Quick provider switching
nnoremap <F9> :LantaeProvider ollama<CR>
nnoremap <F10> :LantaeProvider openai<CR>
```

## Troubleshooting

### Common Issues

**LSP won't start**
```bash
# Check CLI installation
lantae --version

# Test LSP manually  
lantae --lsp --port 7777

# Check Neovim LSP logs
:LspLog
```

**Provider connection failed**
```bash
# Test connection
lantae --provider ollama --test

# Check API keys
echo $OPENAI_API_KEY

# For Ollama
curl http://localhost:11434/api/tags
```

**Performance issues**
```vim
" Disable real-time features
let g:lantae_enable_diagnostics = 0
let g:lantae_enable_auto_complete = 0

" Use local provider
let g:lantae_provider = 'ollama'
```

### Debug Mode

Enable debug output:
```vim
let g:lantae_debug = 1
```

Check logs:
```vim
:messages
:LantaeStatus
```

## Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Development Setup
```bash
git clone https://github.com/lantae/vim-lantae
cd vim-lantae

# Test with your Vim config
vim -u vimrc.test
```

## License

MIT License - see [LICENSE](LICENSE) file.

## Support

- 📚 [Documentation](./doc/lantae.txt)
- 🐛 [Issues](https://github.com/lantae/vim-lantae/issues)  
- 💬 [Discussions](https://github.com/lantae/vim-lantae/discussions)

---

**Happy coding with AI assistance! 🚀**