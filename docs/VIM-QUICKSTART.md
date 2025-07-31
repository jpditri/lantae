# Vim/Neovim Quickstart with Lantae CLI & LSP

This guide walks you through integrating Lantae into Vim/Neovim for AI-powered chat, code analysis, and LSP support.

## 1. Prerequisites

- **Lantae CLI** installed (`gem install lantae` or via `install.sh`).
- **Node.js** (for LSP server).
- Vim 8.0+ or Neovim 0.5+.

## 2. Install the Vim Plugin

Clone into your Vim/Neovim plugin directory:
```bash
# Vim (pack)
git clone https://github.com/jpditri/lantae.git \
  ~/.vim/pack/lantae/start/vim-lantae
# Neovim (packer or native)
git clone https://github.com/jpditri/lantae.git \
  ~/.local/share/nvim/site/pack/lantae/start/vim-lantae
```
Enable it in your config:
```vim
packadd vim-lantae
```

## 3. Key Mappings

```vim
nnoremap <Leader>lc :LantaeChat<CR>        " Open chat
vnoremap <Leader>le :LantaeExplain<CR>     " Explain code
vnoremap <Leader>lo :LantaeOptimize<CR>    " Optimize code
vnoremap <Leader>lt :LantaeGenerateTests<CR> " Generate tests
```

## 4. Start LSP Server

In a separate shell:
```bash
lantae --enable-lsp --lsp-port 7777
```

## 5. Connect Editor to LSP

### Neovim built‑in LSP
```lua
require'lspconfig'.lantae.setup{
  cmd = {'lantae-lsp','--port','7777','--stdio'},
  filetypes = {'ruby','python','javascript','go','rust'},
  root_dir = vim.loop.cwd
}
```

### coc.nvim
```jsonc
// coc-settings.json
{
  "languageserver": {
    "lantae": {
      "command": "lantae-lsp",
      "args": ["--port","7777","--stdio"],
      "filetypes": ["ruby","python","javascript","go","rust"]
    }
  }
}
```

## 6. Chat & Analysis Commands

```vim
:LantaeChat        " Interactive chat panel
:LantaeAnalyze     " AI-powered code analysis on current file
:LantaeFormat      " Format buffer via AI
:LantaeComplete    " AI completion at cursor
```