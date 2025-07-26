" Lantae LSP Module
" Handles Language Server Protocol integration

function! lantae#lsp#start()
  if !has('nvim')
    echohl ErrorMsg
    echo 'LSP support requires Neovim'
    echohl None
    return
  endif
  
  if g:lantae_lsp_client_id != -1
    echo 'Lantae LSP is already running'
    return
  endif
  
  echo 'Starting Lantae LSP server...'
  
  let lsp_config = {
    \ 'name': 'lantae',
    \ 'cmd': ['lantae', '--lsp', '--port', string(g:lantae_lsp_port)],
    \ 'root_dir': getcwd(),
    \ 'settings': {
      \   'lantae': {
        \     'provider': g:lantae_provider,
        \     'model': g:lantae_model,
        \     'temperature': g:lantae_temperature,
        \     'enableDiagnostics': g:lantae_enable_diagnostics,
        \     'enableCompletion': g:lantae_enable_auto_complete
      \   }
    \ },
    \ 'on_attach': function('s:on_lsp_attach'),
    \ 'on_exit': function('s:on_lsp_exit'),
    \ 'capabilities': s:get_lsp_capabilities()
  \ }
  
  try
    let g:lantae_lsp_client_id = v:lua.vim.lsp.start_client(lsp_config)
    
    if g:lantae_lsp_client_id > 0
      echo 'Lantae LSP server started (client ID: ' . g:lantae_lsp_client_id . ')'
      
      " Attach to current buffer if supported
      if s:is_supported_filetype(&filetype)
        call v:lua.vim.lsp.buf_attach_client(0, g:lantae_lsp_client_id)
      endif
    else
      let g:lantae_lsp_client_id = -1
      echohl ErrorMsg
      echo 'Failed to start Lantae LSP server'
      echohl None
    endif
  catch
    let g:lantae_lsp_client_id = -1
    echohl ErrorMsg
    echo 'Error starting LSP: ' . v:exception
    echohl None
  endtry
endfunction

function! lantae#lsp#stop()
  if g:lantae_lsp_client_id == -1
    echo 'Lantae LSP is not running'
    return
  endif
  
  echo 'Stopping Lantae LSP server...'
  
  try
    call v:lua.vim.lsp.stop_client(g:lantae_lsp_client_id)
    let g:lantae_lsp_client_id = -1
    echo 'Lantae LSP server stopped'
  catch
    echohl ErrorMsg
    echo 'Error stopping LSP: ' . v:exception
    echohl None
  endtry
endfunction

function! lantae#lsp#restart()
  call lantae#lsp#stop()
  sleep 1
  call lantae#lsp#start()
endfunction

function! lantae#lsp#status()
  if g:lantae_lsp_client_id == -1
    echo 'Lantae LSP: Not running'
  else
    let client_info = v:lua.vim.lsp.get_client_by_id(g:lantae_lsp_client_id)
    if empty(client_info)
      echo 'Lantae LSP: Client ID ' . g:lantae_lsp_client_id . ' (disconnected)'
      let g:lantae_lsp_client_id = -1
    else
      echo 'Lantae LSP: Running (client ID: ' . g:lantae_lsp_client_id . ')'
      echo '  Root directory: ' . client_info.config.root_dir
      echo '  Attached buffers: ' . len(client_info.attached_buffers)
    endif
  endif
endfunction

function! lantae#lsp#auto_start()
  " Auto-start LSP if configured and supported
  if g:lantae_enable_lsp && s:is_supported_filetype(&filetype)
    call lantae#lsp#start()
  endif
endfunction

function! lantae#lsp#request_completion()
  if g:lantae_lsp_client_id == -1 || !g:lantae_enable_auto_complete
    return
  endif
  
  " Request completion from LSP server
  try
    call v:lua.vim.lsp.buf.completion()
  catch
    if g:lantae_debug
      echo 'LSP completion error: ' . v:exception
    endif
  endtry
endfunction

function! lantae#lsp#request_hover()
  if g:lantae_lsp_client_id == -1
    return
  endif
  
  " Request hover information
  try
    call v:lua.vim.lsp.buf.hover()
  catch
    if g:lantae_debug
      echo 'LSP hover error: ' . v:exception
    endif
  endtry
endfunction

function! lantae#lsp#request_code_action()
  if g:lantae_lsp_client_id == -1
    return
  endif
  
  " Request code actions
  try
    call v:lua.vim.lsp.buf.code_action()
  catch
    if g:lantae_debug
      echo 'LSP code action error: ' . v:exception
    endif
  endtry
endfunction

function! lantae#lsp#format_buffer()
  if g:lantae_lsp_client_id == -1
    return
  endif
  
  " Request buffer formatting
  try
    call v:lua.vim.lsp.buf.formatting()
  catch
    if g:lantae_debug
      echo 'LSP formatting error: ' . v:exception
    endif
  endtry
endfunction

" Private functions
function! s:is_supported_filetype(filetype)
  let supported_types = [
    \ 'python', 'javascript', 'typescript', 'ruby', 'go', 'rust', 'java',
    \ 'cpp', 'c', 'vim', 'lua', 'php', 'html', 'css', 'json', 'yaml',
    \ 'markdown', 'sh', 'bash', 'zsh', 'lantae'
  \ ]
  
  return index(supported_types, a:filetype) != -1
endfunction

function! s:get_lsp_capabilities()
  " Get LSP capabilities with completion support
  if exists('*v:lua.vim.lsp.protocol.make_client_capabilities')
    return v:lua.vim.lsp.protocol.make_client_capabilities()
  else
    return {}
  endif
endfunction

function! s:on_lsp_attach(client, bufnr)
  " Set up LSP keybindings for the buffer
  if g:lantae_debug
    echo 'Lantae LSP attached to buffer ' . a:bufnr
  endif
  
  " Set up buffer-local keybindings
  let buffer_mappings = [
    \ ['n', 'K', ':lua vim.lsp.buf.hover()<CR>'],
    \ ['n', 'gd', ':lua vim.lsp.buf.definition()<CR>'],
    \ ['n', 'gr', ':lua vim.lsp.buf.references()<CR>'],
    \ ['n', '<leader>la', ':lua vim.lsp.buf.code_action()<CR>'],
    \ ['n', '<leader>lf', ':lua vim.lsp.buf.formatting()<CR>'],
    \ ['n', '<leader>lr', ':lua vim.lsp.buf.rename()<CR>'],
    \ ['n', '<leader>ld', ':lua vim.lsp.diagnostic.show_line_diagnostics()<CR>'],
    \ ['n', '[d', ':lua vim.lsp.diagnostic.goto_prev()<CR>'],
    \ ['n', ']d', ':lua vim.lsp.diagnostic.goto_next()<CR>']
  \ ]
  
  for [mode, lhs, rhs] in buffer_mappings
    execute 'nnoremap <buffer> <silent> ' . lhs . ' ' . rhs
  endfor
  
  " Set up auto-completion
  if g:lantae_enable_auto_complete
    setlocal omnifunc=v:lua.vim.lsp.omnifunc
  endif
  
  " Set up diagnostics
  if g:lantae_enable_diagnostics
    call v:lua.vim.lsp.diagnostic.on_publish_diagnostics(nil, {}, {})
  endif
endfunction

function! s:on_lsp_exit(code, signal, client_id)
  if g:lantae_debug
    echo 'Lantae LSP exited with code ' . a:code . ', signal ' . a:signal
  endif
  
  let g:lantae_lsp_client_id = -1
  
  if a:code != 0
    echohl ErrorMsg
    echo 'Lantae LSP server crashed (exit code: ' . a:code . ')'
    echohl None
  endif
endfunction

" Auto-completion integration
function! lantae#lsp#setup_completion()
  if !has('nvim') || g:lantae_lsp_client_id == -1
    return
  endif
  
  " Set up completion menu customization
  set completeopt=menu,menuone,noselect
  
  " Auto-trigger completion
  augroup LantaeLSPCompletion
    autocmd!
    if g:lantae_enable_auto_complete
      autocmd CursorHoldI * call lantae#lsp#request_completion()
    endif
  augroup END
endfunction

" Diagnostics integration
function! lantae#lsp#setup_diagnostics()
  if !has('nvim') || g:lantae_lsp_client_id == -1
    return
  endif
  
  " Configure diagnostic display
  if exists('*v:lua.vim.lsp.diagnostic.config')
    call v:lua.vim.lsp.diagnostic.config({
      \ 'virtual_text': v:true,
      \ 'signs': v:true,
      \ 'underline': v:true,
      \ 'update_in_insert': v:false,
      \ 'severity_sort': v:true
    \ })
  endif
  
  " Set up diagnostic signs
  if exists('*sign_define')
    call sign_define('LspDiagnosticsSignError', {'text': '✗', 'texthl': 'LspDiagnosticsSignError'})
    call sign_define('LspDiagnosticsSignWarning', {'text': '⚠', 'texthl': 'LspDiagnosticsSignWarning'})
    call sign_define('LspDiagnosticsSignInformation', {'text': 'ℹ', 'texthl': 'LspDiagnosticsSignInformation'})
    call sign_define('LspDiagnosticsSignHint', {'text': '💡', 'texthl': 'LspDiagnosticsSignHint'})
  endif
endfunction

" Code action integration
function! lantae#lsp#show_code_actions()
  if g:lantae_lsp_client_id == -1
    echo 'LSP not running'
    return
  endif
  
  call lantae#lsp#request_code_action()
endfunction