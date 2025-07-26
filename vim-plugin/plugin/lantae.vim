" Lantae Vim Plugin
" Provides AI assistance integrated with Vim/Neovim
" Author: Lantae Team
" License: MIT

if exists('g:loaded_lantae')
  finish
endif
let g:loaded_lantae = 1

" Save user's cpoptions and set it to Vim default
let s:save_cpo = &cpo
set cpo&vim

" Plugin configuration
if !exists('g:lantae_provider')
  let g:lantae_provider = 'ollama'
endif

if !exists('g:lantae_model')
  let g:lantae_model = 'cogito:latest'
endif

if !exists('g:lantae_temperature')
  let g:lantae_temperature = 0.1
endif

if !exists('g:lantae_enable_lsp')
  let g:lantae_enable_lsp = 1
endif

if !exists('g:lantae_lsp_port')
  let g:lantae_lsp_port = 7777
endif

if !exists('g:lantae_chat_window_position')
  let g:lantae_chat_window_position = 'right'
endif

if !exists('g:lantae_chat_window_size')
  let g:lantae_chat_window_size = 60
endif

if !exists('g:lantae_enable_auto_complete')
  let g:lantae_enable_auto_complete = 1
endif

if !exists('g:lantae_enable_diagnostics')
  let g:lantae_enable_diagnostics = 1
endif

if !exists('g:lantae_debug')
  let g:lantae_debug = 0
endif

" Global variables for plugin state
let g:lantae_chat_history = []
let g:lantae_current_conversation = []
let g:lantae_lsp_client_id = -1
let g:lantae_chat_buffer = -1
let g:lantae_status = {'connected': 0, 'provider': g:lantae_provider, 'model': g:lantae_model}

" Commands
command! -nargs=0 LantaeChat call lantae#chat#open()
command! -nargs=0 LantaeChatToggle call lantae#chat#toggle()
command! -nargs=0 LantaeChatClear call lantae#chat#clear()
command! -nargs=1 LantaeChatSend call lantae#chat#send(<q-args>)
command! -range LantaeExplain call lantae#code#explain()
command! -range LantaeOptimize call lantae#code#optimize()
command! -range LantaeGenerateTests call lantae#code#generate_tests()
command! -range LantaeDebug call lantae#code#debug()
command! -range LantaeDocument call lantae#code#document()
command! -nargs=1 LantaeProvider call lantae#config#set_provider(<q-args>)
command! -nargs=1 LantaeModel call lantae#config#set_model(<q-args>)
command! -nargs=0 LantaeStatus call lantae#status#show()
command! -nargs=0 LantaeProviders call lantae#config#list_providers()
command! -nargs=0 LantaeModels call lantae#config#list_models()
command! -nargs=0 LantaeLSPStart call lantae#lsp#start()
command! -nargs=0 LantaeLSPStop call lantae#lsp#stop()
command! -nargs=0 LantaeLSPRestart call lantae#lsp#restart()
command! -nargs=0 LantaeLSPStatus call lantae#lsp#status()
command! -nargs=? LantaePrompt call lantae#prompt#create(<q-args>)

" Key mappings
if !exists('g:lantae_no_mappings') || !g:lantae_no_mappings
  " Chat mappings
  nnoremap <silent> <leader>lc :LantaeChat<CR>
  nnoremap <silent> <leader>lt :LantaeChatToggle<CR>
  nnoremap <silent> <leader>lx :LantaeChatClear<CR>
  
  " Code analysis mappings
  vnoremap <silent> <leader>le :LantaeExplain<CR>
  vnoremap <silent> <leader>lo :LantaeOptimize<CR>
  vnoremap <silent> <leader>lg :LantaeGenerateTests<CR>
  vnoremap <silent> <leader>ld :LantaeDebug<CR>
  vnoremap <silent> <leader>lD :LantaeDocument<CR>
  
  " Provider/model mappings
  nnoremap <silent> <leader>lp :LantaeProviders<CR>
  nnoremap <silent> <leader>lm :LantaeModels<CR>
  nnoremap <silent> <leader>ls :LantaeStatus<CR>
  
  " LSP mappings
  nnoremap <silent> <leader>lL :LantaeLSPStart<CR>
  nnoremap <silent> <leader>lS :LantaeLSPStop<CR>
  nnoremap <silent> <leader>lR :LantaeLSPRestart<CR>
  
  " Prompt creation
  nnoremap <silent> <leader>lP :LantaePrompt<CR>
endif

" Auto commands
augroup LantaePlugin
  autocmd!
  
  " Initialize LSP on startup if enabled
  if g:lantae_enable_lsp && has('nvim')
    autocmd VimEnter * call lantae#lsp#auto_start()
  endif
  
  " Auto-complete integration
  if g:lantae_enable_auto_complete
    autocmd CursorHoldI * call lantae#completion#trigger()
  endif
  
  " Diagnostics integration
  if g:lantae_enable_diagnostics
    autocmd BufWritePost * call lantae#diagnostics#analyze_buffer()
    autocmd CursorHold * call lantae#diagnostics#show_hover()
  endif
  
  " Chat window auto-commands
  autocmd BufEnter __Lantae_Chat__ call lantae#chat#setup_buffer()
  autocmd BufLeave __Lantae_Chat__ call lantae#chat#cleanup_buffer()
  
  " File type detection for .lantae files
  autocmd BufNewFile,BufRead *.lantae,*.lnt setfiletype lantae
  
augroup END

" Status line integration
function! LantaeStatusline()
  if g:lantae_status.connected
    return printf('[Lantae: %s:%s]', g:lantae_status.provider, g:lantae_status.model)
  else
    return '[Lantae: disconnected]'
  endif
endfunction

" Add to statusline if user wants it
if exists('g:lantae_show_in_statusline') && g:lantae_show_in_statusline
  set statusline+=%{LantaeStatusline()}
endif

" Restore user's cpoptions
let &cpo = s:save_cpo
unlet s:save_cpo