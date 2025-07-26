" Lantae Chat Module
" Handles chat interface and interactions

function! lantae#chat#open()
  call s:create_chat_window()
  call s:setup_chat_buffer()
  call s:display_welcome_message()
endfunction

function! lantae#chat#toggle()
  let chat_winnr = bufwinnr('__Lantae_Chat__')
  if chat_winnr != -1
    execute chat_winnr . 'wincmd c'
  else
    call lantae#chat#open()
  endif
endfunction

function! lantae#chat#clear()
  let g:lantae_chat_history = []
  let g:lantae_current_conversation = []
  call s:refresh_chat_display()
  echo "Chat history cleared"
endfunction

function! lantae#chat#send(message)
  if empty(a:message)
    let message = input('Message: ')
    if empty(message)
      return
    endif
  else
    let message = a:message
  endif
  
  call s:add_message('user', message)
  call s:show_typing_indicator()
  
  " Send to AI provider asynchronously
  if has('nvim')
    call s:send_async_nvim(message)
  elseif has('job')
    call s:send_async_vim8(message)
  else
    call s:send_sync(message)
  endif
endfunction

function! lantae#chat#setup_buffer()
  " Configure chat buffer settings
  setlocal buftype=nofile
  setlocal noswapfile
  setlocal nowrap
  setlocal nonumber
  setlocal norelativenumber
  setlocal cursorline
  setlocal filetype=lantae-chat
  
  " Set up key mappings for chat buffer
  nnoremap <buffer> <CR> :call <SID>send_current_line()<CR>
  nnoremap <buffer> i :call <SID>start_input_mode()<CR>
  nnoremap <buffer> o :call <SID>start_input_mode()<CR>
  nnoremap <buffer> q :close<CR>
  nnoremap <buffer> c :LantaeChatClear<CR>
  inoremap <buffer> <CR> <Esc>:call <SID>send_input_line()<CR>
  
  " Highlight groups
  syntax match LantaeUser "^You:"
  syntax match LantaeAssistant "^Lantae:"
  syntax match LantaeSystem "^System:"
  syntax match LantaeTimestamp "\[\d\d:\d\d:\d\d\]"
  
  highlight LantaeUser ctermfg=blue guifg=#4a90e2
  highlight LantaeAssistant ctermfg=green guifg=#7ed321
  highlight LantaeSystem ctermfg=yellow guifg=#f5a623
  highlight LantaeTimestamp ctermfg=gray guifg=#9b9b9b
endfunction

function! lantae#chat#cleanup_buffer()
  " Cleanup when leaving chat buffer
endfunction

" Private functions
function! s:create_chat_window()
  " Check if chat window already exists
  let chat_winnr = bufwinnr('__Lantae_Chat__')
  if chat_winnr != -1
    execute chat_winnr . 'wincmd w'
    return
  endif
  
  " Create new window
  if g:lantae_chat_window_position ==# 'right'
    execute 'vertical rightbelow ' . g:lantae_chat_window_size . 'new __Lantae_Chat__'
  elseif g:lantae_chat_window_position ==# 'left'
    execute 'vertical leftabove ' . g:lantae_chat_window_size . 'new __Lantae_Chat__'
  elseif g:lantae_chat_window_position ==# 'top'
    execute 'topleft ' . (g:lantae_chat_window_size / 2) . 'new __Lantae_Chat__'
  else
    execute 'botright ' . (g:lantae_chat_window_size / 2) . 'new __Lantae_Chat__'
  endif
  
  let g:lantae_chat_buffer = bufnr('%')
endfunction

function! s:setup_chat_buffer()
  " Make buffer modifiable for setup
  setlocal modifiable
  
  " Clear existing content
  silent %delete _
  
  " Add header
  call append(0, ['=== Lantae AI Chat ===', 
                \ 'Provider: ' . g:lantae_provider . ' | Model: ' . g:lantae_model,
                \ 'Commands: <CR>=send, i=input, q=quit, c=clear',
                \ '', ''])
  
  " Set buffer as non-modifiable
  setlocal nomodifiable
  
  " Go to end
  normal! G
endfunction

function! s:display_welcome_message()
  call s:add_message('system', 'Welcome to Lantae! How can I help you today?')
endfunction

function! s:add_message(role, content)
  let timestamp = strftime('[%H:%M:%S]')
  let role_name = a:role ==# 'user' ? 'You' : 
                \ a:role ==# 'assistant' ? 'Lantae' : 'System'
  
  " Add to history
  call add(g:lantae_chat_history, {
    \ 'role': a:role,
    \ 'content': a:content,
    \ 'timestamp': timestamp
  \ })
  
  " Add to current conversation context
  if a:role !=# 'system'
    call add(g:lantae_current_conversation, {
      \ 'role': a:role,
      \ 'content': a:content
    \ })
  endif
  
  " Display in chat buffer
  call s:append_to_chat_buffer(timestamp . ' ' . role_name . ': ' . a:content)
endfunction

function! s:append_to_chat_buffer(text)
  let chat_winnr = bufwinnr('__Lantae_Chat__')
  if chat_winnr == -1
    return
  endif
  
  " Switch to chat window
  let current_winnr = winnr()
  execute chat_winnr . 'wincmd w'
  
  " Make buffer modifiable
  setlocal modifiable
  
  " Add text
  call append(line('$'), split(a:text, '\n'))
  
  " Scroll to bottom
  normal! G
  
  " Make buffer non-modifiable again
  setlocal nomodifiable
  
  " Return to previous window
  execute current_winnr . 'wincmd w'
endfunction

function! s:show_typing_indicator()
  call s:append_to_chat_buffer('Lantae is typing...')
endfunction

function! s:hide_typing_indicator()
  let chat_winnr = bufwinnr('__Lantae_Chat__')
  if chat_winnr == -1
    return
  endif
  
  let current_winnr = winnr()
  execute chat_winnr . 'wincmd w'
  
  setlocal modifiable
  
  " Remove last line if it's the typing indicator
  if getline(line('$')) =~# 'typing\.\.\.'
    silent $delete _
  endif
  
  setlocal nomodifiable
  execute current_winnr . 'wincmd w'
endfunction

function! s:send_async_nvim(message)
  let cmd = ['lantae', '--provider', g:lantae_provider, '--model', g:lantae_model, a:message]
  
  let job_id = jobstart(cmd, {
    \ 'on_stdout': function('s:on_response_nvim'),
    \ 'on_stderr': function('s:on_error_nvim'),
    \ 'on_exit': function('s:on_exit_nvim')
  \ })
  
  if job_id <= 0
    call s:on_error('Failed to start Lantae process')
  endif
endfunction

function! s:send_async_vim8(message)
  let cmd = 'lantae --provider ' . g:lantae_provider . ' --model ' . g:lantae_model . ' "' . escape(a:message, '"') . '"'
  
  let job = job_start(cmd, {
    \ 'callback': function('s:on_response_vim8'),
    \ 'err_cb': function('s:on_error_vim8'),
    \ 'exit_cb': function('s:on_exit_vim8')
  \ })
  
  if job_status(job) !=# 'run'
    call s:on_error('Failed to start Lantae process')
  endif
endfunction

function! s:send_sync(message)
  let cmd = 'lantae --provider ' . g:lantae_provider . ' --model ' . g:lantae_model . ' "' . escape(a:message, '"') . '"'
  
  try
    let response = system(cmd)
    call s:hide_typing_indicator()
    
    if v:shell_error == 0
      call s:add_message('assistant', response)
    else
      call s:on_error('Lantae command failed: ' . response)
    endif
  catch
    call s:on_error('Error executing Lantae: ' . v:exception)
  endtry
endfunction

" Async callback functions for Neovim
function! s:on_response_nvim(job_id, data, event)
  if !empty(a:data) && a:data[0] !=# ''
    call s:hide_typing_indicator()
    let response = join(a:data, '\n')
    call s:add_message('assistant', response)
  endif
endfunction

function! s:on_error_nvim(job_id, data, event)
  if !empty(a:data) && a:data[0] !=# ''
    let error = join(a:data, '\n')
    call s:on_error(error)
  endif
endfunction

function! s:on_exit_nvim(job_id, exit_code, event)
  if a:exit_code != 0
    call s:on_error('Lantae process exited with code ' . a:exit_code)
  endif
endfunction

" Async callback functions for Vim 8
function! s:on_response_vim8(channel, msg)
  call s:hide_typing_indicator()
  call s:add_message('assistant', a:msg)
endfunction

function! s:on_error_vim8(channel, msg)
  call s:on_error(a:msg)
endfunction

function! s:on_exit_vim8(job, exit_code)
  if a:exit_code != 0
    call s:on_error('Lantae process exited with code ' . a:exit_code)
  endif
endfunction

function! s:on_error(error_msg)
  call s:hide_typing_indicator()
  call s:add_message('system', 'Error: ' . a:error_msg)
  echohl ErrorMsg
  echo 'Lantae Error: ' . a:error_msg
  echohl None
endfunction

function! s:send_current_line()
  let line = getline('.')
  if !empty(line) && line !~# '^=\|^Provider:\|^Commands:\|^\s*$'
    call lantae#chat#send(line)
  endif
endfunction

function! s:start_input_mode()
  call s:append_to_chat_buffer('> ')
  startinsert!
endfunction

function! s:send_input_line()
  let line = getline('.')
  if line =~# '^> '
    let message = substitute(line, '^> ', '', '')
    if !empty(message)
      " Replace the input line with the formatted message
      setlocal modifiable
      call setline('.', strftime('[%H:%M:%S]') . ' You: ' . message)
      setlocal nomodifiable
      
      " Send the message
      call lantae#chat#send(message)
    endif
  endif
endfunction

function! s:refresh_chat_display()
  let chat_winnr = bufwinnr('__Lantae_Chat__')
  if chat_winnr == -1
    return
  endif
  
  let current_winnr = winnr()
  execute chat_winnr . 'wincmd w'
  
  call s:setup_chat_buffer()
  
  " Redisplay all messages
  for msg in g:lantae_chat_history
    let role_name = msg.role ==# 'user' ? 'You' : 
                  \ msg.role ==# 'assistant' ? 'Lantae' : 'System'
    call s:append_to_chat_buffer(msg.timestamp . ' ' . role_name . ': ' . msg.content)
  endfor
  
  execute current_winnr . 'wincmd w'
endfunction