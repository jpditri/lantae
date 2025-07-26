" Lantae Code Analysis Module
" Handles code explanation, optimization, test generation, etc.

function! lantae#code#explain() range
  let code = s:get_selected_code(a:firstline, a:lastline)
  if empty(code)
    echo "No code selected"
    return
  endif
  
  let filetype = &filetype
  let prompt = printf("Explain this %s code:\n\n```%s\n%s\n```", filetype, filetype, code)
  
  call s:send_code_request(prompt, 'Code Explanation')
endfunction

function! lantae#code#optimize() range
  let code = s:get_selected_code(a:firstline, a:lastline)
  if empty(code)
    echo "No code selected"
    return
  endif
  
  let filetype = &filetype
  let prompt = printf("Optimize this %s code and explain the improvements:\n\n```%s\n%s\n```", filetype, filetype, code)
  
  call s:send_code_request(prompt, 'Code Optimization')
endfunction

function! lantae#code#generate_tests() range
  let code = s:get_selected_code(a:firstline, a:lastline)
  if empty(code)
    echo "No code selected"
    return
  endif
  
  let filetype = &filetype
  let prompt = printf("Generate comprehensive unit tests for this %s code:\n\n```%s\n%s\n```\n\nInclude edge cases and error conditions.", filetype, filetype, code)
  
  call s:send_code_request_with_result(prompt, 'Generated Tests', s:get_test_extension(filetype))
endfunction

function! lantae#code#debug() range
  let code = s:get_selected_code(a:firstline, a:lastline)
  if empty(code)
    echo "No code selected"
    return
  endif
  
  let filetype = &filetype
  let error_context = input("Describe the error or issue (optional): ")
  
  let prompt = printf("Help debug this %s code", filetype)
  if !empty(error_context)
    let prompt .= ".\nError/Issue: " . error_context
  endif
  let prompt .= "\n\n```" . filetype . "\n" . code . "\n```"
  
  call s:send_code_request(prompt, 'Debug Analysis')
endfunction

function! lantae#code#document() range
  let code = s:get_selected_code(a:firstline, a:lastline)
  if empty(code)
    echo "No code selected"
    return
  endif
  
  let filetype = &filetype
  let prompt = printf("Generate comprehensive documentation for this %s code:\n\n```%s\n%s\n```\n\nInclude function descriptions, parameters, return values, and usage examples.", filetype, filetype, code)
  
  call s:send_code_request_with_result(prompt, 'Documentation', 'md')
endfunction

function! lantae#code#refactor() range
  let code = s:get_selected_code(a:firstline, a:lastline)
  if empty(code)
    echo "No code selected"
    return
  endif
  
  let filetype = &filetype
  let style = input("Refactoring style (clean/functional/oop/minimal): ", "clean")
  
  let prompt = printf("Refactor this %s code following %s coding principles:\n\n```%s\n%s\n```", filetype, style, filetype, code)
  
  call s:send_code_request_with_result(prompt, 'Refactored Code', filetype)
endfunction

function! lantae#code#convert() range
  let code = s:get_selected_code(a:firstline, a:lastline)
  if empty(code)
    echo "No code selected"
    return
  endif
  
  let from_lang = &filetype
  let to_lang = input("Convert to language: ")
  if empty(to_lang)
    return
  endif
  
  let prompt = printf("Convert this %s code to %s:\n\n```%s\n%s\n```\n\nMaintain the same functionality and add comments explaining the conversion.", from_lang, to_lang, from_lang, code)
  
  call s:send_code_request_with_result(prompt, 'Converted Code', to_lang)
endfunction

function! lantae#code#security_review() range
  let code = s:get_selected_code(a:firstline, a:lastline)
  if empty(code)
    echo "No code selected"
    return
  endif
  
  let filetype = &filetype
  let prompt = printf("Perform a security review of this %s code:\n\n```%s\n%s\n```\n\nIdentify potential vulnerabilities, security issues, and suggest fixes.", filetype, filetype, code)
  
  call s:send_code_request(prompt, 'Security Review')
endfunction

function! lantae#code#performance_analysis() range
  let code = s:get_selected_code(a:firstline, a:lastline)
  if empty(code)
    echo "No code selected"
    return
  endif
  
  let filetype = &filetype
  let prompt = printf("Analyze the performance of this %s code:\n\n```%s\n%s\n```\n\nIdentify bottlenecks, suggest optimizations, and estimate time/space complexity.", filetype, filetype, code)
  
  call s:send_code_request(prompt, 'Performance Analysis')
endfunction

" Private helper functions
function! s:get_selected_code(firstline, lastline)
  if a:firstline == a:lastline && col('.') == 1
    " No selection, try to get current function/block
    return s:get_current_function()
  else
    " Get selected lines
    return join(getline(a:firstline, a:lastline), "\n")
  endif
endfunction

function! s:get_current_function()
  " Try to intelligently detect current function/method/class
  let current_line = line('.')
  let filetype = &filetype
  
  " Save current position
  let save_pos = getpos('.')
  
  try
    if filetype ==# 'python'
      " Find Python function/class
      call search('^def\|^class', 'bW')
      let start_line = line('.')
      normal! ][
      let end_line = line('.') - 1
    elseif filetype ==# 'javascript' || filetype ==# 'typescript'
      " Find JavaScript/TypeScript function
      call search('function\|=>\|class', 'bW')
      let start_line = line('.')
      normal! ][
      let end_line = line('.') - 1
    elseif filetype ==# 'ruby'
      " Find Ruby method/class
      call search('^def\|^class', 'bW')
      let start_line = line('.')
      call search('^end', 'W')
      let end_line = line('.')
    elseif filetype ==# 'vim'
      " Find Vim function
      call search('^function', 'bW')
      let start_line = line('.')
      call search('^endfunction', 'W')
      let end_line = line('.')
    else
      " Generic approach: find block boundaries
      call search('^[a-zA-Z]', 'bW')
      let start_line = line('.')
      call search('^}', 'W')
      let end_line = line('.')
    endif
    
    if start_line < end_line
      return join(getline(start_line, end_line), "\n")
    endif
  catch
    " Fallback: get current line
  finally
    call setpos('.', save_pos)
  endtry
  
  return getline('.')
endfunction

function! s:send_code_request(prompt, title)
  " Send request and show result in chat or new buffer
  if exists('g:lantae_use_chat_for_code') && g:lantae_use_chat_for_code
    call lantae#chat#send(a:prompt)
  else
    call s:show_in_new_buffer(a:prompt, a:title, 'markdown')
  endif
endfunction

function! s:send_code_request_with_result(prompt, title, filetype)
  " Send request and show result in new buffer with specific filetype
  call s:show_in_new_buffer_async(a:prompt, a:title, a:filetype)
endfunction

function! s:show_in_new_buffer(prompt, title, filetype)
  " Create new buffer and send request synchronously
  let result = s:call_lantae_sync(a:prompt)
  call s:create_result_buffer(a:title, result, a:filetype)
endfunction

function! s:show_in_new_buffer_async(prompt, title, filetype)
  " Create new buffer and send request asynchronously
  let buffer_info = {
    \ 'title': a:title,
    \ 'filetype': a:filetype,
    \ 'buffer_nr': -1
  \ }
  
  " Create buffer first
  call s:create_result_buffer(a:title, "Loading...", a:filetype)
  let buffer_info.buffer_nr = bufnr('%')
  
  " Send async request
  if has('nvim')
    call s:send_async_to_buffer_nvim(a:prompt, buffer_info)
  elseif has('job')
    call s:send_async_to_buffer_vim8(a:prompt, buffer_info)
  else
    " Fallback to sync
    let result = s:call_lantae_sync(a:prompt)
    call s:update_result_buffer(buffer_info.buffer_nr, result)
  endif
endfunction

function! s:create_result_buffer(title, content, filetype)
  " Create new buffer for results
  execute 'new'
  execute 'file Lantae:\ ' . a:title
  
  setlocal buftype=nofile
  setlocal noswapfile
  setlocal bufhidden=wipe
  
  " Set content
  put =a:content
  1delete _
  
  " Set filetype
  execute 'setlocal filetype=' . a:filetype
  
  " Make read-only
  setlocal readonly
  setlocal nomodifiable
  
  " Add helpful mappings
  nnoremap <buffer> q :bwipeout<CR>
  nnoremap <buffer> r :call <SID>refresh_buffer()<CR>
endfunction

function! s:update_result_buffer(buffer_nr, content)
  " Update existing buffer with new content
  let current_buf = bufnr('%')
  
  if bufexists(a:buffer_nr)
    execute 'buffer ' . a:buffer_nr
    setlocal modifiable
    silent %delete _
    put =a:content
    1delete _
    setlocal nomodifiable
    
    " Return to previous buffer if different
    if current_buf != a:buffer_nr && bufexists(current_buf)
      execute 'buffer ' . current_buf
    endif
  endif
endfunction

function! s:call_lantae_sync(prompt)
  let cmd = 'lantae --provider ' . shellescape(g:lantae_provider) . 
          \ ' --model ' . shellescape(g:lantae_model) . 
          \ ' --temperature ' . g:lantae_temperature . 
          \ ' ' . shellescape(a:prompt)
  
  try
    let result = system(cmd)
    if v:shell_error != 0
      return 'Error: ' . result
    endif
    return result
  catch
    return 'Error: Failed to execute Lantae command - ' . v:exception
  endtry
endfunction

function! s:send_async_to_buffer_nvim(prompt, buffer_info)
  let cmd = ['lantae', '--provider', g:lantae_provider, '--model', g:lantae_model, 
           \ '--temperature', string(g:lantae_temperature), a:prompt]
  
  let job_id = jobstart(cmd, {
    \ 'on_stdout': {job, data, event -> s:on_buffer_response_nvim(job, data, event, a:buffer_info)},
    \ 'on_stderr': {job, data, event -> s:on_buffer_error_nvim(job, data, event, a:buffer_info)},
    \ 'on_exit': {job, code, event -> s:on_buffer_exit_nvim(job, code, event, a:buffer_info)}
  \ })
endfunction

function! s:send_async_to_buffer_vim8(prompt, buffer_info)
  let cmd = 'lantae --provider ' . shellescape(g:lantae_provider) . 
          \ ' --model ' . shellescape(g:lantae_model) . 
          \ ' --temperature ' . g:lantae_temperature . 
          \ ' ' . shellescape(a:prompt)
  
  let job = job_start(cmd, {
    \ 'callback': {channel, msg -> s:on_buffer_response_vim8(channel, msg, a:buffer_info)},
    \ 'err_cb': {channel, msg -> s:on_buffer_error_vim8(channel, msg, a:buffer_info)},
    \ 'exit_cb': {job, exit_code -> s:on_buffer_exit_vim8(job, exit_code, a:buffer_info)}
  \ })
endfunction

" Async callback functions for buffer updates
function! s:on_buffer_response_nvim(job, data, event, buffer_info)
  if !empty(a:data) && a:data[0] !=# ''
    let response = join(a:data, "\n")
    call s:update_result_buffer(a:buffer_info.buffer_nr, response)
  endif
endfunction

function! s:on_buffer_error_nvim(job, data, event, buffer_info)
  if !empty(a:data) && a:data[0] !=# ''
    let error = 'Error: ' . join(a:data, "\n")
    call s:update_result_buffer(a:buffer_info.buffer_nr, error)
  endif
endfunction

function! s:on_buffer_exit_nvim(job, exit_code, event, buffer_info)
  " Handle completion
endfunction

function! s:on_buffer_response_vim8(channel, msg, buffer_info)
  call s:update_result_buffer(a:buffer_info.buffer_nr, a:msg)
endfunction

function! s:on_buffer_error_vim8(channel, msg, buffer_info)
  let error = 'Error: ' . a:msg
  call s:update_result_buffer(a:buffer_info.buffer_nr, error)
endfunction

function! s:on_buffer_exit_vim8(job, exit_code, buffer_info)
  " Handle completion
endfunction

function! s:get_test_extension(filetype)
  " Return appropriate test file extension
  let extensions = {
    \ 'python': 'py',
    \ 'javascript': 'test.js',
    \ 'typescript': 'test.ts',
    \ 'ruby': 'rb',
    \ 'go': 'go',
    \ 'rust': 'rs',
    \ 'java': 'java',
    \ 'cpp': 'cpp',
    \ 'c': 'c',
    \ 'vim': 'vim'
  \ }
  
  return get(extensions, a:filetype, 'txt')
endfunction

function! s:refresh_buffer()
  " Refresh current result buffer
  echo "Buffer refresh not implemented yet"
endfunction