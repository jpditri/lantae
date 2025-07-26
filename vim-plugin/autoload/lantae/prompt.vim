" Lantae Prompt Module
" Handles .lantae file processing and prompt execution

function! lantae#prompt#execute_current_buffer()
  let content = join(getline(1, '$'), "\n")
  let parsed = s:parse_prompt_file(content)
  
  if empty(parsed)
    echohl ErrorMsg
    echo 'No valid prompt content found'
    echohl None
    return
  endif
  
  " Apply any configuration directives
  call s:apply_config_directives(parsed.config)
  
  " Build the final prompt
  let prompt = s:build_prompt_from_parsed(parsed)
  
  " Execute the prompt
  call s:execute_prompt(prompt, parsed.config)
endfunction

function! lantae#prompt#validate_current_buffer()
  let content = join(getline(1, '$'), "\n")
  let errors = s:validate_prompt_content(content)
  
  if empty(errors)
    echo 'Prompt file is valid ✓'
  else
    echo 'Validation errors:'
    for error in errors
      echo '  Line ' . error.line . ': ' . error.message
    endfor
  endif
endfunction

function! lantae#prompt#preview_current_buffer()
  let content = join(getline(1, '$'), "\n")
  let parsed = s:parse_prompt_file(content)
  
  if empty(parsed)
    echo 'No content to preview'
    return
  endif
  
  " Create preview buffer
  execute 'new'
  execute 'file Lantae:\ Preview'
  
  setlocal buftype=nofile
  setlocal noswapfile
  setlocal bufhidden=wipe
  
  " Add preview content
  let preview_lines = []
  call add(preview_lines, '=== Lantae Prompt Preview ===')
  call add(preview_lines, '')
  
  " Show configuration
  if !empty(parsed.config)
    call add(preview_lines, 'Configuration:')
    for [key, value] in items(parsed.config)
      call add(preview_lines, '  ' . key . ': ' . string(value))
    endfor
    call add(preview_lines, '')
  endif
  
  " Show prompt structure
  call add(preview_lines, 'Prompt Structure:')
  for block in parsed.blocks
    call add(preview_lines, '  ' . block.type . ': ' . len(split(block.content, '\n')) . ' lines')
  endfor
  call add(preview_lines, '')
  
  " Show assembled prompt
  call add(preview_lines, 'Assembled Prompt:')
  call add(preview_lines, repeat('=', 50))
  let prompt = s:build_prompt_from_parsed(parsed)
  call extend(preview_lines, split(prompt, '\n'))
  
  call append(0, preview_lines)
  1delete _
  
  setlocal readonly
  setlocal nomodifiable
  setlocal filetype=markdown
  
  nnoremap <buffer> q :bwipeout<CR>
endfunction

function! lantae#prompt#export_current_buffer()
  let content = join(getline(1, '$'), "\n")
  let parsed = s:parse_prompt_file(content)
  
  if empty(parsed)
    echo 'No content to export'
    return
  endif
  
  let formats = ['json', 'yaml', 'markdown', 'plain']
  let format = input('Export format (' . join(formats, '/') . '): ', 'json')
  
  if index(formats, format) == -1
    echo 'Invalid format'
    return
  endif
  
  let exported = s:export_prompt(parsed, format)
  let filename = 'lantae_export.' . format
  
  " Create new buffer with exported content
  execute 'new'
  execute 'file ' . filename
  
  call append(0, split(exported, '\n'))
  1delete _
  
  execute 'setlocal filetype=' . format
  
  echo 'Exported to ' . filename
endfunction

function! lantae#prompt#complete_directive()
  let line = getline('.')
  let col = col('.') - 1
  
  if line[col-1] ==# '@'
    let directives = ['provider', 'model', 'temperature', 'system', 'user', 'assistant', 'context', 'format', 'style', 'tone', 'language', 'output']
    return join(directives, "\n")
  endif
  
  return ''
endfunction

function! lantae#prompt#wrap_selection() range
  let selection = s:get_visual_selection()
  let wrapper = input('Wrap with directive: @')
  
  if !empty(wrapper)
    call s:replace_visual_selection('@' . wrapper . "\n" . selection . "\n")
  endif
endfunction

function! lantae#prompt#omnifunc(findstart, base)
  if a:findstart
    " Find start of current word
    let line = getline('.')
    let start = col('.') - 1
    
    while start > 0 && line[start - 1] =~# '\w'
      let start -= 1
    endwhile
    
    return start
  else
    " Return list of matches
    let matches = []
    
    " Check if we're completing after @
    let line = getline('.')
    let col = col('.') - 1
    
    if col > 0 && line[col-1] ==# '@' || (col > 1 && line[col-2:col-1] ==# '@')
      let directives = [
        \ {'word': 'provider', 'menu': 'Set AI provider'},
        \ {'word': 'model', 'menu': 'Set AI model'},
        \ {'word': 'temperature', 'menu': 'Set response temperature'},
        \ {'word': 'system', 'menu': 'System message'},
        \ {'word': 'user', 'menu': 'User message'},
        \ {'word': 'assistant', 'menu': 'Assistant response'},
        \ {'word': 'context', 'menu': 'Additional context'},
        \ {'word': 'format', 'menu': 'Output format'},
        \ {'word': 'style', 'menu': 'Response style'},
        \ {'word': 'tone', 'menu': 'Response tone'},
        \ {'word': 'language', 'menu': 'Response language'},
        \ {'word': 'output', 'menu': 'Output directive'}
      \ ]
      
      for directive in directives
        if directive.word =~# '^' . a:base
          call add(matches, directive)
        endif
      endfor
    endif
    
    return matches
  endif
endfunction

function! lantae#prompt#update_folds()
  " Update folding when content changes
  if &foldmethod ==# 'expr'
    setlocal foldmethod=expr
  endif
endfunction

function! lantae#prompt#highlight_matching_directives()
  " Highlight matching directive pairs
  let line = getline('.')
  if line =~# '^@\w\+'
    " TODO: Implement directive matching highlighting
  endif
endfunction

" Private functions
function! s:parse_prompt_file(content)
  let lines = split(a:content, '\n')
  let parsed = {'config': {}, 'blocks': []}
  let current_block = {'type': 'content', 'content': ''}
  let i = 0
  
  while i < len(lines)
    let line = lines[i]
    
    if line =~# '^@\w\+'
      " Save previous block if it has content
      if !empty(trim(current_block.content))
        call add(parsed.blocks, current_block)
      endif
      
      " Parse directive
      let directive = substitute(line, '^@\(\w\+\)\s*', '\1', '')
      let value = substitute(line, '^@\w\+\s*', '', '')
      
      if directive =~# '^\(provider\|model\|temperature\|format\|style\|tone\|language\)$'
        " Configuration directive
        let parsed.config[directive] = s:parse_directive_value(value)
        let current_block = {'type': 'content', 'content': ''}
      else
        " Content directive
        let current_block = {'type': directive, 'content': ''}
      endif
    else
      " Add line to current block
      if !empty(current_block.content)
        let current_block.content .= "\n"
      endif
      let current_block.content .= line
    endif
    
    let i += 1
  endwhile
  
  " Add final block
  if !empty(trim(current_block.content))
    call add(parsed.blocks, current_block)
  endif
  
  return parsed
endfunction

function! s:parse_directive_value(value)
  let value = trim(a:value)
  
  " Try to parse as number
  if value =~# '^\d\+\.\?\d*$'
    return str2float(value)
  endif
  
  " Try to parse as boolean
  if value =~# '^\(true\|false\)$'
    return value ==# 'true'
  endif
  
  " Return as string
  return value
endfunction

function! s:validate_prompt_content(content)
  let errors = []
  let lines = split(a:content, '\n')
  let line_num = 0
  
  for line in lines
    let line_num += 1
    
    " Check for invalid directives
    if line =~# '^@\w\+'
      let directive = substitute(line, '^@\(\w\+\).*', '\1', '')
      let valid_directives = ['provider', 'model', 'temperature', 'system', 'user', 'assistant', 'context', 'format', 'style', 'tone', 'language', 'output']
      
      if index(valid_directives, directive) == -1
        call add(errors, {'line': line_num, 'message': 'Unknown directive: @' . directive})
      endif
      
      " Validate directive values
      if directive ==# 'temperature'
        let value = substitute(line, '^@temperature\s*', '', '')
        if !empty(value) && (str2float(value) < 0.0 || str2float(value) > 2.0)
          call add(errors, {'line': line_num, 'message': 'Temperature must be between 0.0 and 2.0'})
        endif
      endif
      
      if directive ==# 'provider'
        let value = substitute(line, '^@provider\s*', '', '')
        let valid_providers = ['ollama', 'openai', 'anthropic', 'bedrock', 'gemini', 'mistral']
        if !empty(value) && index(valid_providers, value) == -1
          call add(errors, {'line': line_num, 'message': 'Invalid provider: ' . value})
        endif
      endif
    endif
  endfor
  
  return errors
endfunction

function! s:apply_config_directives(config)
  " Apply configuration from directives
  for [key, value] in items(a:config)
    if key ==# 'provider'
      call lantae#config#set_provider(value)
    elseif key ==# 'model'
      call lantae#config#set_model(value)
    elseif key ==# 'temperature'
      let g:lantae_temperature = value
    endif
  endfor
endfunction

function! s:build_prompt_from_parsed(parsed)
  let prompt_parts = []
  
  for block in a:parsed.blocks
    if block.type ==# 'system'
      call add(prompt_parts, 'System: ' . block.content)
    elseif block.type ==# 'user'
      call add(prompt_parts, 'User: ' . block.content)
    elseif block.type ==# 'assistant'
      call add(prompt_parts, 'Assistant: ' . block.content)
    elseif block.type ==# 'context'
      call add(prompt_parts, 'Context: ' . block.content)
    else
      call add(prompt_parts, block.content)
    endif
  endfor
  
  return join(prompt_parts, "\n\n")
endfunction

function! s:execute_prompt(prompt, config)
  " Show the prompt being executed
  echo 'Executing prompt with ' . g:lantae_provider . '...'
  
  " Send to chat interface
  call lantae#chat#send(a:prompt)
endfunction

function! s:export_prompt(parsed, format)
  if a:format ==# 'json'
    return s:export_to_json(a:parsed)
  elseif a:format ==# 'yaml'
    return s:export_to_yaml(a:parsed)
  elseif a:format ==# 'markdown'
    return s:export_to_markdown(a:parsed)
  else
    return s:export_to_plain(a:parsed)
  endif
endfunction

function! s:export_to_json(parsed)
  let json_data = {
    \ 'config': a:parsed.config,
    \ 'blocks': a:parsed.blocks
  \ }
  
  return json_encode(json_data)
endfunction

function! s:export_to_yaml(parsed)
  let yaml_lines = ['config:']
  
  for [key, value] in items(a:parsed.config)
    call add(yaml_lines, '  ' . key . ': ' . string(value))
  endfor
  
  call add(yaml_lines, 'blocks:')
  for block in a:parsed.blocks
    call add(yaml_lines, '  - type: ' . block.type)
    call add(yaml_lines, '    content: |')
    for line in split(block.content, '\n')
      call add(yaml_lines, '      ' . line)
    endfor
  endfor
  
  return join(yaml_lines, "\n")
endfunction

function! s:export_to_markdown(parsed)
  let md_lines = ['# Lantae Prompt Export', '']
  
  if !empty(a:parsed.config)
    call add(md_lines, '## Configuration')
    call add(md_lines, '')
    for [key, value] in items(a:parsed.config)
      call add(md_lines, '- **' . key . '**: ' . string(value))
    endfor
    call add(md_lines, '')
  endif
  
  call add(md_lines, '## Content')
  call add(md_lines, '')
  
  for block in a:parsed.blocks
    call add(md_lines, '### ' . toupper(block.type))
    call add(md_lines, '')
    call add(md_lines, block.content)
    call add(md_lines, '')
  endfor
  
  return join(md_lines, "\n")
endfunction

function! s:export_to_plain(parsed)
  return s:build_prompt_from_parsed(a:parsed)
endfunction

function! s:get_visual_selection()
  let [line_start, column_start] = getpos("'<")[1:2]
  let [line_end, column_end] = getpos("'>")[1:2]
  let lines = getline(line_start, line_end)
  
  if len(lines) == 0
    return ''
  endif
  
  lines[-1] = lines[-1][:column_end - (&selection == 'inclusive' ? 1 : 2)]
  lines[0] = lines[0][column_start - 1:]
  
  return join(lines, "\n")
endfunction

function! s:replace_visual_selection(text)
  let [line_start, column_start] = getpos("'<")[1:2]
  let [line_end, column_end] = getpos("'>")[1:2]
  
  " Delete the selected text
  execute 'normal! gvd'
  
  " Insert the new text
  call append(line_start - 1, split(a:text, "\n"))
endfunction