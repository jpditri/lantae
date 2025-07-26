" Lantae Configuration Module
" Handles provider/model management and configuration

function! lantae#config#set_provider(provider)
  let valid_providers = ['ollama', 'openai', 'anthropic', 'bedrock', 'gemini', 'mistral']
  
  if index(valid_providers, a:provider) == -1
    echohl ErrorMsg
    echo 'Invalid provider. Valid options: ' . join(valid_providers, ', ')
    echohl None
    return
  endif
  
  let g:lantae_provider = a:provider
  let g:lantae_status.provider = a:provider
  
  " Test connection with new provider
  call s:test_provider_connection()
  
  echo 'Provider set to: ' . a:provider
endfunction

function! lantae#config#set_model(model)
  let g:lantae_model = a:model
  let g:lantae_status.model = a:model
  
  echo 'Model set to: ' . a:model
endfunction

function! lantae#config#list_providers()
  echo 'Available providers:'
  echo '  ollama      - Local models via Ollama'
  echo '  openai      - OpenAI GPT models'
  echo '  anthropic   - Anthropic Claude models'
  echo '  bedrock     - Amazon Bedrock models'
  echo '  gemini      - Google Gemini models'
  echo '  mistral     - Mistral AI models'
  echo ''
  echo 'Current: ' . g:lantae_provider
  echo ''
  echo 'Use :LantaeProvider <name> to switch'
endfunction

function! lantae#config#list_models()
  echo 'Fetching available models for ' . g:lantae_provider . '...'
  
  " Get models asynchronously
  if has('nvim')
    call s:get_models_async_nvim()
  elseif has('job')
    call s:get_models_async_vim8()
  else
    call s:get_models_sync()
  endif
endfunction

function! lantae#config#get_provider_info(provider)
  let provider_info = {
    \ 'ollama': {
      \   'name': 'Ollama',
      \   'description': 'Local AI models',
      \   'url': 'http://localhost:11434',
      \   'requires_key': 0,
      \   'models': ['cogito:latest', 'llama2:latest', 'mistral:latest']
    \ },
    \ 'openai': {
      \   'name': 'OpenAI',
      \   'description': 'GPT models',
      \   'url': 'https://api.openai.com',
      \   'requires_key': 1,
      \   'env_var': 'OPENAI_API_KEY',
      \   'models': ['gpt-4', 'gpt-3.5-turbo', 'gpt-4-turbo']
    \ },
    \ 'anthropic': {
      \   'name': 'Anthropic',
      \   'description': 'Claude models',
      \   'url': 'https://api.anthropic.com',
      \   'requires_key': 1,
      \   'env_var': 'ANTHROPIC_API_KEY',
      \   'models': ['claude-3-opus', 'claude-3-sonnet', 'claude-3-haiku']
    \ },
    \ 'bedrock': {
      \   'name': 'Amazon Bedrock',
      \   'description': 'AWS hosted models',
      \   'url': 'https://bedrock.amazonaws.com',
      \   'requires_key': 1,
      \   'env_var': 'AWS_ACCESS_KEY_ID',
      \   'models': ['anthropic.claude-v2', 'amazon.titan-text-express-v1']
    \ },
    \ 'gemini': {
      \   'name': 'Google Gemini',
      \   'description': 'Google AI models',
      \   'url': 'https://generativelanguage.googleapis.com',
      \   'requires_key': 1,
      \   'env_var': 'GOOGLE_API_KEY',
      \   'models': ['gemini-pro', 'gemini-pro-vision']
    \ },
    \ 'mistral': {
      \   'name': 'Mistral AI',
      \   'description': 'Mistral models',
      \   'url': 'https://api.mistral.ai',
      \   'requires_key': 1,
      \   'env_var': 'MISTRAL_API_KEY',
      \   'models': ['mistral-large', 'mistral-medium', 'mistral-small']
    \ }
  \ }
  
  return get(provider_info, a:provider, {})
endfunction

function! lantae#config#setup_provider_wizard()
  echo 'Lantae Provider Setup Wizard'
  echo '============================'
  echo ''
  
  " Show current configuration
  echo 'Current configuration:'
  echo '  Provider: ' . g:lantae_provider
  echo '  Model: ' . g:lantae_model
  echo '  Temperature: ' . g:lantae_temperature
  echo ''
  
  " Ask if user wants to change provider
  let change = input('Change provider? (y/n): ')
  if change =~? '^y'
    call lantae#config#list_providers()
    let new_provider = input('Enter provider name: ')
    if !empty(new_provider)
      call lantae#config#set_provider(new_provider)
    endif
  endif
  
  " Ask about model
  let change_model = input('Change model? (y/n): ')
  if change_model =~? '^y'
    call lantae#config#list_models()
    let new_model = input('Enter model name: ')
    if !empty(new_model)
      call lantae#config#set_model(new_model)
    endif
  endif
  
  " Ask about temperature
  let change_temp = input('Change temperature? (y/n): ')
  if change_temp =~? '^y'
    let new_temp = input('Enter temperature (0.0-2.0): ', string(g:lantae_temperature))
    if !empty(new_temp) && str2float(new_temp) >= 0.0 && str2float(new_temp) <= 2.0
      let g:lantae_temperature = str2float(new_temp)
      echo 'Temperature set to: ' . g:lantae_temperature
    endif
  endif
  
  " Test configuration
  echo ''
  echo 'Testing configuration...'
  call s:test_provider_connection()
endfunction

function! lantae#config#save_config()
  " Save current configuration to vimrc or init.vim
  let config_lines = [
    \ '" Lantae configuration',
    \ 'let g:lantae_provider = "' . g:lantae_provider . '"',
    \ 'let g:lantae_model = "' . g:lantae_model . '"',
    \ 'let g:lantae_temperature = ' . g:lantae_temperature,
    \ ''
  \ ]
  
  " Determine config file location
  let config_file = has('nvim') ? stdpath('config') . '/init.vim' : $HOME . '/.vimrc'
  
  if filereadable(config_file)
    let choice = input('Append to ' . config_file . '? (y/n): ')
    if choice =~? '^y'
      call writefile(config_lines, config_file, 'a')
      echo 'Configuration saved to ' . config_file
    endif
  else
    echo 'Config file not found: ' . config_file
  endif
endfunction

" Private functions
function! s:test_provider_connection()
  echo 'Testing connection to ' . g:lantae_provider . '...'
  
  let test_cmd = 'lantae --provider ' . shellescape(g:lantae_provider) . ' --test-connection'
  
  if has('nvim')
    call jobstart(test_cmd, {
      \ 'on_exit': function('s:on_test_connection_exit_nvim')
    \ })
  elseif has('job')
    call job_start(test_cmd, {
      \ 'exit_cb': function('s:on_test_connection_exit_vim8')
    \ })
  else
    let result = system(test_cmd)
    call s:handle_test_result(v:shell_error == 0)
  endif
endfunction

function! s:handle_test_result(success)
  if a:success
    let g:lantae_status.connected = 1
    echohl MoreMsg
    echo '✓ Connected to ' . g:lantae_provider
    echohl None
  else
    let g:lantae_status.connected = 0
    echohl ErrorMsg
    echo '✗ Failed to connect to ' . g:lantae_provider
    echohl None
    
    " Show help for connection issues
    call s:show_connection_help()
  endif
endfunction

function! s:show_connection_help()
  let provider_info = lantae#config#get_provider_info(g:lantae_provider)
  
  if !empty(provider_info)
    echo ''
    echo 'Connection help for ' . provider_info.name . ':'
    
    if provider_info.requires_key
      echo '• Make sure ' . provider_info.env_var . ' environment variable is set'
      echo '• Check your API key is valid'
      echo '• Verify internet connectivity'
    elseif g:lantae_provider ==# 'ollama'
      echo '• Make sure Ollama is running: ollama serve'
      echo '• Check if Ollama is accessible at http://localhost:11434'
      echo '• Try: curl http://localhost:11434/api/tags'
    endif
  endif
endfunction

function! s:get_models_sync()
  let cmd = 'lantae --provider ' . shellescape(g:lantae_provider) . ' --list-models'
  
  try
    let result = system(cmd)
    if v:shell_error == 0
      echo 'Available models for ' . g:lantae_provider . ':'
      echo result
      echo ''
      echo 'Current: ' . g:lantae_model
      echo ''
      echo 'Use :LantaeModel <name> to switch'
    else
      echohl ErrorMsg
      echo 'Failed to get models: ' . result
      echohl None
    endif
  catch
    echohl ErrorMsg
    echo 'Error getting models: ' . v:exception
    echohl None
  endtry
endfunction

function! s:get_models_async_nvim()
  let cmd = ['lantae', '--provider', g:lantae_provider, '--list-models']
  
  call jobstart(cmd, {
    \ 'on_stdout': function('s:on_models_response_nvim'),
    \ 'on_stderr': function('s:on_models_error_nvim'),
    \ 'on_exit': function('s:on_models_exit_nvim')
  \ })
endfunction

function! s:get_models_async_vim8()
  let cmd = 'lantae --provider ' . shellescape(g:lantae_provider) . ' --list-models'
  
  call job_start(cmd, {
    \ 'callback': function('s:on_models_response_vim8'),
    \ 'err_cb': function('s:on_models_error_vim8'),
    \ 'exit_cb': function('s:on_models_exit_vim8')
  \ })
endfunction

" Async callback functions
function! s:on_test_connection_exit_nvim(job_id, exit_code, event)
  call s:handle_test_result(a:exit_code == 0)
endfunction

function! s:on_test_connection_exit_vim8(job, exit_code)
  call s:handle_test_result(a:exit_code == 0)
endfunction

function! s:on_models_response_nvim(job_id, data, event)
  if !empty(a:data) && a:data[0] !=# ''
    echo 'Available models for ' . g:lantae_provider . ':'
    for line in a:data
      if !empty(line)
        echo '  ' . line
      endif
    endfor
    echo ''
    echo 'Current: ' . g:lantae_model
    echo ''
    echo 'Use :LantaeModel <name> to switch'
  endif
endfunction

function! s:on_models_error_nvim(job_id, data, event)
  if !empty(a:data) && a:data[0] !=# ''
    echohl ErrorMsg
    echo 'Error getting models: ' . join(a:data, ' ')
    echohl None
  endif
endfunction

function! s:on_models_exit_nvim(job_id, exit_code, event)
  if a:exit_code != 0
    echohl ErrorMsg
    echo 'Failed to get models (exit code: ' . a:exit_code . ')'
    echohl None
  endif
endfunction

function! s:on_models_response_vim8(channel, msg)
  echo 'Available models for ' . g:lantae_provider . ':'
  for line in split(a:msg, '\n')
    if !empty(line)
      echo '  ' . line
    endif
  endfor
  echo ''
  echo 'Current: ' . g:lantae_model
  echo ''
  echo 'Use :LantaeModel <name> to switch'
endfunction

function! s:on_models_error_vim8(channel, msg)
  echohl ErrorMsg
  echo 'Error getting models: ' . a:msg
  echohl None
endfunction

function! s:on_models_exit_vim8(job, exit_code)
  if a:exit_code != 0
    echohl ErrorMsg
    echo 'Failed to get models (exit code: ' . a:exit_code . ')'
    echohl None
  endif
endfunction