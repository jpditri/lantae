" Lantae prompt file type plugin
" Provides special handling for .lantae files

if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

" Save user's cpoptions and set it to Vim default
let s:save_cpo = &cpo
set cpo&vim

" Buffer-local settings
setlocal commentstring=//%s
setlocal comments=://
setlocal formatoptions+=croql
setlocal textwidth=80
setlocal expandtab
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2

" Enable spell checking for prompt content
setlocal spell
setlocal spelllang=en

" Custom folding for sections
setlocal foldmethod=expr
setlocal foldexpr=LantaeFoldExpr(v:lnum)
setlocal foldtext=LantaeFoldText()

" Custom syntax highlighting
if exists('*lantae#syntax#setup')
  call lantae#syntax#setup()
endif

" Buffer-local commands
command! -buffer -nargs=0 LantaeExecute call lantae#prompt#execute_current_buffer()
command! -buffer -nargs=0 LantaeValidate call lantae#prompt#validate_current_buffer()
command! -buffer -nargs=0 LantaePreview call lantae#prompt#preview_current_buffer()
command! -buffer -nargs=0 LantaeExport call lantae#prompt#export_current_buffer()

" Buffer-local key mappings
nnoremap <buffer> <silent> <leader>le :LantaeExecute<CR>
nnoremap <buffer> <silent> <leader>lv :LantaeValidate<CR>
nnoremap <buffer> <silent> <leader>lp :LantaePreview<CR>
nnoremap <buffer> <silent> <leader>lx :LantaeExport<CR>

" Insert mode mappings for directive completion
inoremap <buffer> <silent> @<Tab> <C-R>=lantae#prompt#complete_directive()<CR>

" Visual mode mappings
vnoremap <buffer> <silent> <leader>lw :call lantae#prompt#wrap_selection()<CR>

" Auto-completion for directives
setlocal omnifunc=lantae#prompt#omnifunc

" Auto-commands for this buffer
augroup LantaeBuffer
  autocmd! * <buffer>
  
  " Auto-validate on save
  autocmd BufWritePre <buffer> call lantae#prompt#validate_current_buffer()
  
  " Update fold when content changes
  autocmd TextChanged <buffer> call lantae#prompt#update_folds()
  autocmd TextChangedI <buffer> call lantae#prompt#update_folds()
  
  " Highlight matching directives
  autocmd CursorMoved <buffer> call lantae#prompt#highlight_matching_directives()
  
augroup END

" Folding functions
function! LantaeFoldExpr(lnum)
  let line = getline(a:lnum)
  
  " Start fold at directive lines
  if line =~# '^@\w\+'
    return '>1'
  endif
  
  " Continue fold for content
  if line =~# '^\s*$' && getline(a:lnum + 1) =~# '^@\w\+'
    return '<1'
  endif
  
  return '='
endfunction

function! LantaeFoldText()
  let line = getline(v:foldstart)
  let directive = substitute(line, '^@\(\w\+\).*', '\1', '')
  let line_count = v:foldend - v:foldstart + 1
  
  return '@' . directive . ' (' . line_count . ' lines) '
endfunction

" Restore user's cpoptions
let &cpo = s:save_cpo
unlet s:save_cpo