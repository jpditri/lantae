" Vim syntax file
" Language: Lantae Prompt
" Maintainer: Lantae Team
" Latest Revision: 2024

if exists('b:current_syntax')
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

" Directive syntax
syntax match lantaeDirective /^@\w\+/ contained
syntax match lantaeDirectiveStart /^@\(provider\|model\|temperature\|system\|user\|assistant\|context\|format\|style\|tone\|language\|output\)\>/ nextgroup=lantaeDirectiveValue skipwhite
syntax match lantaeDirectiveValue /.*$/ contained

" Variable substitution
syntax match lantaeVariable /\${\w\+}/
syntax match lantaeVariable /\$\w\+/

" Code blocks
syntax region lantaeCodeBlock start=/```/ end=/```/ contains=@NoSpell
syntax match lantaeCodeBlockDelimiter /```\w*/ contained containedin=lantaeCodeBlock

" Inline code
syntax region lantaeInlineCode start=/`/ end=/`/ oneline

" Comments
syntax match lantaeComment /\/\/.*$/
syntax region lantaeBlockComment start=/\/\*/ end=/\*\//

" Emphasis
syntax region lantaeBold start=/\*\*/ end=/\*\*/ oneline
syntax region lantaeItalic start=/\*/ end=/\*/ oneline skip=/\*\*/ contains=NONE
syntax region lantaeUnderline start=/__/ end=/__/ oneline

" Links and URLs
syntax match lantaeURL /https\?:\/\/[^\s]\+/
syntax region lantaeLink start=/\[/ end=/\]/ nextgroup=lantaeLinkURL skipwhite
syntax region lantaeLinkURL start=/(/ end=/)/ contained

" Strings (for directive values)
syntax region lantaeString start=/"/ end=/"/ contained containedin=lantaeDirectiveValue
syntax region lantaeString start=/'/ end=/'/ contained containedin=lantaeDirectiveValue

" Numbers
syntax match lantaeNumber /\<\d\+\.\?\d*\>/ contained containedin=lantaeDirectiveValue

" Special keywords
syntax keyword lantaeBoolean true false nil null contained containedin=lantaeDirectiveValue
syntax keyword lantaeProvider ollama openai anthropic bedrock gemini mistral contained containedin=lantaeDirectiveValue

" Regions for directive blocks
syntax region lantaeSystemBlock start=/^@system/ end=/^@\|^$\|%$/ contains=lantaeDirectiveStart,@Spell
syntax region lantaeUserBlock start=/^@user/ end=/^@\|^$\|%$/ contains=lantaeDirectiveStart,@Spell
syntax region lantaeAssistantBlock start=/^@assistant/ end=/^@\|^$\|%$/ contains=lantaeDirectiveStart,@Spell
syntax region lantaeContextBlock start=/^@context/ end=/^@\|^$\|%$/ contains=lantaeDirectiveStart,@Spell

" Error highlighting for invalid syntax
syntax match lantaeError /^@\w\+.*\n\s*@\w\+/ "Two directives on consecutive lines

" Define highlighting
highlight default link lantaeDirectiveStart Keyword
highlight default link lantaeDirectiveValue String
highlight default link lantaeVariable Identifier
highlight default link lantaeCodeBlock PreProc
highlight default link lantaeCodeBlockDelimiter Special
highlight default link lantaeInlineCode Constant
highlight default link lantaeComment Comment
highlight default link lantaeBlockComment Comment
highlight default link lantaeBold Bold
highlight default link lantaeItalic Italic
highlight default link lantaeUnderline Underlined
highlight default link lantaeURL Underlined
highlight default link lantaeLink Label
highlight default link lantaeLinkURL Underlined
highlight default link lantaeString String
highlight default link lantaeNumber Number
highlight default link lantaeBoolean Boolean
highlight default link lantaeProvider Type
highlight default link lantaeError Error

" Special highlighting for directive blocks
highlight default link lantaeSystemBlock Function
highlight default link lantaeUserBlock Identifier
highlight default link lantaeAssistantBlock Type
highlight default link lantaeContextBlock Comment

" Set up spell checking regions
syntax cluster lantaeSpell contains=lantaeSystemBlock,lantaeUserBlock,lantaeAssistantBlock,lantaeContextBlock

let b:current_syntax = 'lantae'

let &cpo = s:save_cpo
unlet s:save_cpo