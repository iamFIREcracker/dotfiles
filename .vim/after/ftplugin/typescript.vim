" function! TurnOnTypescriptFolding() abort "{{{
"     let export       = '%(module\.)?export(s)?%(\.)?.*\{'
"     let class        = 'class%(\s+\S+)*\s*\{'
"     let method       = '%(async )?%(private )?%(%(get|set) )?%(\S*\.\S*|if|for|switch)@!\S+\s*\([^)]*\)\s*\{'
"     let functionwrap = '\s*[a-zA-Z0-9:]*\S*\)\s*\{'
"     let functiondec  = '%(async )?function%(\s+\S+)?\s*\([^)]*' . functionwrap
"     let functiondef  = '%(%(const|var|let)\s)?\S+\s*\=\s*' . functiondec
"     let arrowdefwrap = '\s*[a-zA-Z0-9:]*\)\s*\=\>\s*\{'
"     let arrowdef     = '%(%(const|var|let)\s)?\S+\s*\=\s*\([^)]*' . arrowdefwrap

"     let folded_statements = [
"                 \ export,
"                 \ class,
"                 \ method,
"                 \ functionwrap,
"                 \ functiondec,
"                 \ functiondef,
"                 \ arrowdefwrap,
"                 \ arrowdef
"                 \ ]

"     let b:manual_regexp_folding_statements_re_bare = '\v^\s*%(' . join(folded_statements, '|') . ')\s*$'
"     call TurnOnManualRegexpFolding()
" endfunction "}}}
" silent! call TurnOnTypescriptFolding()
" silent! call RefreshManualRegexpFolding()

setlocal suffixesadd+=.ts,.js

" XXX temporarily moved inside syntax/
" RainbowParenthesesActivate
" RainbowParenthesesLoadRound
" RainbowParenthesesLoadSquare
" RainbowParenthesesLoadBrace

call SetupLspBindings()
inoremap <buffer> <c-n> <c-x><c-o>

setl formatexpr=NeoformatExpr()
nnoremap <buffer> <C-Q> :Neoformat<CR>
call PrettierIndentInit()

" Soft-wrap: automatic indentation of 3 characters (it's 2 there, but then there
" is an additional one used by `:h showbreak`
setl wrap
setl breakindent
setl breakindentopt+=shift:2

" Abbreviations {{{

call MakeSpacelessBufferIabbrev('if',      'if (HERE)')
call MakeSpacelessBufferIabbrev('rt',      'return HERE;')
call MakeSpacelessBufferIabbrev('for',     'for (HERE) {}<left><cr>')
call MakeSpacelessBufferIabbrev('while',   'while (HERE) {}<left><cr>')
call MakeSpacelessBufferIabbrev('clog', 'console.log();<left><left>')
call MakeSpacelessBufferIabbrev('cerr', 'console.error(HERE);')

" }}}
