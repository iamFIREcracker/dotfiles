function! TurnOnJavaFolding() abort "{{{
    let modifier     = '%(public|private|protected)?\s*'
    let static       = '%(static\s*)?\s*'
    let returntype   = '%(,\s|\S)+\s*'
    let class        = modifier.'class%(\s+\S+)*\s*\{'
    " XXX it doesn't look like the following ignores if/for/while/switch statements correctly...
    let method       = modifier.static.returntype.'%(\S*\.\S*|if|for|while|switch)@![a-zA-Z0-9]+\s*\([^)]*\)\s*\{'
    let functionwrap = '\s*\S*\s[a-zA-Z]*\)\s*\{'

    let folded_statements = [
                \ class,
                \ method,
                \ functionwrap
                \ ]

    let b:manual_regexp_folding_statements_re_bare = '\v^\s*%(' . join(folded_statements, '|') . ')\s*$'
    call TurnOnManualRegexpFolding()
endfunction "}}}
silent! call TurnOnJavaFolding()
silent! call RefreshManualRegexpFolding()

setlocal omnifunc=javacomplete#Complete
inoremap <buffer> <c-n> <c-x><c-o>

let b:rbpt_max=2
RainbowParenthesesActivate
RainbowParenthesesLoadRound
RainbowParenthesesLoadSquare
RainbowParenthesesLoadBrace

" Abbreviations {{{

call MakeSpacelessBufferIabbrev('if',      'if (HERE)')
call MakeSpacelessBufferIabbrev('rt',      'return HERE;')
call MakeSpacelessBufferIabbrev('for',     'for (HERE) {}<left><cr>')
call MakeSpacelessBufferIabbrev('while',   'while (HERE) {}<left><cr>')
call MakeSpacelessBufferIabbrev('println', 'System.out.println(HERE);')

" }}}

