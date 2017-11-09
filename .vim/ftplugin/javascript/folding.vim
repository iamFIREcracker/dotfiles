setlocal foldmethod=expr
setlocal foldexpr=GetJavascriptFold(v:lnum)

let s:export    = '(module\.)?export.*(\(|\{)'
let s:class     = 'class\s+\S+\s*\{'
let s:method    = '(if|for|switch)@!\S+\s*\([^)]*\)\s*\{'
let s:function  = 'function(\s+\S+)?\s*\([^)]*\)\s*\{'
let s:router    = 'router.\S+\([^}]*\{'

let s:folded_statements = [
            \ s:export,
            \ s:class,
            \ s:method,
            \ s:function,
            \ s:router
            \ ]

let s:statements_re_bare = '\v^\s*(' . join(s:folded_statements, '|') . ')\s*$'
let s:blankline_re       = '\v^\s*$'

function! GetJavascriptFold(lnum)
    if getline(a:lnum) =~? s:blankline_re
        " the level of this line is 'undefined'. Vim will interpret this as
        " 'the foldlevel of this line is equal to the foldlevel of the line
        " above or below it, whichever is smaller'.
        return '-1'
    endif

    let this_level = IndentLevel(PrevDefinition(a:lnum))
    let next_level = IndentLevel(PrevDefinition(NextNonBlankLine(a:lnum)))

    if next_level == this_level
        return this_level
    elseif next_level < this_level
        return this_level
    elseif next_level > this_level
        return '>' . next_level
    endif
endfunction

function! PrevDefinition(lnum)
    if getline(a:lnum) =~? '\v^\s*}*\)*;?\s*$'
        return PrevDefinition(PrevNonBlankLine(a:lnum))
    endif

    let lindent = IndentLevel(a:lnum)
    let current = a:lnum

    while current > 0
        let currentindent = IndentLevel(current)
        if currentindent < lindent
            let line = getline(current)
            if line =~? s:statements_re_bare
                return current
            endif
        endif

        let current -= 1
    endwhile

    return -2
endfunction

function! PrevNonBlankLine(lnum)
    let current = a:lnum - 1

    while current > 0
        if getline(current) =~? '\v\S'
            return current
        endif

        let current -= 1
    endwhile

    return -2
endfunction

function! NextNonBlankLine(lnum)
    let numlines = line('$')
    let current = a:lnum + 1

    while current <= numlines
        if getline(current) =~? '\v\S'
            return current
        endif

        let current += 1
    endwhile

    return -2
endfunction

function! IndentLevel(lnum)
    if a:lnum == -2
        return 0
    endif

    return indent(a:lnum) / &shiftwidth + 1
endfunction
