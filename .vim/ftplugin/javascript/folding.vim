setlocal foldmethod=expr
setlocal foldexpr=GetJavascriptFold(v:lnum)

nnoremap <localleader>F :echo IndentLevel(PrevDefinition(line('.')))<cr>


function! GetJavascriptFold(lnum)
    if getline(a:lnum) =~? '\v^\s*$'
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
            if line =~? '\v^\s*\S*=\S*\('
                \ || line =~? '\v^\s*\S*\(\S*\)'
                \ || line =~? '\vclass \S*'
                \ || line =~? '\vfunction ?\S*'
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
