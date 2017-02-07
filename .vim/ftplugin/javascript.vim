setlocal foldmethod=expr
setlocal foldexpr=GetJavascriptFold(v:lnum)

function! GetJavascriptFold(lnum)
    if getline(a:lnum) =~? '\v^\s*$'
        return '-1'
    endif

    let this_indent = IndentLevel(a:lnum)
    let next_indent = IndentLevel(NextNonBlankLine(a:lnum))

    if next_indent == this_indent
        return this_indent
    elseif next_indent < this_indent
        return this_indent
    elseif next_indent > this_indent
        return '>' . next_indent
endfunction

function! IndentLevel(lnum)
    let indent = indent(a:lnum) / &shiftwidth
    " Closing function, method, call, definition -- }, ], or )
    if getline(a:lnum) =~? '\v^\s*(}|\]|\))'
        return indent + 1
    endif

    " Closing JSX
    if getline(a:lnum) =~? '\v^\s*\<\/.*\>'
        return indent + 1
    endif

    return indent
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
