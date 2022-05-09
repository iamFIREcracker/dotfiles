function! PrettierIndentCalcIndentLvl(lnum) abort " {{{
    " Get the content of the current buffer
    "
    " Note: `prettier` would not try to indent _empty_ lines (i.e. lines
    " containing whitespace only), so if the current line happens to be
    " _empty_, we replace it with something else (i.e. something that hopefully
    " would not break the syntax)
    let l:buff_lines = getline(1, '$')
    if l:buff_lines[a:lnum - 1] !~ '\v\S'
        " XXX use &commentstring
        let l:buff_lines[a:lnum - 1] = '// prettier-indent-was-here'
    endif
    let l:buff = join(l:buff_lines, "\n")

    " Send the current buffer to `prettier` and extract the line
    " which we are trying to calculate the indentation level for
    let l:cmd = 'prettier --stdin-filepath ' . expand('%')
    let l:indented_line = systemlist(l:cmd, buff)[a:lnum - 1]
    echom l:indented_line

    " Return the position of the first non-whitespace character
    let l:level = 0
    while l:indented_line[l:level] == ' '
        let l:level += 1
    endwhile
    return l:level
endfunction " }}}

function! PrettierIndentInit() "{{{
    setlocal indentexpr=PrettierIndentCalcIndentLvl(v:lnum)
endfunction "}}}
