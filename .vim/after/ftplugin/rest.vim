setlocal shiftwidth=2 softtabstop=2 expandtab

setlocal foldmethod=expr foldexpr=GetRestFold(v:lnum)

function! GetRestFold(lnum) abort
    if getline(a:lnum) =~? '\v^(GET|POST|PUT|DELETE)\s'
        return 1
    endif
    if getline(a:lnum) =~? '\v^--\s*'
        return 0
    endif

    return '='
endfunction
