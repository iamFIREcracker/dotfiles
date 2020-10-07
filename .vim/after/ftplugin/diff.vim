setlocal nolist

" Diffs are folded by changes, so let's use [c]c to move to the
" next/previous fold (ie. change)
nnoremap <buffer> [c [z
nnoremap <buffer> ]c ]z
" Also, make it possible to move between folds/changes using arrow keys (as if
" they were results from a location list)
nnoremap <buffer> <down> ]z
nnoremap <buffer> <up> [z

" Folding {{{

setlocal foldmethod=expr foldexpr=DiffFold(v:lnum)

function! DiffFold(lnum) abort
    let line = getline(a:lnum)
    if line =~ '^diff '
        return '>1'
    elseif line =~ '^\(index\|---\|+++\) '
        return '='
    elseif line =~ '^\(@@\) '
        return '>2'
    elseif line[0] =~ '[-+ ]'
        return '='
    else
        return 0
    endif
endfunction

" }}}
