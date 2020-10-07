function! DadbodInit() abort
    let line = getline(".")

    if line =~ ';DB '
        execute line[1:]
        echo 'Loaded: ' . line[1:]
    else
        echohl ErrorMsg
        echo 'Line not starting with `;DB `: ' . line
        echohl NONE
    endif
endfunction

nnoremap <buffer> <silent> <localleader>cc :call DadbodInit()<CR>
nnoremap <buffer> <silent> <C-J> vap:DB w:db<CR>
xnoremap <buffer> <silent> <C-J> :DB w:db<CR>
