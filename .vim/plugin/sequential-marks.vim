" Stolen from: https://vitalyparnas.com/guides/vim-sequential-marks/
function! NewMark()
    if (!exists('b:last_mark'))
        let list = getmarklist(bufname())
        call filter(list, {idx, val -> val['mark'] =~ '[a-z]'})
        if (len(list) == 0)
            let b:last_mark = 'z'
        else
            let b:last_mark = strpart(list[-1].mark, 1)
        endif
    endif
    if (b:last_mark == 'z')
        let b:last_mark = 'a'
    else
        let b:last_mark = nr2char(char2nr(b:last_mark) + 1)
    endif
    execute "mark" b:last_mark
    echo "Mark set:" b:last_mark
endfunction

nnoremap <leader>m :call NewMark()<CR>
