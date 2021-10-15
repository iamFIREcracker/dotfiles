function! EatChar(pat)
    let c = nr2char(getchar(0))
    return (c =~ a:pat) ? '' : c
endfunction

" XXX can we disable these while inside _comment_ areas?!
function! MakeSpacelessIabbrev(from, to) abort " {{{
    execute "inoreabbr <silent> ".a:from." ".a:to."<C-R>=EatChar('\\s')<CR>"
endfunction " }}}
function! MakeSpacelessBufferIabbrev(from, to) " {{{
    let cmd = "inoreabbr <silent> <buffer> ".a:from." ".a:to
    if a:to =~ 'HERE'
        let cmd .= "<ESC>" .
            \ ":let search_active=v:hlsearch<CR>" .
            \ "?HERE<CR>\"_dw" .
            \ ":call histdel('search', -1)<CR>" .
            \ ":let @/ = histget('search', -1)<CR>" .
            \ ":if search_active == 0 <bar> noh <bar> endif<CR>" .
            \ (a:to =~ 'HERE$' ? "a" : "i")
    endif
    let cmd .= "<C-R>=EatChar('\\s')<CR>"

    execute cmd
endfunction " }}}
