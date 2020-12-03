setlocal spell
let b:delimitMate_nesting_quotes = ['`']

nnoremap <buffer> <localleader>cc :STTConnect
nnoremap <buffer> <localleader>cd :STTDisconnect
nnoremap <buffer> <C-J> :<C-U>call SelectAndSendToTerminal('vv')<cr>
xnoremap <buffer> <C-J> :<C-U>call SendSelectionToTerminal(visualmode())<cr>

" Folding {{{

" folding for Markdown headers, both styles (atx- and setex-)
" http://daringfireball.net/projects/markdown/syntax#header
"
" this code can be placed in file
"   $HOME/.vim/after/ftplugin/markdown.vim

function! Get_Markdown_Header_Level(lnum) " {{{
    let l1 = getline(a:lnum)

    if l1 =~ '^\s*$'
        " a blank line is never a header
        return 0
    endif

    let l2 = getline(a:lnum+1)

    if  l2 =~ '^==\+\s*'
        " next line is underlined (level 1)
        return 1
    elseif l2 =~ '^--\+\s*'
        " next line is underlined (level 2)
        return 2
    elseif l1 =~ '^#'
        " current line starts with hashes
        return matchend(l1, '^#\+')
    endif

    return 0
endfunction " }}}

function! Foldexpr_markdown(lnum) " {{{
    let line = getline(a:lnum)
    let anchor_re = '\v^\<a name.*'

    if line =~ anchor_re
        let next_line_hval = Get_Markdown_Header_Level(a:lnum + 1)
        if next_line_hval
            return '>' . next_line_hval
        else
            return '='
        endif
    endif

    let line_hval = Get_Markdown_Header_Level(a:lnum)

    if line_hval
        if getline(a:lnum - 1) =~ anchor_re
            return line_hval
        else
            return '>' . line_hval
        endif
    elseif a:lnum == 1
        " fold any 'preamble'
        return '>1'
    else
        " keep previous foldlevel
        return '='
    endif
endfunction " }}}

function! MarkdownFoldText() " {{{
    let line = getline(v:foldstart)

    let anchor_re = '\v^\<a name.*'
    if line =~ anchor_re
        let line = getline(v:foldstart + 1)
    end

    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart

    " expand tabs into spaces
    let onetab = strpart('          ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')

    let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
    let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
    return line . '…' . repeat(" ",fillcharcount) . foldedlinecount . '…' . ' '
endfunction " }}}

setlocal foldtext=MarkdownFoldText()
setlocal foldexpr=Foldexpr_markdown(v:lnum)
setlocal foldmethod=expr

"---------- everything after this is optional -----------------------
" change the following fold options to your liking
" see ':help fold-options' for more
setlocal foldenable foldlevel=0 foldcolumn=0

" }}}
