" Add ` and * to iskeyword, so we can use word boundaries, in the syntax file,
" to better identify inline-code, and strong blocks
setlocal iskeyword+=`,*

" I use dashes for bullet lists, but the default configuration is not quite
" all right.
"
" Imagine I have the following (the cursor is placed where the | is):
"
"   - First item|
"
" When I press <Enter>, I get:
"
"   - First item
"     |
"
" While instead I wanted to create a new item in the list.
"
" This is mostly fault of the default configuration, that ships with `fb:-`
" stuffed inside &comments -- this basically tells Vim to indent the next
" line, but not to add a leading dash (which kind of makes sense, if &wrap
" was enabled.
"
" Anyway, let's remove this default, and add a new one that would add the
" leading dash to the line:
"
"   - First item
"   - |
"
setlocal comments-=fb:-
setlocal comments+=b:+

" Another annoying thing, which is happening by default
"
"   > Some quoted text|
"
" When I press <Enter>, I get:
"
"   > Some quoted text
"   > |
"
" What I want instead -- most of the times at least -- is to break out of
" the quoted area.
"
" This is happening because &comments contains 'n:>' by default
" (read `:help format-comments` for additional info)
setlocal comments-=n:>

setlocal synmaxcol=0
setlocal wrap textwidth=0
function! s:CreateNewPlanEntry() abort "{{{
    let l:last_ai=&autoindent
    let l:plan_title=strftime('%Y-%m-%d')
    let l:entry_exists=search('# ' . plan_title, 'n')
    setlocal noautoindent

    if l:entry_exists == 0
        " Go to the end of the file, and add a new line
        " Create additional empty line
        " Create the damn entry
        execute "normal! Go\<cr># " . plan_title
    else
        " Go to the end of the file, and add a new line
        " Create additional empty line
        " Create the separator
        execute "normal! Go\<cr>---\<cr>"
    endif

    let &l:autoindent=l:last_ai
endfunction "}}}
nnoremap <buffer> <localleader>n :<C-U>call <SID>CreateNewPlanEntry()<cr>o
nnoremap <buffer> <localleader>o :silent lgrep '^\?' %<cr>:lopen<cr>:redraw!<cr>
nnoremap <buffer> <localleader>O :execute 'lgrep "^\?.*\@' . input('Tag? ') . '" %'<cr>

nnoremap <buffer> <localleader>cc :STTConnect
nnoremap <buffer> <localleader>cd :STTDisconnect
nnoremap <buffer> <C-J> :<C-U>call SelectAndSendToTerminal('vv')<cr>
xnoremap <buffer> <C-J> :<C-U>call SendSelectionToTerminal(visualmode())<cr>
nmap <buffer> [[ zk
nmap <buffer> ]] zj

" Folding {{{
setlocal foldmethod=expr foldexpr=GetPlanFold(v:lnum)

function! GetPlanFold(lnum)
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
    endif
endfunction

function! IndentLevel(lnum)
    if getline(a:lnum) =~? '\v^\# [0-9]{4}'
        return 0
    endif
    if getline(a:lnum) =~? '\v^---$'
        return 1
    endif
    return 2
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

" }}}
