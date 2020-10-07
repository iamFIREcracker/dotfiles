function! HighlightSchemeRepl() abort " {{{
    " set syntax=lisp
    syn match replPrompt /\v^\[([a-z A-Z])+\] [-._a-zA-Z0-9]+\>/
    syn match replComment /\v^;.*/

    " syn match replResult /\v^#\<[^>]+\>$/
    hi def link replResult Debug
    hi def link replComment Comment
endfunction "}}}

function! InitializeSchemeRepl() abort "{{{
    call HighlightSchemeRepl()
endfunction "}}}

function! OpenSchemeReplChez() abort "{{{
    call term_start("bash -c chez-rlwrap", {
        \ "term_finish": "close",
        \ "vertical": 1
    \ })
    call InitializeSchemeRepl()
endfunction "}}}

function! OpenSchemeReplPrompt() abort "{{{
    call term_start("bash -c " . input("? "), {
        \ "term_finish": "close",
        \ "vertical": 1
    \ })
    call InitializeSchemeRepl()
endfunction "}}}

function! MotionToSelectTopLevelSchemeForm() abort " {{{
    let motion = '99[(v%'
    if col('.') == 1 && getline('.')[col('.') - 1] == '('
        " We are at the beginning of a top-level form, so select until
        " the matching parenthesis
        let motion = 'v%'
    endif
    return motion
endfunction "}}}

function! SelectToplevelSchemeFormAndSendToTerminal() abort "{{{
    call SelectAndSendToTerminal(MotionToSelectTopLevelSchemeForm())
endfunction "}}}

function! SelectToplevelSchemeFormAndSendToVlimeREPL() abort "{{{
    let view = winsaveview()
    let reg_save = @@
    let motion = MotionToSelectTopLevelSchemeForm()

    execute "normal! " . motion . "y"
    call vlime#plugin#SendToREPL(@@)

    let @@ = reg_save
    call winrestview(view)
endfunction "}}}

function! IndentToplevelSchemeForm() abort "{{{
    let view = winsaveview()
    let reg_save = @@
    let motion = MotionToSelectTopLevelSchemeForm()

    execute "normal! " . motion . "="

    let @@ = reg_save
    call winrestview(view)
endfunction "}}}

call SetSchemeWords()

setlocal iskeyword+=!,?,%,-
setlocal suffixesadd+=.scm

RainbowParenthesesActivate
RainbowParenthesesLoadRound

" Fix windows:
" - <c-w>j: select window below -- Vlime Connection
" - <c-w>J: move it to the far bottom (and expand horizontally)
" - <c-w>k: select window above --  the actual lisp buffer
" - <c-w>H: move it to the far right (and expand vertically)
nnoremap <buffer> <localleader>W <c-w>j<c-w>J<c-w>k<c-w>H

let b:delimitMate_quotes = "\""

nnoremap <buffer> <silent> <localleader>o :call OpenSchemeReplChez()<cr>
nnoremap <buffer> <silent> <localleader>O :call OpenSchemeReplPrompt()<cr>
nnoremap <buffer> <silent> gI :<C-U>call IndentToplevelSchemeForm()<cr>
nnoremap <buffer> <localleader>cc :STTConnect
nnoremap <buffer> <localleader>cd :STTDisconnect
nnoremap <buffer> <silent> <C-J> :call SelectToplevelSchemeFormAndSendToTerminal()<cr>
xnoremap <buffer> <silent> <C-J> :call SendSelectionToTerminal(visualmode())<cr>
