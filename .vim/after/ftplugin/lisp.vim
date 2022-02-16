function! HighlightLispRepl() abort " {{{
    syn match replPrompt /\v^\[([a-z A-Z])+\] [-._a-zA-Z0-9]+\>/
    syn match replComment /\v^;.*/

    hi def link replResult Debug
    hi def link replComment Comment
endfunction "}}}

function! InitializeLispRepl() abort "{{{
    call HighlightLispRepl()
endfunction "}}}

function! LispCurrentWindowPlusVlimeOnes() abort " {{{
    let focused_win = win_getid()

    for tab in gettabinfo()
        for win in tab.windows
            if win != focused_win
                let bufname = bufname(winbufnr(win))

                if (bufname !~? 'bash.*-vlime') && (bufname !~? 'vlime | repl')
                    let winnr = win_id2win(win)
                    execute winnr . 'close'
                endif
            endif
        endfor
    endfor
endfunction " }}}

function! SetupLispProjectMappings() abort " {{{
endfunction " }}}

function! OpenLispReplSBCL() abort "{{{
    Start! -title=REPL "sbcl-vlime"
    " call term_start("sbcl-vlime", {
    "     \ "term_finish": "close",
    "     \ "vertical": 1
    " \ })
    " call InitializeLispRepl()
    call SetupLispProjectMappings()
endfunction "}}}

function! OpenLispReplPrompt() abort "{{{
    Start! -title=REPL input("? ")
    " call term_start("bash -c " . input("? "), {
    "     \ "term_finish": "close",
    "     \ "vertical": 1
    " \ })
    " call InitializeLispRepl()
    call SetupLispProjectMappings()
endfunction "}}}

function! MotionToSelectTopLevelLispForm() abort " {{{
    return 'vaF'
endfunction "}}}

function! SelectToplevelLispFormAndSendToVlimeREPL() abort "{{{
    let view = winsaveview()
    let reg_save = @@
    let motion = MotionToSelectTopLevelLispForm()

    execute "normal " . motion . "y"
    call vlime#plugin#SendToREPL(@@)

    let @@ = reg_save
    call winrestview(view)
endfunction "}}}

function! SelectToplevelLispFormAndSendToTerminal() abort "{{{
    let view = winsaveview()
    let reg_save = @@
    let motion = MotionToSelectTopLevelLispForm()

    execute "normal " . motion . "y"
    call SendToTerminal(@@)

    let @@ = reg_save
    call winrestview(view)
endfunction "}}}

function! IndentToplevelLispForm() abort "{{{
    let view = winsaveview()
    let reg_save = @@
    let motion = MotionToSelectTopLevelLispForm()

    execute "normal " . motion . "="

    let @@ = reg_save
    call winrestview(view)
endfunction "}}}

function! QuickprojectMakePrompt() abort "{{{
    let l:path = getcwd()
    let l:guessed_name = split(l:path, '/')[-1]
    let l:name = input("? ", guessed_name)

    call vlime#plugin#SendToREPL("(quickproject:make-project #p\"" . l:path . "\" :name \"" . l:name . "\")\n")
endfunction " }}}

function! QuickloadLispSystem() abort "{{{
    let systems = split(system('ls -1 *.asd | grep -v test | cut -d. -f1 | uniq')) " its fine
    if len(systems) == 0
        echom "Could not find any .asd files..."
        return
    elseif len(systems) > 1
        echom "Found too many .asd files..."
        return
    endif

    call vlime#plugin#SendToREPL("(ql:quickload :" . systems[0] . ")")
endfunction " }}}

function! QuickloadLispPrompt() abort "{{{
    call vlime#plugin#SendToREPL("(ql:quickload :" . input("? ") . ")\n")
endfunction " }}}

function! TestLispSystem() abort "{{{
    let systems = split(system('ls -1 *.asd | grep -v test | cut -d. -f1 | uniq')) " its fine
    if len(systems) == 0
        echom "Could not find any .asd files..."
        return
    elseif len(systems) > 1
        echom "Found too many .asd files..."
        return
    endif

    call vlime#plugin#SendToREPL("(asdf:test-system :" . systems[0] . ")")
endfunction " }}}

function! TestLispPrompt() abort "{{{
    call vlime#plugin#SendToREPL("(asdf:test-system :" . input("? ") . ")\n")
endfunction " }}}

setlocal iskeyword+=!,?,%,-
setlocal suffixesadd+=.lisp

RainbowParenthesesActivate
RainbowParenthesesLoadRound

" Fix windows:
" - <c-w>j: select window below -- Vlime Connection
" - <c-w>J: move it to the far bottom (and expand horizontally)
" - <c-w>k: select window above --  the actual lisp buffer
" - <c-w>H: move it to the far right (and expand vertically)
nnoremap <buffer> <localleader>W <c-w>j<c-w>J<c-w>k<c-w>H

let b:match_skip = 's:comment\|string\|escape\|symbol'

let b:delimitMate_quotes = "\""
let b:stt_substitute_eol_with = '@@'

inoremap <buffer> <c-n> <c-x><c-o>

nnoremap <buffer> <silent> <localleader>o :call OpenLispReplSBCL()<cr>
nnoremap <buffer> <silent> <localleader>O :call OpenLispReplPrompt()<cr>
nnoremap <buffer> <silent> <localleader>n :call QuickprojectMakePrompt()<cr>
nnoremap <buffer> <silent> <localleader>q :call QuickloadLispSystem()<cr>
nnoremap <buffer> <silent> <localleader>Q :call QuickloadLispPrompt()<cr>
nnoremap <buffer> <silent> <localleader>t :call TestLispSystem()<cr>
nnoremap <buffer> <silent> <localleader>T :call TestLispPrompt()<cr>
nnoremap <buffer> <silent> <C-I> :call IndentToplevelLispForm()<cr>
nnoremap <buffer> <silent> <C-J> :<C-U>call SelectToplevelLispFormAndSendToVlimeREPL()<CR>
xmap <buffer> <silent> <C-J> <localleader>s
nnoremap <buffer> <silent> g<C-J> :<C-U>call SelectToplevelLispFormAndSendToTerminal()<CR>
xnoremap <buffer> <silent> g<C-J> :<C-U>call SendSelectionToTerminal(visualmode())<cr>
nmap <buffer> <silent> K <localleader>ddo
nmap <buffer> <silent> <C-]> <localleader>xd
nmap <buffer> <silent> <C-^> <localleader>xc
nnoremap <buffer> <C-W>o :call LispCurrentWindowPlusVlimeOnes()<cr>
nnoremap <buffer> <C-W>O :call LispCurrentWindowPlusVlimeOnes()<cr>
nnoremap <buffer> <C-W><C-O> :call LispCurrentWindowPlusVlimeOnes()<cr>
