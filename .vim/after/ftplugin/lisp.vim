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

                if bufname !~? 'vlime'
                    let winnr = win_id2win(win)
                    execute winnr . 'close'
                endif
            endif
        endfor
    endfor
endfunction " }}}

function! SetupLispProjectMappings() abort " {{{
    nnoremap <C-W>o :call LispCurrentWindowPlusVlimeOnes()<cr>
    nnoremap <C-W>O :call LispCurrentWindowPlusVlimeOnes()<cr>
    nnoremap <C-W><C-O> :call LispCurrentWindowPlusVlimeOnes()<cr>
endfunction " }}}

function! OpenLispReplSBCL() abort "{{{
    call term_start("bash -c sbcl-vlime", {
        \ "term_finish": "close",
        \ "vertical": 1
    \ })
    call InitializeLispRepl()
    call SetupLispProjectMappings()
endfunction "}}}

function! OpenLispReplPrompt() abort "{{{
    call term_start("bash -c " . input("? "), {
        \ "term_finish": "close",
        \ "vertical": 1
    \ })
    call InitializeLispRepl()
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

    call SendToTerminal("(quickproject:make-project #p\"" . l:path . "\" :name \"" . l:name . "\")\n")
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

    call SendToTerminal("(ql:quickload :" . systems[0] . ")")
endfunction " }}}

function! QuickloadLispPrompt() abort "{{{
    call SendToTerminal("(ql:quickload :" . input("? ") . ")\n")
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

    call SendToTerminal("(asdf:test-system :" . systems[0] . ")")
endfunction " }}}

function! TestLispPrompt() abort "{{{
    call SendToTerminal("(asdf:test-system :" . input("? ") . ")\n")
endfunction " }}}

function! InPackage() abort "{{{
    let packages = split(system('grep "(in-package .*" '. fnameescape(expand("%")) .' --only-matching | grep -v cl-user | cut -d" " -f2 | uniq')) " its fine
    if len(packages) == 0
        echom "Could not find any defpackage lines..."
        return
    elseif len(packages) > 1
        echom "Found too many defpackage lines..."
        return
    endif

    call SendToTerminal("(in-package " . packages[0])
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

let b:delimitMate_quotes = "\""

inoremap <buffer> <c-n> <c-x><c-o>

nnoremap <buffer> <silent> <localleader>o :call OpenLispReplSBCL()<cr>
nnoremap <buffer> <silent> <localleader>O :call OpenLispReplPrompt()<cr>
nnoremap <buffer> <silent> gI :<C-U>call IndentToplevelLispForm()<cr>
nnoremap <buffer> <silent> <localleader>n :call QuickprojectMakePrompt()<cr>
nnoremap <buffer> <silent> <localleader>q :call QuickloadLispSystem()<cr>
nnoremap <buffer> <silent> <localleader>Q :call QuickloadLispPrompt()<cr>
nnoremap <buffer> <silent> <localleader>t :call TestLispSystem()<cr>
nnoremap <buffer> <silent> <localleader>T :call TestLispPrompt()<cr>
if !exists('b:vlime_mappings_unmapped')
    let b:vlime_mappings_unmapped=1
    silent! unmap <buffer> <localleader>of
    silent! unmap <buffer> <localleader>ot
    silent! unmap <buffer> <localleader>oe
    silent! unmap <buffer> <localleader>Tt
    silent! unmap <buffer> <localleader>TT
    silent! unmap <buffer> <localleader>Ti
    silent! unmap <buffer> <localleader>TI
    silent! unmap <buffer> <localleader>Td
    silent! unmap <buffer> <localleader>TD
endif
nnoremap <buffer> <silent> <localleader>i :call InPackage()<cr>
nnoremap <buffer> <silent> gs :call SelectToplevelLispFormAndSendToTerminal()<cr>
xnoremap <buffer> <silent> gs :call SendSelectionToTerminal(visualmode())<cr>

" Vlime's send-top-level-s-expression mapping is not always working as
" expected -- it seems it does not properly handle comments that include
" s-expressions -- so what we are doing here:
"
" 1) select the top-level expression, manually
" 2) send it
nnoremap <buffer> <silent> <C-J> :<C-U>call SelectToplevelLispFormAndSendToVlimeREPL()<CR>
xmap <buffer> <silent> <C-J> <localleader>s

nmap <buffer> <silent> K <localleader>ddo
nmap <buffer> <silent> <C-]> <localleader>xd
