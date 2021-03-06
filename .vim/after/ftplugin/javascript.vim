function! TurnOnJavascriptFolding() abort "{{{
    let export       = '%(module\.)?export(s)?%(\.)?.*\{'
    let class        = 'class%(\s+\S+)*\s*\{'
    let method       = '%(static |async )?%(\S*\.\S*|if|for|switch)@!\S+\s*\([^)]*\)\s*\{'
    let functionwrap = '\s*[a-zA-Z0-9:]*\S*\)\s*\{'
    let functiondec  = '%(async )?function%(\s+\S+)?\s*\([^)]*' . functionwrap
    let functiondef  = '%(%(const|var|let)\s)?\S+\s*\=\s*' . functiondec
    let arrowdefwrap = '\s*[a-zA-Z0-9:]*\)\s*\=\>\s*\{'
    let arrowdef     = '%(%(const|var|let)\s)?\S+\s*\=\s*\([^)]*' . arrowdefwrap
    let router       = 'router\.\S+\([^}]*\{'
    let mocha_descr  = 'describe%(\.only)?\([^}]*\{'
    let mocha_it     = 'it\([^}]*\{'

    let folded_statements = [
                \ export,
                \ class,
                \ method,
                \ functionwrap,
                \ functiondec,
                \ functiondef,
                \ arrowdefwrap,
                \ arrowdef,
                \ router,
                \ mocha_descr,
                \ mocha_it
                \ ]

    let b:manual_regexp_folding_statements_re_bare = '\v^\s*%(' . join(folded_statements, '|') . ')\s*$'
    call TurnOnManualRegexpFolding()
endfunction "}}}
silent! call TurnOnJavascriptFolding()
silent! call RefreshManualRegexpFolding()

setlocal suffixesadd+=.js,.ts

let b:stt_substitute_eol_with = '@'

function! SelectTopLevelExpression(around) abort " {{{
    let motion = ''

    " Let's start by jumping to the end of the line: that's because
    " the cursor might be at the befinning of a function declaration (i.e.
    " with no outermost pair to jump to), so this hopefully moves the cursor to
    " the opening curly brace
    let motion .= 'g_'

    " However, the previous motion might have moved the cursor on a `;` char,
    " one that does not have a pair defined; so in that case, move the cursor
    " one char to the left (i.e. on a closing curly brace)
    let line = getline('.')
    if line[len(line) - 1] == ';'
        let motion .= 'h'
    endif

    " Jump to the outmost _pair_; this should gracefully handle the case where
    " the cursor is in the middle of:
    "
    " - array definitions (single, and nested ones)
    " - function parameters (i.e. its signature)
    " - or simply inside a nested block
    let motion .= '99[%'

    if a:around
        let motion .= 'V%'
    else
        let motion .= 'vi{'
    endif

    " Finally run the motion.  Note, `normal` instead of `normal!` needs to be
    " used, or otherwise we won't be able to use user-defined mappings
    echom motion
    execute "normal " . motion
endfunction " }}}
vnoremap <buffer> <silent>af :<C-U>call SelectTopLevelExpression(1)<CR>
onoremap <buffer> <silent>af :normal Vaf<CR>
vnoremap <buffer> <silent>if :<C-U>call SelectTopLevelExpression(0)<CR>
onoremap <buffer> <silent>if :normal vif<CR>

function! HighlightJavascriptRepl() abort " {{{
    syn match replPrompt /\v\>/
    syn match replComment /\v^\/\/.*/

    " syn match replResult /\v^#\<[^>]+\>$/
    hi def link replResult Debug
    hi def link replComment Comment
endfunction "}}}

function! InitializeJavascriptRepl() abort "{{{
    call HighlightJavascriptRepl()
endfunction "}}}

function! JavascriptCurrentWindowPlusNodeOnes() abort " {{{
    let focused_win = win_getid()

    for tab in gettabinfo()
        for win in tab.windows
            if win != focused_win
                let bufname = bufname(winbufnr(win))

                if bufname !~? 'node'
                    let winnr = win_id2win(win)
                    execute winnr . 'close'
                endif
            endif
        endfor
    endfor
endfunction " }}}

function! SetupJavascriptProjectMappings() abort " {{{
    nnoremap <C-W>o :call JavascriptCurrentWindowPlusNodeOnes()<cr>
    nnoremap <C-W>O :call JavascriptCurrentWindowPlusNodeOnes()<cr>
    nnoremap <C-W><C-O> :call JavascriptCurrentWindowPlusNodeOnes()<cr>
endfunction " }}}

function! OpenNodeRepl() abort "{{{
    call term_start("bash -c node-rlwrap", {
                \ "term_finish": "close",
                \ "vertical": 1
                \ })
    call InitializeJavascriptRepl()
    call SetupJavascriptProjectMappings()
endfunction "}}}
nnoremap <buffer> <localleader>o :call OpenNodeRepl()<cr>

function! ConnectNodeInspect() abort "{{{
    let l:host = input("Host: ", "localhost")
    let l:port = input("Port: ", "9229")
    let l:cmd = "node inspect " . l:host . ":" . l:port

    belowright call term_start(l:cmd, {
                \ "term_finish": "close",
                \ })
    wincmd k
endfunction " }}}
nnoremap <buffer> <localleader>cc :call ConnectNodeInspect()<cr>
nnoremap <buffer> <localleader>cd :STTDisconnect

" Fix windows:
" - <c-w>j: select window below -- Vlime Connection
" - <c-w>J: move it to the far bottom (and expand horizontally)
" - <c-w>k: select window above --  the actual lisp buffer
" - <c-w>H: move it to the far right (and expand vertically)
nnoremap <buffer> <localleader>W <c-w>j<c-w>J<c-w>k<c-w>H

nnoremap <buffer> <C-J> :<C-U>call SelectAndSendToTerminal('Vaf')<cr>
xnoremap <buffer> <C-J> :<C-U>call SendSelectionToTerminal(visualmode())<cr>
nnoremap <buffer> <C-^> :LspReferences<cr>
nnoremap <buffer> <silent> <C-]> :LspDefinition<cr>zvzz
nnoremap <buffer> <silent> gd :LspDefinition<cr>zvzz
nnoremap <buffer> <silent> ,S :LspRename<cr>
nnoremap <buffer> <silent> ◊ :LspCodeAction<cr>
nnoremap <buffer> <silent> K :LspHover<cr>
setlocal omnifunc=lsp#complete

inoremap <buffer> <c-n> <c-x><c-o>

RainbowParenthesesActivate
RainbowParenthesesLoadRound
RainbowParenthesesLoadSquare
RainbowParenthesesLoadBrace

" Abbreviations {{{

call MakeSpacelessBufferIabbrev('if',   'if (HERE)')
call MakeSpacelessBufferIabbrev('rt',   'return HERE;')
call MakeSpacelessBufferIabbrev('clog', 'console.log(HERE);')
call MakeSpacelessBufferIabbrev('cerr', 'console.error(HERE);')
call MakeSpacelessBufferIabbrev('pclog', 'console.log(JSON.stringify(HERE, null, 2));')
call MakeSpacelessBufferIabbrev('dolog', 'do(console.log)')
call MakeSpacelessBufferIabbrev('maplog', 'map(e => console.log(e) \|\| e)')
call MakeSpacelessBufferIabbrev('thenlog', 'then(e => console.log(e) \|\| e)')

" }}}
