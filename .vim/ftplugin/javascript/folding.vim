let s:export       = '%(module\.)?export(s)?\.*%(\(|\{)'
let s:class        = 'class\s+\S+\s*\{'
let s:method       = '%(\S*\.\S*|if|for|switch)@!\S+\s*\([^)]*\)\s*\{'
let s:functionwrap = '\s*[a-zA-Z0-9:]*\S*\)\s*\{'
let s:functiondec  = 'function%(\s+\S+)?\s*\([^)]*' . s:functionwrap
let s:functiondef  = '%(%(const|var|let)\s)?\S+\s*\=\s*' . s:functiondec
let s:arrowdefwrap = '\s*[a-zA-Z0-9:]*\)\s*\=\>\s*\{'
let s:arrowdef     = '%(%(const|var|let)\s)?\S+\s*\=\s*\([^)]*' . s:arrowdefwrap
let s:router       = 'router\.\S+\([^}]*\{'

let s:folded_statements = [
            \ s:export,
            \ s:class,
            \ s:method,
            \ s:functionwrap,
            \ s:functiondec,
            \ s:functiondef,
            \ s:arrowdefwrap,
            \ s:arrowdef,
            \ s:router
            \ ]

let b:statements_re_bare = '\v^\s*%(' . join(s:folded_statements, '|') . ')\s*$'

function! UpdateJavascriptFolds()
    " Save cursor position
    let js_indent_view = winsaveview()

    " Delete all folds
    normal zE

    " Move to the beginning of the file
    normal gg

    " Search matching lines
    while search(b:statements_re_bare, 'cW') != 0
        " Create fold
        normal $zf%

        " Open it
        normal zo
    endwhile

    " Close all folds
    normal zM

    " Restore cursor position
    call winrestview(js_indent_view)

    " Open fold
    normal zo
endfunction

function! TurnOnJavascriptFolding()
    setlocal foldmethod=manual

    call UpdateJavascriptFolds()
endfunction
