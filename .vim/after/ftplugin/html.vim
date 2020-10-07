setlocal foldmethod=manual

function! HtmlWrap() abort "{{{
    " Back up x registry
    let old_x = @x

    " Copy html element to x registry
    normal! vi<"xy

    if len(split(@x, '\n', 0)) != 1
        " Unwrap
        normal! vi<J
    else
        " Wrap:
        "
        " \s+ - any leading whitespace (there has to be whitespace!),
        "       otherwise we are going to end up splitting properties like:
        "
        "           attr.href="/?query=foo&bar=bax"
        "
        " ( ... ) - the first group of matched text -- for the substitution
        "     \* - a literal asterisk, used by Angular
        "     \[ ... \]  - literal square brackets, used by Angular
        "     \( ... \)  - literal braces, used by Angular
        "     (\w|-|\.)+ - multiple word character, or hypen, or .
        " ( ... ) - the second group of matched text
        "     (\=|$) - literal =, or end of line -- e.g. ng-onload, as well
        "                                           as allowFullscreen
        let @x = substitute(@x, '\v\s+(\*?\[?\(?(\w|-|\.)+\)?\]?)\=', '\n\1\=', 'g')

        " Replace selection with the modified content
        normal! gv"xp

        " Re-indent the new content
        normal! gv=
    endif

    " Restore x registry
    let @x = old_x
endfunction " }}}
nnoremap <buffer> gw :call HtmlWrap()<cr>

" Use Shift-Return (Ø) in insert mode to turn this:
"     <tag>|</tag>
"
" into this:
"     <tag>
"         |
"     </tag>
inoremap <buffer> Ø <esc>cit<cr><esc>ko
" Use Shift-Return (Ø) in normal mode to turn this:
"     <tag>something|else</tag>
"
" into this:
"     <tag>
"         |something else
"     </tag>
nnoremap <buffer> Ø <esc>vit<esc>a<cr><esc>vito<esc>i<cr><esc>
