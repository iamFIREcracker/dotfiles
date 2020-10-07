function! DirectiveUnderCursor() abort " {{{
    let directive = expand('<cWORD>')
    if directive =~ '='
        let directive = split(directive, '=')[0]
    endif
    if directive =~ '>'
        let directive = split(directive, '>')[0]
    endif
    let directive = substitute(directive, '<\|/\|>', '', 'g')
    return directive
endfunction " }}}
function! FindDirectiveOccurrences() abort " {{{
    let directive = DirectiveUnderCursor()

    exe "normal! :Ack! --literal " . directive . "\<cr>"
endfunction "}}}
function! GuessDirectiveFilename() abort " {{{
    let inex_save = &inex

    function! FilenameOfDirectiveUnderCursor() abort " {{{
        let directive = DirectiveUnderCursor()
        let filename = substitute(directive, '-', '', 'g')
        return filename
    endfunction  " }}}

    set includeexpr=FilenameOfDirectiveUnderCursor()
    normal! gf

    let &inex = inex_save
endfunction "}}}

setlocal suffixesadd+=.js path+=**
nnoremap <buffer> <C-^> :call FindDirectiveOccurrences()<cr>
nnoremap <buffer> gf :call GuessDirectiveFilename()<cr>
