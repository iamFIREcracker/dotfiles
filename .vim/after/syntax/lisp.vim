syntax match lispScratchMarker /\v\#;.*/
highlight link lispScratchMarker lispComment

" lispEscapeSpecial which is defined inside $RUNTIME, ends up matching
" escape chars first, so to tell Vim to to still process our lispEscapeChar
" definition, we have to add a `containedin=lispEscapeSpecial`
" https://stackoverflow.com/a/27687580
syntax match lispEscapeChar /\v\#[^ ()|]+/ containedin=lispEscapeSpecial
highlight link lispEscapeChar lispString

" Excluded forms {{{

syntax region lispExcludedList
            \ contained
            \ matchgroup=lispExcludedListOpenParen
            \ start="("
            \ skip="|.\{-}|"
            \ matchgroup=lispExcludedListCloseParen
            \ end=")"
            \ contains=lispExcludedList

highlight link lispExcludedList lispComment
highlight link lispExcludedListOpenParen lispComment
highlight link lispExcludedListCloseParen lispComment
highlight link lispExcludedContentStart lispComment
highlight link lispExcludedContentStop lispComment

function! s:createLispExcludedSyntaxRegion(regionName, regionPrefix) abort "{{{
    execute 'syntax region' a:regionName
                \ 'matchgroup=lispExcludedContentStart'
                \ 'start="' . a:regionPrefix . '("'
                \ 'skip="|.\{-}|"'
                \ 'matchgroup=lispExcludedContentStop'
                \ 'end=")"'
                \ 'contains=lispExcludedList'
    execute 'highlight link' a:regionName 'lispComment'
    execute 'syntax cluster lispBaseListCluster add=' . a:regionName
endfunction " }}}

" ... with NIL {{{

syntax match lispExcludedElementWithNil /\v\#\+nil *[^(`][^ ]+/
highlight link lispExcludedElementWithNil lispComment
syn cluster lispBaseListCluster add=lispExcludedElementWithNil

call s:createLispExcludedSyntaxRegion("lispExcludedFormWithNil", "#+nil *")
call s:createLispExcludedSyntaxRegion("lispExcludedQuotedFormWithNil", "#+nil *'")
call s:createLispExcludedSyntaxRegion("lispExcludedQuasiQuotedFormWithNil", "#+nil *`")
call s:createLispExcludedSyntaxRegion("lispExcludedSharpsignedDotFormWithNil", "#+nil *#\.")

" }}}
" ... with empty or, i.e. +(or nil) {{{

syntax match lispExcludedElementWithEmptyOr /\v\#\+\(or nil\) *[^(`][^ ]+/
highlight link lispExcludedElementWithEmptyOr lispComment
syn cluster lispBaseListCluster add=lispExcludedElementWithEmptyOr

call s:createLispExcludedSyntaxRegion("lispExcludedFormWithEmptyOr", "#+(or nil) *")
call s:createLispExcludedSyntaxRegion("lispExcludedQuotedFormWithEmptyOr", "#+(or nil) *'")
call s:createLispExcludedSyntaxRegion("lispExcludedQuasiQuotedFormWithEmptyOr", "#+(or nil) *`")
call s:createLispExcludedSyntaxRegion("lispExcludedSharpsignedDotFormWithEmptyOr", "#+(or nil) *#\.")

" }}}
" ... with #:_description_ {{{

syntax match lispExcludedElementWithDescription /\v\#\+\#:[^ ]+ *[^(`][^ ]+/
highlight link lispExcludedElementWithDescription lispComment
syn cluster lispBaseListCluster add=lispExcludedElementWithDescription

call s:createLispExcludedSyntaxRegion("lispExcludedFormWithDescription", "#+#:[^ ]\\+ *")
call s:createLispExcludedSyntaxRegion("lispExcludedQuotedFormWithDescription", "#+#:[^ ]\\+ *'")
call s:createLispExcludedSyntaxRegion("lispExcludedQuasiQuotedFormWithDescription", "#+#:[^ ]\\+ *`")
call s:createLispExcludedSyntaxRegion("lispExcludedSharpsignedDotFormWithDescription", "#+#:[^ ]\\+ *#\.")

" }}}

" }}}

" /usr/local/share/vim/vim82
" syn region lispQList			matchgroup=PreProc   start="'("  skip="|.\{-}|"			matchgroup=PreProc   end=")"		contains=@lispListCluster

" XXX temporarily borrowed from ftplugin
RainbowParenthesesActivate
RainbowParenthesesLoadRound
