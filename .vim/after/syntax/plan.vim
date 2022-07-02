syntax match planSubHeader /\v^\#{1,6} .*/
highlight planSubHeader ctermfg=121

syntax match planHeader /\v^\# .*/
highlight planHeader term=bold ctermfg=154

syntax match planDayHeader        /\v^\# [0-9]{4}.*/
highlight link planDayHeader planHeader

" syntax match planIndentation  /\v^(\| )+/
" highlight planIndentation ctermfg=238
" syn cluster planInlineElementsCluster add=planIndentation

syntax match planEntryCompleted        /\v^(  )*\*/
highlight planEntryCompleted cterm=bold

syntax match planEntryCompletedLater        /\v^(  )*\+/
highlight link planEntryCompletedLater planEntryCompleted

syntax match planEntryOpen        /\v^(  )*\?/
highlight planEntryOpen ctermfg=211

syntax match planEntryDiscarded        /\v^(  )*\~ .*/ contains=@planInlineElementsCluster
highlight planEntryDiscarded ctermfg=238

syntax match planEntrySnippet        /\v^    .*/   contains=@NoSpell
highlight link planEntrySnippet String

syntax match planEntryQuote        /\v^(  )*\>.*/    contains=@planInlineElementsCluster
highlight planEntryQuote cterm=italic ctermfg=245

syntax match planEntryQuote1        /\v^(  )*\>\>.*/    contains=@planInlineElementsCluster
highlight planEntryQuote1 cterm=italic ctermfg=211

syntax match planEntryQuote2        /\v^(  )*\>\>\>.*/    contains=@planInlineElementsCluster
highlight planEntryQuote2 cterm=italic ctermfg=222

syntax match planEmphasis /\v<_.{-}_>/
highlight planEmphasis cterm=italic
syn cluster planInlineElementsCluster add=planEmphasis

syntax match planStrong /\v\*\*.{-}\*\*/
highlight planStrong cterm=bold
syn cluster planInlineElementsCluster add=planStrong

syntax match planMono /\v`[^`]*`/ contains=@NoSpell
highlight planMono cterm=bold ctermfg=245
syn cluster planInlineElementsCluster add=planMono

syntax region planEntryCodeBlock start="\v```" end="\v```"  keepend contains=@NoSpell
highlight link planEntryCodeBlock planMono

syntax match planLink        /\vhttps?[^ ;!)]*/  contains=@NoSpell
highlight link planLink planMono
syn cluster planInlineElementsCluster add=planLink

syntax match planNamedLink        /\v\[[^]]*]\([^)]*\)/  contains=@NoSpell
highlight link planNamedLink planMono
syn cluster planInlineElementsCluster add=planNamedLink

syntax match planTag        /\v(\W|^)\zs\@\k+\ze/  contains=@NoSpell
" highlight planTag ctermfg=214
highlight link planTag planMono
syn cluster planInlineElementsCluster add=planTag
syntax match planTagMultiWord        /\v(\W|^)\zs\@"[^"]*"\ze/  contains=@NoSpell
highlight link planTagMultiWord planMono
syn cluster planInlineElementsCluster add=planTagMultiWord

syntax match planReferenceKey        /\v#[0-9]{8}-\k+/  contains=@NoSpell
highlight link planReferenceKey planHeader
syn cluster planInlineElementsCluster add=planReferenceKey

syntax match planReferenceKeyTag        /\v\^[0-9]{8}-\k+/  contains=@NoSpell
highlight link planReferenceKeyTag planMono
syn cluster planInlineElementsCluster add=planReferenceKeyTag
