syntax match planIntroduction /\v^.*/
highlight planIntroduction cterm=italic ctermfg=245

syntax region planDay start=/\v^\#.*/ end=/\v\ze^\#/
highlight link planDay Normal

syntax match planDayHeader        /\v^\# [0-9]{4}.*/  containedin=planDay contained
highlight link planDayHeader Title

syntax match planDaySubHeader        /\v^\#\# .*/  containedin=planDay contained
highlight link planDaySubHeader Title

syntax match planEntryCompleted        /\v^\*.*/  containedin=planDay contained
highlight planEntryCompleted cterm=bold

syntax match planEntryCompletedLater        /\v^\+.*/  containedin=planDay contained
highlight planEntryCompletedLater cterm=bold

syntax match planEntryOpen        /\v^\?.*/  containedin=planDay contained
highlight link planEntryOpen Error

syntax match planEntryDiscarded        /\v^\~.*/  containedin=planDay contained
highlight link planEntryDiscarded NonText

syntax match planEntrySnippet        /\v^    .*/  containedin=planDay contained contains=@NoSpell
highlight link planEntrySnippet String

syntax match planEntryQuote        /\v^\>.*/  containedin=planDay contained contains=@NoSpell
highlight planEntryQuote cterm=italic ctermfg=245

syntax match planEntryQuote1        /\v^\>\>.*/  containedin=planDay contained contains=@NoSpell
highlight planEntryQuote1 cterm=italic ctermfg=211

syntax match planEntryQuote2        /\v^\>\>\>.*/  containedin=planDay contained contains=@NoSpell
highlight planEntryQuote2 cterm=italic ctermfg=222

syntax match planEmphasis /\v<_.{-}_>/ containedin=planDay,planEntryOpen contained
highlight planEmphasis cterm=italic
syntax match planEmphasisCompleted /\v<_.{-}_>/ containedin=planEntryCompleted,planEntryCompletedLater contained
highlight planEmphasisCompleted cterm=bold,italic

syntax match planStrong /\v<\*\*.{-}\*\*>/ containedin=planDay,planEntryCompleted,planEntryCompletedLater,planEntryOpen contained
highlight planStrong cterm=bold

syntax match planInlineCode /\v`[^`]*`/ containedin=planDay contained contains=@NoSpell
highlight link planInlineCode String

syntax match planLink        /\vhttps?[^ ;!)]*/  containedin=planDay,planEntryCompleted,planEntryCompletedLater,planEntryOpen contained contains=@NoSpell
highlight link planLink String

syntax match planNamedLink        /\v\[[^]]*]\([^)]*\)/  containedin=planDay,planEntryCompleted,planEntryCompletedLater,planEntryOpen contained contains=@NoSpell
highlight link planNamedLink String

syntax match planEntryNamedLink        /\v\[[^]]*]\([^)]*\)/  containedin=planDay,planEntryCompleted,planEntryCompletedLater,planEntryOpen contained contains=@NoSpell
highlight link planEntryNamedLink String

syntax match planEntryTag        /\v(^| )\@[^@]\w*/  containedin=planDay,planEntryCompleted,planEntryCompletedLater,planEntryOpen contained contains=@NoSpell
highlight planEntryTag ctermfg=214
