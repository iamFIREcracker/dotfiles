syntax match planIntroduction /\v^.*/
highlight planIntroduction cterm=italic ctermfg=245

syntax region planDay start=/\v^\#.*/ end=/\v\ze^\#/
highlight link planDay Normal

syntax match planDayHeader        /\v^\# [0-9]{4}.*/  containedin=planDay contained
highlight link planDayHeader markdownH1

syntax match planEntryCompleted        /\v^\*.*/  containedin=planDay contained
highlight planEntryCompleted cterm=bold

syntax match planEntryCompletedLater        /\v^\+.*/  containedin=planDay contained
highlight planEntryCompletedLater cterm=bold

syntax match planEntryOpen        /\v^\?.*/  containedin=planDay contained
highlight link planEntryOpen WarningMsg

syntax match planEntryDiscarded        /\v^\~.*/  containedin=planDay contained
highlight link planEntryDiscarded NonText

syntax match planEntrySnippet        /\v^    .*/  containedin=planDay contained contains=@NoSpell
highlight link planEntrySnippet String

syntax match planEntryQuote        /\v^\> .*/  containedin=planDay contained contains=@NoSpell
highlight planEntryQuote cterm=italic ctermfg=245

syntax match planEntryNamedLink        /\v\[[^]]*]\([^)]*\)/  containedin=planDay,planEntryCompleted contained contains=@NoSpell
highlight link planEntryNamedLink String

syntax match planEntryUnamedLink        /\v\<[^>]+\>/  containedin=planDay contained
highlight link planEntryUnamedLink String

syntax match planEntryInlineCode /\v`[^`]+`/ containedin=planDay contained contains=@NoSpell
highlight link planEntryInlineCode String

syntax match planEntryItalic /\v<_[^_]+_>/ containedin=planDay contained
highlight planEntryItalic cterm=italic

syntax match planEntryBold /\v\*\*[^*]+\*\*/ containedin=planDay contained
highlight planEntryBold cterm=bold

