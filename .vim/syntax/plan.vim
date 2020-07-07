syntax match planIntroduction /\v^.*/
highligh planIntroduction term=italic cterm=italic ctermfg=245

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

syntax match planEntryQuote        /\v^    .*/  containedin=planDay contained
highlight link planEntryQuote String

syntax match planEntryNamedLink        /\v\[[^]]*]\([^)]*\)/  containedin=planDay,planEntryCompleted contained
highlight link planEntryNamedLink String

syntax match planEntryUnamedLink        /\v\<[^>]+\>/  containedin=planDay contained
highlight link planEntryUnamedLink String

syntax match planEntryInlineCode /\v<`[^`]+`>/ containedin=planDay contained
highlight link planEntryInlineCode String

syntax match planEntryItalic /\v<_[^_]+_>/ containedin=planDay contained
highlight planEntryItalic term=italic cterm=italic

syntax match planEntryBold /\v<\*\*[^*]+\*\*>/ containedin=planDay contained
highligh planEntryBold term=bold cterm=bold

syntax region planEntryCode start=/\v```/ end=/\v```/ containedin=planDay contained
highlight link planEntryCode String
