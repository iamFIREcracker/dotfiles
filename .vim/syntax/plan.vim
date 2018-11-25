syntax match planIntroduction /\v^.*/
highligh planIntroduction term=italic cterm=italic ctermfg=245

syntax region planDay start=/\v^\#.*/ end=/\v\ze^\#/

syntax match planEntry /\v^.*/    containedin=planDay contained
highlight link planEntry Normal

syntax match planDayHeader        /\v^\#.*/  containedin=planEntry contained
highlight link planDayHeader markdownH1

syntax match planEntryCompleted        /\v^\*.*/  containedin=planEntry contained
highlight planEntryCompleted cterm=bold

syntax match planEntryCompletedLater        /\v^\+.*/  containedin=planEntry contained
highlight planEntryCompletedLater cterm=bold

syntax match planEntryOpen        /\v^\?.*/  containedin=planEntry contained
highlight link planEntryOpen WarningMsg

syntax match planEntryDiscarded        /\v^\~.*/  containedin=planEntry contained
highlight link planEntryDiscarded NonText

syntax match planEntryContinuation        /\v^\|.*/  containedin=planEntry contained
highlight link planEntryContinuation String

syntax match planEntryQuote        /\v^    .*/  containedin=planEntry contained
highlight link planEntryQuote String

syntax match planEntryNamedLink        /\v\[[^]]*]\([^)]*\)/  containedin=planEntry,planEntryCompleted contained
highlight link planEntryNamedLink String

syntax match planEntryUnamedLink        /\v\<[^>]+\>/  containedin=planEntry contained
highlight link planEntryUnamedLink String

syntax match planEntryUnamedLink        /\v\<[^>]+\>/  containedin=planEntry contained
highlight link planEntryUnamedLink String

syntax match planEntryInlineCode /\v`[^`]+`/ containedin=planEntry contained
highlight link planEntryInlineCode String

syntax region planEntryCode start=/\v```/ end=/\v```/ containedin=planEntry contained
highlight link planEntryCode String
