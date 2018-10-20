syntax match planIntroduction /\v^.*/
highligh planIntroduction term=italic cterm=italic ctermfg=245

syntax region planDay start=/\v^\=.*/ end=/\v\ze^\=/

syntax match planEntry /\v^.*/    containedin=planDay contained
highlight link planEntry Normal

syntax match planDayHeader        /\v^\=.*/  containedin=planEntry contained
highlight link planDayHeader markdownH1

syntax match planEntryCompleted        /\v^\*.*/  containedin=planEntry contained
highlight planEntryCompleted cterm=bold

syntax match planEntryCompletedLater        /\v^\+.*/  containedin=planEntry contained
highlight planEntryCompletedLater cterm=bold

syntax match planEntryOpen        /\v^\?.*/  containedin=planEntry contained
highlight link planEntryOpen WarningMsg

syntax match planEntryDiscarded        /\v^-.*/  containedin=planEntry contained
highlight link planEntryDiscarded NonText

syntax match planEntryContinuation        /\v^\|.*/  containedin=planEntry contained
highlight link planEntryContinuation String
