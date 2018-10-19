syntax match planDayHeader        /\v^\=.*/
highlight link planDayHeader markdownH1

syntax match planEntryCompleted        /\v^\*.*/
highlight planEntryCompleted cterm=bold

syntax match planEntryCompletedLater        /\v^\+.*/
highlight planEntryCompletedLater cterm=bold

syntax match planEntryOpen        /\v^\?.*/
highlight link planEntryOpen WarningMsg

syntax match planEntryContinuation        /\v^\|.*/
highlight link planEntryContinuation String
