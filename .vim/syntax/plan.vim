syntax match planDayHeader        /\v^\= .*/
highlight link planDayHeader markdownH1

syntax match planEntryCompleted        /\v^\* .*/
highlight planEntryCompleted cterm=bold

syntax match planEntryContinuation        /\v^\| .*/
highlight link planEntryContinuation String
