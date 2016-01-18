highlight jiraBold cterm=bold
highlight jiraHeadings ctermfg=magenta
highlight jiraItalic cterm=underline
highlight jiraCode ctermfg=cyan
highlight jiraCodeInline ctermfg=cyan
highlight jiraQuote ctermfg=darkgrey
highlight jiraQuoteInline ctermfg=darkgrey

syntax match jiraBold /\*[^*]\+\*/ excludenl contains=ALL
syntax match jiraHeadings /h[1-6]\..*/ excludenl contains=ALL
syntax match jiraItalic /_[^_]\+_/ excludenl contains=ALL
syntax match jiraCodeInline /{{[^}]\+}}/ excludenl contains=ALL
syntax match jiraQuoteInline /bq\..*/ excludenl contains=ALL

syntax region jiraCode start="{code}" end="{code}" fold keepend
syntax region jiraQuote start="{quote}" end="{quote}" fold keepend
