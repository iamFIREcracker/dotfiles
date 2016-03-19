highlight hide ctermfg=darkgrey
highlight jiraBold cterm=bold
highlight jiraHeadings ctermfg=magenta
highlight jiraItalic cterm=underline
highlight jiraCode cterm=italic
highlight jiraCodeInline cterm=italic
highlight jiraQuote ctermfg=darkgrey
highlight jiraQuoteInline ctermfg=darkgrey

syntax match jiraBold /\*[^*]\+\*/ excludenl contains=ALL
syntax match jiraHeadings /h[1-6]\..*/ excludenl contains=ALL
syntax match jiraItalic /_[^_]\+_/ excludenl contains=ALL
syntax match jiraCodeInline /{{[^}]\+}}/ excludenl contains=ALL
syntax match jiraQuoteInline /bq\..*/ excludenl contains=ALL

syntax region jiraCode matchgroup=hide start="{code}" end="{code}" fold keepend
syntax region jiraQuote matchgroup=hide start="{quote}" end="{quote}" fold keepend
