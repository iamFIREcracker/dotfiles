highlight hide                              ctermfg=darkgrey
highlight link jiraHeadings Title
highlight jiraBold      term=bold           ctermfg=2
highlight link jiraItalic   Underlined
highlight link jiraLink   Underlined
highlight jiraLinkName                      ctermfg=yellow
highlight jiraList                          ctermfg=cyan
highlight jiraCode       term=italic
highlight jiraCodeInline term=italic
highlight jiraQuote                         ctermfg=darkgrey
highlight jiraQuoteInline                   ctermfg=darkgrey

syntax match jiraHeadings       /\vh[1-6]\..*/        excludenl contains=ALL
syntax match jiraBold           /\v*(([^*]|*\w)+)\*/
syntax match jiraItalic         /\v_(([^_]|_\w)+)_/
syntax match jiraLink           /\[[^\]]\+\]/       excludenl contains=ALL
syntax match jiraLinkName       /\[\([^\]|]\+\)|/   excludenl contained
syntax match jiraList           /^[#*\-]* /         excludenl contains=ALL
syntax match jiraCodeInline     /{{[^}]\+}}/        excludenl contains=ALL
syntax match jiraQuoteInline    /bq\..*/            excludenl contains=ALL

syntax region jiraCode matchgroup=hide start="{code}" end="{code}" fold keepend
syntax region jiraQuote matchgroup=hide start="{quote}" end="{quote}" fold keepend
