syntax region javaScriptTemplateString   start=+`+  skip=+\\\(`\|$\)+  end=+`+      keepend

hi link javaScriptTemplateString String

" XXX temporarily moved from ftplugin
RainbowParenthesesActivate
RainbowParenthesesLoadRound
RainbowParenthesesLoadSquare
RainbowParenthesesLoadBrace
