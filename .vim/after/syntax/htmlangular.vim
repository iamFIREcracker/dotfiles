if exists("b:current_syntax")
    finish
endif

runtime! syntax/html.vim
let b:current_syntax = 'htmlangular'
