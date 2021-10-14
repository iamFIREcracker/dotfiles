au BufNewFile,BufRead *.template.html,*.tpl.html,*.component.html
            \ if g:inside_angular_project == 1 |
            \   setf htmlangular |
            \ endif
