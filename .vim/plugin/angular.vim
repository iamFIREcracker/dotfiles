function! CheckIfAngularProject() abort " {{{
    return filereadable(".angular-cli.json") || filereadable('angular.json')
endfunction " }}}
function! InitAngularMappings() abort " {{{
    nnoremap <localleader>Ns  :Dispatch! ng serve<cr>
    nnoremap <localleader>Ngc :Dispatch ng generate component --spec false<space>
    nnoremap <localleader>Ngd :Dispatch ng generate directive --spec false<space>
    nnoremap <localleader>Ngs :Dispatch ng generate service --spec false<space>
endfunction " }}}
au VimEnter *
            \ if CheckIfAngularProject()
            \ | call InitAngularMappings()
            \ | let g:inside_angular_project = 1
            \ | endif
