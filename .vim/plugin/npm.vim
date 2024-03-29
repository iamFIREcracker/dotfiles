function! NpmTryInit() abort "{{{
    if CheckIfNpmProject()
        call InitNpmMappings()
    endif
endfunction " }}}

function! CheckIfNpmProject() abort "{{{
    return filereadable("package.json")
endfunction " }}}

function! InitNpmMappings() abort "{{{
    nnoremap <localleader>ni  :Dispatch npm install --save<space>
    nnoremap <localleader>nr  :Dispatch npm run<space>
    nnoremap <localleader>nn  :Dispatch npm<space>
endfunction " }}}

au VimEnter * call NpmTryInit()
