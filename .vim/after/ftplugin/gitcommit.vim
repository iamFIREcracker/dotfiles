" Abbreviations {{{

call MakeSpacelessBufferIabbrev('smr',   'See merge request !HERE')

" }}}

" Add support for _proper_ indentation for bulleted lists
" https://superuser.com/questions/99138/bulleted-lists-for-plain-text-documents-in-vim
set formatlistpat=^\\s*[0-9*\\-]\\+[\\]:.)}\\t\ ]\\s*

" Enable diff chunk folding -- https://github.com/tpope/vim-fugitive/issues/146
set foldmethod=syntax
