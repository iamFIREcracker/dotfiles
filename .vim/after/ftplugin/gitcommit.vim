" Abbreviations {{{

call MakeSpacelessBufferIabbrev('smr',   'See merge request !HERE')

" }}}

" Add support for _proper_ indentation for bulleted lists
" https://superuser.com/questions/99138/bulleted-lists-for-plain-text-documents-in-vim
setl formatlistpat=^\\s*[0-9*\\-]\\+[\\]:.)}\\t\ ]\\s*

" Enable diff chunk folding -- https://github.com/tpope/vim-fugitive/issues/146
setl foldmethod=syntax
setl foldlevel=1

setl spell
