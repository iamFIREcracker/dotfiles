" Enable diff chunk folding -- https://github.com/tpope/vim-fugitive/issues/1714#issuecomment-806841493
setl foldmethod=syntax

" Worktrees {{{

command! GWorktrees call fzf#run({ 'source': 'ls -1 ..', 'sink': 'GWorktreeSelect'})
command! -nargs=1 GWorktreeSelect call GWorktreeSelect(<q-args>)

function! GWorktreeSelect(worktree) abort "{{{
    execute 'cd ../' . a:worktree
    echo 'Switched to [worktree=' . a:worktree . ']'
endfunction " }}}

" }}}
