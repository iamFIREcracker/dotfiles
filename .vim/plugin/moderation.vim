" moderation.vim - approve a file's modelines before they are applied
" Maintainer: Matteo Landi

if exists('g:loaded_moderation') || &cp
  finish
endif
let g:loaded_moderation = 1

" Modelines are never applied silently: 'modeline' is enabled only for the
" instant an approved modeline is re-applied, in s:moderate() below.
set nomodeline

" Vim's own detection rule: 'vi:' or 'vim[<=>][NNN]:' at the start of the
" line or after whitespace, 'ex:' only after whitespace.
let s:pattern = '\v%(^|\s)%(vi|[vV]im[<=>]?\d*):|\sex:'

function! s:candidates() abort
  let l:lines = getline(1, 5) + getline(line('$') - 4, line('$'))
  let l:found = []
  for l:line in l:lines
    if l:line =~# s:pattern
      let l:line = substitute(l:line, '^\s*', '', '')
      if index(l:found, l:line) < 0
        call add(l:found, l:line)
      endif
    endif
  endfor
  return l:found
endfunction

function! s:moderate() abort
  if get(b:, 'moderation_processed', 0)
    return
  endif
  let b:moderation_processed = 1

  let l:modelines = s:candidates()
  if empty(l:modelines)
    return
  endif

  let l:msg = "Modeline detected:\n\n" . join(l:modelines, "\n") . "\n\nApply settings?"
  if confirm(l:msg, "&Yes\n&No", 2) == 1
    " Vim runs modelines after any :doautocmd, whatever the event; a private
    " event applies them without re-firing every BufRead handler.
    set modeline modelines=5
    doautocmd User Moderation
    set nomodeline modelines=0
  endif
endfunction

augroup moderation
  autocmd!
  autocmd User Moderation silent
  autocmd BufReadPost * call s:moderate()
augroup END
