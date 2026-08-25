" Ensure we are in a version of Vim that supports bracketed paste
if &term =~# 'xterm' || &term =~# 'alacritty'
  " Manually define the Paste keys if the terminal hasn't already
  if empty(&t_BE)
    let &t_BE = "\<Esc>[?2004h"
    let &t_BD = "\<Esc>[?2004l"
    let &t_PS = "\<Esc>[200~"
    let &t_PE = "\<Esc>[201~"
  endif

  " The Magic: Remap the 'Paste Start' sequence to break undo
  " <C-g>u = break undo chain
  " <PasteStart> = trigger Vim's internal 'paste' mode logic
  execute "inoremap <special> " . &t_PS . " <C-g>u" . &t_PS
endif
