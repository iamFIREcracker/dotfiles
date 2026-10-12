" Reload Windows Alacritty when one of its WSL-side config files changes.

if exists('g:loaded_alacritty_reload')
  finish
endif
let g:loaded_alacritty_reload = 1

if !exists('$WSL_DISTRO_NAME')
  finish
endif

function! s:AlacrittyReload() abort
  let l:file = resolve(expand('%:p'))
  let l:config = resolve(expand('~/.config/alacritty'))
  let l:dotfiles = resolve(expand('~/my-env/dotfiles/.config/alacritty'))

  if l:file !~# '^' . escape(l:config, '\') . '/'
        \ && l:file !~# '^' . escape(l:dotfiles, '\') . '/'
    return
  endif

  let l:appdata = trim(system(
        \ "cmd.exe /c 'echo %APPDATA%' 2>/dev/null | tr -d '\\r'"
        \ ))
  let l:appdata = trim(system('wslpath ' . shellescape(l:appdata)))

  for l:file in [
        \ 'alacritty.toml',
        \ 'editor.toml',
        \ 'llm.toml',
        \ 'plan.toml',
        \ 'start.toml',
        \ ]
    call system('touch ' . shellescape(l:appdata . '/alacritty/' . l:file))
  endfor
endfunction

augroup alacritty_reload
  autocmd!
  autocmd BufWritePost * call <SID>AlacrittyReload()
augroup END
