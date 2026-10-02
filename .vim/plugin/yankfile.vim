if exists('g:loaded_yankfile')
  finish
endif
let g:loaded_yankfile = 1

" Without a range, yank the whole buffer.  With a range (e.g. from visual
" mode), yank only those lines, each prefixed with its line number.
function! s:YankFile(line1, line2, range) abort
  if empty(expand('%'))
    echoerr 'YankFile: the current buffer has no filename'
    return
  endif

  if !has('clipboard') && !has('clipboard_provider')
    echoerr 'YankFile: Vim has no clipboard or clipboard-provider support'
    return
  endif

  let l:filename = expand('%:p')

  if a:range == 0
    let l:header = l:filename
    let l:lines = getline(1, '$')
  else
    let l:header = l:filename . ':' . a:line1
    if a:line2 != a:line1
      let l:header .= '-' . a:line2
    endif
    let l:width = len(string(a:line2))
    let l:lines = map(getline(a:line1, a:line2),
          \ {i, line -> printf('%*d  %s', l:width, a:line1 + i, line)})
  endif

  " Always terminate the last line, so the closing fence sits on its own line.
  let l:content = join(l:lines, "\n") . "\n"

  " Use a fence longer than any sequence of backticks in the content.
  let l:longest = 0
  let l:current = 0

  for l:character in split(l:content, '\zs')
    if l:character ==# '`'
      let l:current += 1
      let l:longest = max([l:longest, l:current])
    else
      let l:current = 0
    endif
  endfor

  let l:fence = repeat('`', max([3, l:longest + 1]))
  let l:output = l:header
        \ . "\n"
        \ . l:fence
        \ . "\n"
        \ . l:content
        \ . l:fence
        \ . "\n"

  call setreg('+', l:output)
  echo 'Yanked ' . l:header
endfunction

command! -range=% Yankfile call <SID>YankFile(<line1>, <line2>, <range>)
