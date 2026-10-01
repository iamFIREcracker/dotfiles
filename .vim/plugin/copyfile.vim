if exists('g:loaded_copyfile')
  finish
endif
let g:loaded_copyfile = 1

function! s:CopyFile() abort
  if empty(expand('%'))
    echoerr 'CopyFile: the current buffer has no filename'
    return
  endif

  if !has('clipboard') && !has('clipboard_provider')
    echoerr 'CopyFile: Vim has no clipboard or clipboard-provider support'
    return
  endif

  let l:filename = expand('%:p')
  let l:content = join(getline(1, '$'), "\n")

  " Preserve the final newline when the buffer has one.
  if &endofline
    let l:content .= "\n"
  endif

  " Use a fence longer than any sequence of backticks in the file.
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
  let l:output = l:filename
        \ . "\n"
        \ . l:fence
        \ . "\n"
        \ . l:content
        \ . l:fence
        \ . "\n"

  call setreg('+', l:output)
  echo 'Copied ' . l:filename
endfunction

command! Copyfile call <SID>CopyFile()
