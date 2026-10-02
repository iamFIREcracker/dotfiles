if exists('g:loaded_yankfile')
  finish
endif
let g:loaded_yankfile = 1

" Render one file as a header line followed by a fenced block.  The fence is
" longer than any sequence of backticks in the content.
function! s:Block(header, lines) abort
  " Always terminate the last line, so the closing fence sits on its own line.
  let l:content = join(a:lines, "\n") . "\n"

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
  return a:header
        \ . "\n"
        \ . l:fence
        \ . "\n"
        \ . l:content
        \ . l:fence
        \ . "\n"
endfunction

" Without a range, yank the whole buffer.  With a range (e.g. from visual
" mode), yank only those lines, each prefixed with its line number.
function! s:YankBuffer(line1, line2, range) abort
  if empty(expand('%'))
    echoerr 'YankFile: the current buffer has no filename'
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

  call setreg('+', s:Block(l:header, l:lines))
  echo 'Yanked ' . l:header
endfunction

" Turn the netrw listing lines in the range into absolute paths.  Banner lines
" are skipped; tree indentation, type suffixes, symlink targets and long-listing
" columns are stripped.
function! s:NetrwPaths(line1, line2) abort
  let l:directory = substitute(b:netrw_curdir, '[\/]$', '', '')
  let l:paths = []

  for l:line in getline(a:line1, a:line2)
    if l:line =~# '^"' || l:line =~# '^\s*$'
      continue
    endif
    let l:name = substitute(l:line, '^\(| \)*', '', '')
    let l:name = substitute(l:name, '\t.*$', '', '')
    if get(w:, 'netrw_liststyle', get(g:, 'netrw_liststyle', 0)) == 1
      let l:name = substitute(l:name, '\s\+\d\+\s\+\S.*$', '', '')
    endif
    let l:name = substitute(l:name, '[/*|@=]$', '', '')
    call add(l:paths, l:directory . '/' . l:name)
  endfor

  return l:paths
endfunction

" In a netrw buffer, yank every marked file; failing that, the files on the
" lines in the range (the cursor line when no range was given).  Directories
" are skipped.
function! s:YankNetrw(line1, line2, range) abort
  let l:marked = netrw#Expose('netrwmarkfilelist')
  if type(l:marked) == v:t_list
    let l:paths = copy(l:marked)
  elseif a:range == 0
    let l:paths = s:NetrwPaths(line('.'), line('.'))
  else
    let l:paths = s:NetrwPaths(a:line1, a:line2)
  endif

  call filter(l:paths, '!isdirectory(v:val)')
  if empty(l:paths)
    echoerr 'YankFile: no files selected'
    return
  endif

  let l:blocks = []
  for l:path in l:paths
    if !filereadable(l:path)
      echoerr 'YankFile: cannot read ' . l:path
      return
    endif
    call add(l:blocks, s:Block(fnamemodify(l:path, ':p'), readfile(l:path)))
  endfor

  call setreg('+', join(l:blocks, "\n"))
  echo 'Yanked ' . len(l:paths) . ' file' . (len(l:paths) == 1 ? '' : 's')
endfunction

function! s:YankFile(line1, line2, range) abort
  if !has('clipboard') && !has('clipboard_provider')
    echoerr 'YankFile: Vim has no clipboard or clipboard-provider support'
    return
  endif

  if &filetype ==# 'netrw'
    call s:YankNetrw(a:line1, a:line2, a:range)
  else
    call s:YankBuffer(a:line1, a:line2, a:range)
  endif
endfunction

command! -range=% Yankfile call <SID>YankFile(<line1>, <line2>, <range>)
