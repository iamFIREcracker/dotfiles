" Preamble ---------------------------------------------------------------- {{{

filetype off
filetype plugin indent on

" }}}
" Basic options ----------------------------------------------------------- {{{
set encoding=utf-8
set nomodeline
set modelines=0
set mouse=a
set autoindent
set showmode
set showcmd
set hidden
set visualbell
set ttyfast
set ruler
set backspace=indent,eol,start
set relativenumber
set laststatus=2
set history=1000
set undofile
set undoreload=10000
set list
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set lazyredraw
set matchtime=3
set showbreak=↪
set splitbelow
set splitright
set path=.,,

" Don't increment numbers in octal notation, please
set nrformats-=octal

" iTerm2 is currently slow as balls at rendering the nice unicode lines, so for
" " now I'll just use ASCII pipes.  They're ugly but at least I won't want to kill
" " myself when trying to move around a file.
" set fillchars=diff:⣿,vert:│
set fillchars=diff:⣿,vert:\|

" Don't try to highlight lines longer than 800 characters.
set synmaxcol=800

" Time out on key codes but not mappings.
" Basically this makes terminal Vim work sanely.
set notimeout
set ttimeout
set ttimeoutlen=10

set autowrite
set shiftround
set autoread
set title
set linebreak
set dictionary=/usr/share/dict/words
"set clipboard=unnamed
" Both Alacritty and Mintty support true colors
set termguicolors

" Make Vim able to edit crontab files again.
set backupskip=/tmp/*,/private/tmp/*"

" Wildmenu completion {{{

set wildmenu
set wildmode=list:longest

set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.spl                            " compiled spelling word lists
set wildignore+=*.sw?                            " Vim swap files
set wildignore+=*.DS_Store                       " OSX bullshit
set wildignore+=*.pyc                            " Python byte code
set wildignore+=*.orig                           " Merge resolution files
set wildignore+=*/node_modules/*                 " npm
set wildignore+=*.class                          " java
" Clojure/Leiningen
set wildignore+=classes
"set wildignore+=lib

" }}}
" Tabs, spaces, wrapping {{{

set tabstop=8
set shiftwidth=4
set softtabstop=4
set expandtab
set nowrap
set textwidth=79
set formatoptions=qrn1j
set colorcolumn=+1

" }}}
" Backups {{{

set undodir=~/.vim/tmp/undo//     " undo files
set backupdir=~/.vim/tmp/backup// " backups
set directory=~/.vim/tmp/swap//   " swap files
set backup                        " enable backups
set noswapfile                    " It's 2012, Vim.

" }}}
" Leader {{{

let mapleader = ","
let maplocalleader = "\\"

" }}}
" Color scheme {{{

syntax on

set background=dark
let g:badwolf_tabline = 2
let g:badwolf_html_link_underline = 0

augroup theme_customizations
    au!

    autocmd ColorScheme *
            \ syn match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$' |
            \ hi NeomakeErrorSign ctermfg=196 ctermbg=232 guifg=#ff2c4b guibg=#141413 |
            \ hi NeomakeWarningSign ctermfg=214 ctermbg=232 guifg=#ffa724 guibg=#141413 |
            \ hi link LspErrorText NeomakeErrorSign |
            \ hi link LspWarningText NeomakeWarningSign
    autocmd ColorScheme goodwolf
            \ hi! link diffFile diffIndexLine |
            \ hi! link diffOldFile diffIndexLine |
            \ hi! link diffNewFile diffIndexLine |
            \ hi! link diffLine diffSubName |
            \ hi! link DiffAdd diffAdded |
            \ hi! link DiffDelete diffRemoved |
            \ hi! link level2c level1c |
            \ hi! link javaDocTags Comment |
            \ hi! link shDerefSimple Comment |
            \ hi! link shDerefVar Comment |
            \ hi! link PreProc Comment |
            \ hi! link javaScriptEmbed javaScriptStringT |
            \ hi! markdownItalic cterm=italic |
            \ hi! link markdownH2 markdownH1delimiter |
            \ hi! link markdownH3 markdownH1delimiter |
            \ call GoodWolfHL('DiffText', 'orange', 'deepergravel', 'none')
augroup END

let g:badwolf_darkgutter = 1
colorscheme goodwolf

" }}}
" Autogroups {{{

" Save when losing focus {{{

augroup auto_save
    au!

    au FocusLost * :silent wa
augroup END

" }}}
" Cursorline {{{
" Only show cursorline in the current window and in normal mode.

augroup cline
    au!
    au VimEnter * set cursorline
    au WinLeave,InsertEnter * set nocursorline
    au WinEnter,InsertLeave * set cursorline
augroup END

" }}}
" cpoptions+=J, dammit {{{

" Something occasionally removes this.  If I manage to find it I'm going to
" comment out the line and replace all its characters with 'FUCK'.
augroup twospace
    au!
    au BufRead * :set cpoptions+=J
augroup END

" }}}
" Trailing whitespace {{{
" Only shown when not in insert mode so I don't go insane.

augroup trailing
    au!
    au InsertEnter * :set listchars-=trail:⌴
    au InsertLeave,WinLeave * :set listchars+=trail:⌴
augroup END

" }}}
" Fix relative filename completion {{{

augroup relative_fname_completion
    au!

    au InsertEnter * let save_cwd = getcwd() | set autochdir
    au InsertLeave,WinLeave * if exists('save_cwd') | set noautochdir | execute 'cd' fnameescape(save_cwd) | endif
augroup END

" }}}
" Line Return {{{

" Make sure Vim returns to the same line when you reopen a file.
" Thanks, Amit
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$")
        \       && &ft != 'gitcommit'
        \       |
        \     execute 'normal! g`"zvzz' |
        \ endif
augroup END

" }}}
" Keep folds closed while inserting text {{{

augroup sane_folds
    au!
    " Don't screw up folds when inserting text that might affect them, until
    " leaving insert mode. Foldmethod is local to the window. Protect against
    " screwing up folding when switching between windows.
    "
    " http://vim.wikia.com/wiki/Keep_folds_closed_while_inserting_text
    autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
    autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif
augroup END

" }}}

" }}}

" }}}
" Abbreviations ----------------------------------------------------------- {{{

function! EatChar(pat)
    let c = nr2char(getchar(0))
    return (c =~ a:pat) ? '' : c
endfunction

function! MakeSpacelessIabbrev(from, to)
    execute "inoreabbr <silent> ".a:from." ".a:to."<C-R>=EatChar('\\s')<CR>"
endfunction
function! MakeSpacelessBufferIabbrev(from, to)
    let cmd = "inoreabbr <silent> <buffer> ".a:from." ".a:to
    if a:to =~ 'HERE'
        let cmd .= "<ESC>".
            \ ":let search_active=v:hlsearch<CR>".
            \ "?HERE<CR>dw" .
            \ ":call histdel('search', -1)<CR>" .
            \ ":let @/ = histget('search', -1)<CR>" .
            \ ":if search_active == 0 <bar> noh <bar> endif<CR>" .
            \ "i"
    endif
    let cmd .= "<C-R>=EatChar('\\s')<CR>"

    execute cmd
endfunction

call MakeSpacelessIabbrev('ml/',  'http://matteolandi.net/')
call MakeSpacelessIabbrev('bb/',  'http://bitbucket.org/')
call MakeSpacelessIabbrev('bbm/', 'http://bitbucket.org/iamFIREcracker/')
call MakeSpacelessIabbrev('gh/',  'http://github.com/')
call MakeSpacelessIabbrev('ghm/', 'http://github.com/iamFIREcracker/')

inoreabbr Mateo Matteo
inoreabbr lenght length
inoreabbr mainteinance maintainance
inoreabbr ml@ matteo@matteolandi.net

" }}}
" Searching and movement -------------------------------------------------- {{{

" Use sane regexes.
nnoremap / /\v
vnoremap / /\v
nnoremap ? ?\v
vnoremap ? ?\v

set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
set gdefault

set scrolloff=5
set sidescroll=1
set sidescrolloff=10

set virtualedit+=block

noremap <silent> <leader><space> :noh<cr>:call clearmatches()<cr>

runtime macros/matchit.vim

" Keep search matches in the middle of the window
nnoremap n nzvzz
nnoremap N Nzvzz

" Same when jumping around
nnoremap g; g;zvzz
nnoremap g, g,zvzz
nnoremap gd gdzvzz
nnoremap gD gDzvzz

" Easier to type, and I never use the default behavior.
noremap H ^
noremap L g_

" Heresy
inoremap <c-a> <c-o>^
inoremap <c-e> <c-o>$

" Open a Quickfix window for the last search.
nnoremap <silent> <leader>/ :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

" Fix linewise visual selection of various text objects
nnoremap VV V
nnoremap Vit vitVkoj
nnoremap Vat vatV
nnoremap Vab vabV
nnoremap VaB vaBV

" Smarcase for */# -- and don't automatically jump around {{{

function! NormalStarSearchSet() " {{{
    let query = "\\<" . escape(expand('<cword>'), '*\') . "\\>"
    let @/ = query
    call histadd("search", query)
endfunction " }}}

" `v:searchforward` and `hlsearch` are reset after a function terminate, so we
" are forced to set them **after** we figured out what to search
"
" Read `:help v:searchforward` for additional details
nnoremap * :<C-U>call NormalStarSearchSet()<CR>
            \ :let v:searchforward = 1<CR>
            \ :set hlsearch<CR>
nnoremap # :<C-U>call NormalStarSearchSet()<CR>
            \ :let v:searchforward = 0<CR>
            \ :set hlsearch<CR>

"" }}}
" Directional Keys {{{

" It's 2011.
noremap <silent> <expr> j (v:count == 0 ? 'gj' : 'j')
noremap <silent> <expr> k (v:count == 0 ? 'gk' : 'k')

" }}}
" Some terminal mappings {{{

if has('terminal')
    " Exit terminal mode like, seamlessly
    " this completely fucks up REPLs..
    " tnoremap <Esc> <C-\><C-N>

    " Verbatim
    tnoremap <C-v><C-w> <C-W>.
endif

" }}}

" }}}
" Folding ----------------------------------------------------------------- {{{

set foldlevelstart=0

" Space to toggle folds.
nnoremap <space> za
vnoremap <space> za

" "Focus" the current line.  Basically:
"
" 1. Close all folds.
" 2. Open just the folds containing the current line.
" 3. Move the line to a little bit (15 lines) above the center of the screen.
"
" This mapping wipes out the z mark, which I never use.
"
" I use :sus for the rare times I want to actually background Vim.
nnoremap <expr> <Plug>FocusCurrentLine 'mzzMzvzz15<c-e>`z'
nmap <leader>z <Plug>FocusCurrentLine

function! MyFoldText() " {{{
    let line = getline(v:foldstart)

    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart

    " expand tabs into spaces
    let onetab = strpart('          ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')

    let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
    let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
    return line . '…' . repeat(" ",fillcharcount) . foldedlinecount . '…' . ' '
endfunction " }}}
set foldtext=MyFoldText()

" }}}
" Destroy infuriating keys ------------------------------------------------ {{{

" Fuck you, help key.
noremap  <F1> <nop>
inoremap <F1> <nop>
"
" Stop it, hash key.
inoremap # X<BS>#

" Fuck you too, manual key.
nnoremap K <nop>

" }}}
" Various filetype-specific stuff ----------------------------------------- {{{

" https://www.reddit.com/r/vim/comments/3hwall/how_to_close_vim_when_last_buffer_is_deleted/cub629z/
function! CloseOnLast()
    let cnt = 0

    for i in range(0, bufnr("$"))
        if buflisted(i)
            let cnt += 1
        endif
    endfor

    if cnt <= 1
        q
    else
        bw
    endif
 endfunction

" Angular {{{

augroup ft_angular
    au!

    function! CheckIfAngularProject() abort " {{{
        return filereadable(".angular-cli.json") || filereadable('angular.json')
    endfunction " }}}
    function! InitAngularMappings() abort " {{{
        nnoremap <localleader>Ns  :Dispatch! ng serve<cr>
        nnoremap <localleader>Ngc :Dispatch ng generate component --spec false<space>
        nnoremap <localleader>Ngd :Dispatch ng generate directive --spec false<space>
        nnoremap <localleader>Ngs :Dispatch ng generate service --spec false<space>
    endfunction " }}}
    au VimEnter *
                \ if CheckIfAngularProject()
                \ | call InitAngularMappings()
                \ | endif

    au BufNewFile,BufRead *.template.html,*.tpl.html,*.component.html
                \ if CheckIfAngularProject()
                \ | setlocal filetype=angular_html
                \ | endif

    function! DirectiveUnderCursor() abort " {{{
        let directive = expand('<cWORD>')
        if directive =~ '='
            let directive = split(directive, '=')[0]
        endif
        if directive =~ '>'
            let directive = split(directive, '>')[0]
        endif
        let directive = substitute(directive, '<\|/\|>', '', 'g')
        return directive
    endfunction " }}}
    function! FindDirectiveOccurrences() abort " {{{
        let directive = DirectiveUnderCursor()

        exe "normal! :Ack! --literal " . directive . "\<cr>"
    endfunction "}}}
    function! GuessDirectiveFilename() abort " {{{
        let inex_save = &inex

        function! FilenameOfDirectiveUnderCursor() abort " {{{
            let directive = DirectiveUnderCursor()
            let filename = substitute(directive, '-', '', 'g')
            return filename
        endfunction  " }}}

        set includeexpr=FilenameOfDirectiveUnderCursor()
        normal! gf

        let &inex = inex_save
    endfunction "}}}
    au Filetype angular_html setlocal suffixesadd+=.js path+=**
    au Filetype angular_html nnoremap <buffer> <C-^> :call FindDirectiveOccurrences()<cr>
    au Filetype angular_html nnoremap <buffer> gf :call GuessDirectiveFilename()<cr>

augroup END

" }}}
" Autohotkey {{{

augroup ft_autohotkey
  au!

  autocmd FileType autohotkey setlocal commentstring=;\ %s
augroup END

" }}}
" Blogger {{{

augroup ft_blogger
    au!

    au BufNewFile,BufRead *.blogger setlocal filetype=blogger
    au Filetype blogger setlocal filetype=markdown

    au Filetype blogger setlocal spell
    au Filetype blogger setlocal norelativenumber

    " Markdown the current file and pipe the output to bcat/browser
    au Filetype blogger nnoremap <buffer> <localleader>p :!markdown % \| browser<cr>

    " Markdown the current file, prepare it for Blogger, and finally copy it to
    " clipboard
    function! GenerateTransformAndCopy() abort " {{{
        !markdown %
            \ | sed
            \       -e "s:\(</*h\)1\(>\):\14\2:g"
            \       -e "s:\(</*h\)2\(>\):\15\2:g"
            \       -e "s:\(</*h\)3\(>\):\16\2:g"
            \ | xclip -selection clip-board
    endfunction " }}}
    au Filetype blogger nnoremap <buffer> <localleader>c :call GenerateTransformAndCopy()<cr>
augroup END


" }}}
" C {{{

augroup ft_c
    au!
    au FileType c setlocal foldmethod=syntax
augroup END

" }}}
" Common Lisp {{{

augroup ft_commonlisp
    au!

    au BufNewFile,BufRead *.sbclrc setlocal filetype=lisp
    au BufNewFile,BufRead *.cgrc setlocal filetype=lisp
    au BufNewFile,BufRead *.lispwords setlocal filetype=lisp
    au BufNewFile,BufRead *.asd setlocal filetype=lisp

    function! HighlightLispRepl() abort " {{{
        " set syntax=lisp
        syn match replPrompt /\v^\[([a-z A-Z])+\] [-._a-zA-Z0-9]+\>/
        syn match replComment /\v^;.*/

        " syn match replResult /\v^#\<[^>]+\>$/
        hi def link replResult Debug
        hi def link replComment Comment
    endfunction "}}}

    function! InitializeLispRepl() abort "{{{
        call HighlightLispRepl()
    endfunction "}}}

    function! OpenLispReplSBCL() abort "{{{
        call term_start("bash -c sbcl-vlime", {
            \ "term_finish": "close",
            \ "vertical": 1
        \ })
        call InitializeLispRepl()
    endfunction "}}}

    function! OpenLispReplPrompt() abort "{{{
        call term_start("bash -c " . input("? "), {
            \ "term_finish": "close",
            \ "vertical": 1
        \ })
        call InitializeLispRepl()
    endfunction "}}}

    function! MotionToSelectTopLevelLispForm() abort " {{{
        let motion = '99[(v%'
        if col('.') == 1 && getline('.')[col('.') - 1] == '('
            " We are at the beginning of a top-level form, so select until
            " the matching parenthesis
            let motion = 'v%'
        endif
        return motion
    endfunction "}}}

    function! SelectToplevelLispFormAndSendToTerminal() abort "{{{
        call SelectAndSendToTerminal(MotionToSelectTopLevelLispForm())
    endfunction "}}}

    function! SelectToplevelLispFormAndSendToVlimeREPL() abort "{{{
        let view = winsaveview()
        let reg_save = @@
        let motion = MotionToSelectTopLevelLispForm()

        execute "normal! " . motion . "y"
        call vlime#plugin#SendToREPL(@@)

        let @@ = reg_save
        call winrestview(view)
    endfunction "}}}

    function! IndentToplevelLispForm() abort "{{{
        let view = winsaveview()
        let reg_save = @@
        let motion = MotionToSelectTopLevelLispForm()

        execute "normal! " . motion . "="

        let @@ = reg_save
        call winrestview(view)
    endfunction "}}}

    function! QuickprojectMakePrompt() abort "{{{
        let l:path = getcwd()
        let l:guessed_name = split(l:path, '/')[-1]
        let l:name = input("? ", guessed_name)

        call SendToTerminal("(quickproject:make-project #p\"" . l:path . "\" :name \"" . l:name . "\")\n")
    endfunction " }}}

    function! QuickloadLispSystem() abort "{{{
        let systems = split(system('ls -1 *.asd | grep -v test | cut -d. -f1 | uniq')) " its fine
        if len(systems) == 0
            echom "Could not find any .asd files..."
            return
        elseif len(systems) > 1
            echom "Found too many .asd files..."
            return
        endif

        call SendToTerminal("(ql:quickload :" . systems[0] . ")")
    endfunction " }}}

    function! QuickloadLispPrompt() abort "{{{
        call SendToTerminal("(ql:quickload :" . input("? ") . ")\n")
    endfunction " }}}

    function! TestLispSystem() abort "{{{
        let systems = split(system('ls -1 *.asd | grep -v test | cut -d. -f1 | uniq')) " its fine
        if len(systems) == 0
            echom "Could not find any .asd files..."
            return
        elseif len(systems) > 1
            echom "Found too many .asd files..."
            return
        endif

        call SendToTerminal("(asdf:test-system :" . systems[0] . ")")
    endfunction " }}}

    function! TestLispPrompt() abort "{{{
        call SendToTerminal("(asdf:test-system :" . input("? ") . ")\n")
    endfunction " }}}

    function! InPackage() abort "{{{
        let packages = split(system('grep "(in-package .*" '. fnameescape(expand("%")) .' --only-matching | cut -d" " -f2 | uniq')) " its fine
        if len(packages) == 0
            echom "Could not find any defpackage lines..."
            return
        elseif len(packages) > 1
            echom "Found too many defpackage lines..."
            return
        endif

        call SendToTerminal("(in-package " . packages[0])
    endfunction " }}}

    au FileType lisp setlocal iskeyword+=!,?,%,-
    au FileType lisp setlocal suffixesadd+=.lisp

    au FileType lisp RainbowParenthesesActivate
    au syntax lisp RainbowParenthesesLoadRound

    " Fix windows:
    " - <c-w>j: select window below -- Vlime Connection
    " - <c-w>J: move it to the far bottom (and expand horizontally)
    " - <c-w>k: select window above --  the actual lisp buffer
    " - <c-w>H: move it to the far right (and expand vertically)
    au FileType lisp nnoremap <buffer> <localleader>W <c-w>j<c-w>J<c-w>k<c-w>H

    au FileType lisp let b:delimitMate_quotes = "\""

    " Force omnicompletion (vlime's)
    au FileType lisp inoremap <c-n> <c-x><c-o>

    au FileType lisp nnoremap <buffer> <silent> <localleader>o :call OpenLispReplSBCL()<cr>
    au FileType lisp nnoremap <buffer> <silent> <localleader>O :call OpenLispReplPrompt()<cr>
    au FileType lisp nnoremap <buffer> <silent> gI :<C-U>call IndentToplevelLispForm()<cr>
    au FileType lisp nnoremap <buffer> <silent> <localleader>n :call QuickprojectMakePrompt()<cr>
    au FileType lisp nnoremap <buffer> <silent> <localleader>q :call QuickloadLispSystem()<cr>
    au FileType lisp nnoremap <buffer> <silent> <localleader>Q :call QuickloadLispPrompt()<cr>
    au FileType lisp nnoremap <buffer> <silent> <localleader>t :call TestLispSystem()<cr>
    au FileType lisp nnoremap <buffer> <silent> <localleader>T :call TestLispPrompt()<cr>
    if !exists('b:vlime_mappings_unmapped')
        let b:vlime_mappings_unmapped=1
        au FileType lisp silent! unmap <buffer> <localleader>of
        au FileType lisp silent! unmap <buffer> <localleader>ot
        au FileType lisp silent! unmap <buffer> <localleader>oe
        au FileType lisp silent! unmap <buffer> <localleader>Tt
        au FileType lisp silent! unmap <buffer> <localleader>TT
        au FileType lisp silent! unmap <buffer> <localleader>Ti
        au FileType lisp silent! unmap <buffer> <localleader>TI
        au FileType lisp silent! unmap <buffer> <localleader>Td
        au FileType lisp silent! unmap <buffer> <localleader>TD
    endif
    au FileType lisp nnoremap <buffer> <silent> <localleader>i :call InPackage()<cr>
    au FileType lisp nnoremap <buffer> <silent> gs :call SelectToplevelLispFormAndSendToTerminal()<cr>
    au FileType lisp xnoremap <buffer> <silent> gs :call SendSelectionToTerminal(visualmode())<cr>

    " Vlime's send-top-level-s-expression mapping is not always working as
    " expected -- it seems it does not properly handle comments that include
    " s-expressions -- so what we are doing here:
    "
    " 1) select the top-level expression, manually
    " 2) send it
    au FileType lisp nnoremap <buffer> <silent> <C-S> :<C-U>call SelectToplevelLispFormAndSendToVlimeREPL()<CR>
    au FileType lisp xmap <buffer> <silent> <C-S> <localleader>s

    au FileType lisp nmap <buffer> <silent> K <localleader>ddo
augroup END

" }}}
" CSS, SCSS, and LessCSS {{{

augroup ft_css
    au!

    au BufNewFile,BufRead *.less setlocal filetype=less

    au Filetype less,css,scss setlocal foldmethod=marker
    au Filetype less,css,scss setlocal foldmarker={,}
    au Filetype less,css,scss setlocal omnifunc=csscomplete#CompleteCSS
    au Filetype less,css,scss setlocal iskeyword+=-

    " Use <leader>S to sort properties.  Turns this:
    "
    "     p {
    "         width: 200px;
    "         height: 100px;
    "         background: red;
    "
    "         ...
    "     }
    "
    " into this:

    "     p {
    "         background: red;
    "         height: 100px;
    "         width: 200px;
    "
    "         ...
    "     }
    au BufNewFile,BufRead *.less,*.css,*.scss nnoremap <buffer> <localleader>S ?{<CR>jV/\v^\s*\}?$<CR>k:sort<CR>:noh<CR>
augroup END

" }}}
" DadBod {{{

augroup ft_dadbod
    au!

    function! DadbodInit() abort
        let line = getline(".")

        if line =~ ';DB '
            execute line[1:]
            echo 'Loaded: ' . line[1:]
        else
            echohl ErrorMsg
            echo 'Line not starting with `;DB `: ' . line
            echohl NONE
        endif
    endfunction

    au BufNewFile,BufRead *.db* nnoremap <buffer> <silent> <localleader>cc :call DadbodInit()<cr>
    au BufNewFile,BufRead *.db* nmap <buffer> <silent> <C-S> vap:DB w:db<cr>
    au BufNewFile,BufRead *.db* xmap <buffer> <silent> <C-S> :DB w:db<cr>
augroup END

" }}}
" Diff {{{

augroup ft_diff
    au!

    function! DiffFold(lnum) abort
        let line = getline(a:lnum)
        if line =~ '^diff '
            return '>1'
        elseif line =~ '^\(index\|---\|+++\) '
            return '='
        elseif line =~ '^\(@@\) '
            return '>2'
        elseif line[0] =~ '[-+ ]'
            return '='
        else
            return 0
        endif
    endfunction

    au FileType diff setlocal foldmethod=expr foldexpr=DiffFold(v:lnum)
    au Filetype diff setlocal nolist
    au Filetype diff nnoremap <buffer> q :q<cr>

    " Diffs are folded by changes, so let's use [c]c to move to the
    " next/previous fold (ie. change)
    au Filetype diff nmap <buffer> [c [z
    au Filetype diff nmap <buffer> ]c ]z
    au Filetype diff nmap <buffer> <down> ]z
    au Filetype diff nmap <buffer> <up> [z
augroup END

" }}}
" Eslintrc {{{

augroup ft_eslintrc
    au!

    au BufNewFile,BufRead .eslintrc nnoremap <buffer> q :call CloseOnLast()<cr>
augroup END

" }}}
" HTML {{{

augroup ft_html
    au!

    au BufNewFile,BufRead *.html setlocal filetype=htmldjango
    au FileType html,jinja,htmldjango setlocal foldmethod=manual
    au FileType html,jinja,htmldjango EmmetInstall

    " Invoke emmet instead of supertab
    au FileType html,jinja,htmldjango imap <buffer> <expr> <c-n> emmet#expandAbbrIntelligent("\<c-n>")

    function! HtmlWrap() abort "{{{
        " Back up x registry
        let old_x = @x

        " Copy html element to x registry
        normal! vi<"xy

        if len(split(@x, '\n', 0)) != 1
            " Unwrap
            normal! vi<J
        else
            " Wrap:
            "
            " \s+ - any leading whitespace (there has to be whitespace!),
            "       otherwise we are going to end up splitting properties like:
            "
            "           attr.href="/?query=foo&bar=bax"
            "
            " ( ... ) - the first group of matched text -- for the substitution
            "     \* - a literal asterisk, used by Angular
            "     \[ ... \]  - literal square brackets, used by Angular
            "     \( ... \)  - literal braces, used by Angular
            "     (\w|-|\.)+ - multiple word character, or hypen, or .
            " ( ... ) - the second group of matched text
            "     (\=|$) - literal =, or end of line -- e.g. ng-onload, as well
            "                                           as allowFullscreen
            let @x = substitute(@x, '\v\s+(\*?\[?\(?(\w|-|\.)+\)?\]?)\=', '\n\1\=', 'g')

            " Replace selection with the modified content
            normal! gv"xp

            " Re-indent the new content
            normal! gv=
        endif

        " Restore x registry
        let @x = old_x
    endfunction " }}}
    au FileType html,jinja,htmldjango nnoremap <buffer> gw :call HtmlWrap()<cr>

    " Use <localleader>f to fold the current tag.
    au FileType html,jinja,htmldjango nnoremap <buffer> <localleader>f Vatzf

    " Indent tag
    au FileType html,jinja,htmldjango nnoremap <buffer> <localleader>= Vat=

    " Use Shift-Return (Ø) in insert mode to turn this:
    "     <tag>|</tag>
    "
    " into this:
    "     <tag>
    "         |
    "     </tag>
    au FileType html,jinja,htmldjango inoremap <buffer> Ø <esc>cit<cr><esc>ko
    " Use Shift-Return (Ø) in normal mode to turn this:
    "     <tag>something|else</tag>
    "
    " into this:
    "     <tag>
    "         |something else
    "     </tag>
    au FileType html,jinja,htmldjango nnoremap <buffer> Ø <esc>vit<esc>a<cr><esc>vito<esc>i<cr><esc>
augroup END

" }}}
" Grunt-ion {{{

augroup ft_gruntion
    au!

    function! DetectGruntIon() abort
        let moduleinfo = join([getcwd(), 'moduleInfo.ts'], '/')
        if filereadable(moduleinfo)
            " Override default dispatch command
            execute "Focus -compiler=grunt-ion " .
                        \ fnameescape(globpath(&runtimepath, 'compiler/grunt-ion.py')) .
                        \ " debug --no-color"

            " Run tests
            execute "nnoremap <leader>t :Dispatch -compiler=grunt-ion " .
                    \ fnameescape(globpath(&runtimepath, 'compiler/grunt-ion.py')) .
                    \ " test --no-color<cr>"
        endif
    endfunction

    autocmd VimEnter * call DetectGruntIon()
augroup end

" }}}
" Java {{{

augroup ft_java
    au!
    function! TurnOnJavaFolding() abort "{{{
        let modifier     = '%(public|private|protected)?\s*'
        let static       = '%(static\s*)?\s*'
        let returntype   = '%(,\s|\S)+\s*'
        let class        = modifier.'class%(\s+\S+)*\s*\{'
        " XXX it doesn't look like the following ignores if/for/while/switch statements correctly...
        let method       = modifier.static.returntype.'%(\S*\.\S*|if|for|while|switch)@![a-zA-Z0-9]+\s*\([^)]*\)\s*\{'
        let functionwrap = '\s*\S*\s[a-zA-Z]*\)\s*\{'

        let folded_statements = [
                    \ class,
                    \ method,
                    \ functionwrap
                    \ ]

        let b:manual_regexp_folding_statements_re_bare = '\v^\s*%(' . join(folded_statements, '|') . ')\s*$'
        call TurnOnManualRegexpFolding()
    endfunction "}}}
    au FileType java silent! call TurnOnJavaFolding()
    au FileType java silent! call RefreshManualRegexpFolding()

    au FileType java setlocal omnifunc=javacomplete#Complete

    au FileType java let b:rbpt_max=2
    au FileType java RainbowParenthesesActivate
    au syntax java RainbowParenthesesLoadRound
    au syntax java RainbowParenthesesLoadSquare
    au syntax java RainbowParenthesesLoadBrace

    au FileType java inoremap <c-n> <c-x><c-o>

    " Abbreviations {{{

    au FileType java call MakeSpacelessBufferIabbrev('if',      'if (HERE)')
    au FileType java call MakeSpacelessBufferIabbrev('rt',      'return HERE;')
    au FileType java call MakeSpacelessBufferIabbrev('for',     'for (HERE) {}<left><cr>')
    au FileType java call MakeSpacelessBufferIabbrev('while',   'while (HERE) {}<left><cr>')
    au FileType java call MakeSpacelessBufferIabbrev('println', 'System.out.println(HERE);')

    " }}}
augroup END

" }}}
" Javascript {{{

augroup ft_javascript
    au!

    function! TurnOnJavascriptFolding() abort "{{{
        let export       = '%(module\.)?export(s)?%(\.)?.*\{'
        let class        = 'class%(\s+\S+)*\s*\{'
        let method       = '%(static |async )?%(\S*\.\S*|if|for|switch)@!\S+\s*\([^)]*\)\s*\{'
        let functionwrap = '\s*[a-zA-Z0-9:]*\S*\)\s*\{'
        let functiondec  = '%(async )?function%(\s+\S+)?\s*\([^)]*' . functionwrap
        let functiondef  = '%(%(const|var|let)\s)?\S+\s*\=\s*' . functiondec
        let arrowdefwrap = '\s*[a-zA-Z0-9:]*\)\s*\=\>\s*\{'
        let arrowdef     = '%(%(const|var|let)\s)?\S+\s*\=\s*\([^)]*' . arrowdefwrap
        let router       = 'router\.\S+\([^}]*\{'
        let mocha_descr  = 'describe%(\.only)?\([^}]*\{'
        let mocha_it     = 'it\([^}]*\{'

        let folded_statements = [
                    \ export,
                    \ class,
                    \ method,
                    \ functionwrap,
                    \ functiondec,
                    \ functiondef,
                    \ arrowdefwrap,
                    \ arrowdef,
                    \ router,
                    \ mocha_descr,
                    \ mocha_it
                    \ ]

        let b:manual_regexp_folding_statements_re_bare = '\v^\s*%(' . join(folded_statements, '|') . ')\s*$'
        call TurnOnManualRegexpFolding()
    endfunction "}}}
    au FileType javascript silent! call TurnOnJavascriptFolding()
    au FileType javascript silent! call RefreshManualRegexpFolding()

    au FileType javascript setlocal suffixesadd+=.js,.ts

    au FileType javascript nnoremap <localleader>cc :STTConnect
    au FileType javascript nnoremap <localleader>cd :STTDisconnect
    au FileType javascript nnoremap <C-S> :<C-U>call SelectAndSendToTerminal('vap')<cr>
    au FileType javascript xnoremap <C-S> :<C-U>call SendSelectionToTerminal(visualmode())<cr>
    au Filetype javascript nnoremap <buffer> <C-^> :LspReferences<cr>
    au FileType javascript nnoremap <buffer> <silent> <C-]> :LspDefinition<cr>zvzz
    au FileType javascript nnoremap <buffer> <silent> gd :LspDefinition<cr>zvzz
    au FileType javascript nnoremap <buffer> <silent> ,S :LspRename<cr>
    au FileType javascript nnoremap <buffer> <silent> ◊ :LspCodeAction<cr>
    au FileType javascript nnoremap <buffer> <silent> K :LspHover<cr>
    au FileType javascript setlocal omnifunc=lsp#complete

    au FileType javascript inoremap <c-n> <c-x><c-o>

    au FileType javascript RainbowParenthesesActivate
    au syntax javascript RainbowParenthesesLoadRound
    au syntax javascript RainbowParenthesesLoadSquare
    au syntax javascript RainbowParenthesesLoadBrace

    " Abbreviations {{{

    au FileType javascript call MakeSpacelessBufferIabbrev('if',   'if (HERE)')
    au FileType javascript call MakeSpacelessBufferIabbrev('fn',   'function ')
    au FileType javascript call MakeSpacelessBufferIabbrev('afn',  'function(HERE)')
    au FileType javascript call MakeSpacelessBufferIabbrev('rt',   'return HERE;')
    au FileType javascript call MakeSpacelessBufferIabbrev('clog', 'console.log(HERE);')
    au FileType javascript call MakeSpacelessBufferIabbrev('cerr', 'console.error(HERE);')
    au FileType javascript call MakeSpacelessBufferIabbrev('pclog', 'console.log(JSON.stringify(HERE, null, 2));')
    au FileType javascript call MakeSpacelessBufferIabbrev('dolog', 'do(console.log)')
    au FileType javascript call MakeSpacelessBufferIabbrev('maplog', 'map(e => console.log(e) \|\| e)')
    au FileType javascript call MakeSpacelessBufferIabbrev('thenlog', 'then(e => console.log(e) \|\| e)')
    au FileType javascript call MakeSpacelessBufferIabbrev('desc', "describe('HERE', () => {});<left><left><left><cr>")
    au FileType javascript call MakeSpacelessBufferIabbrev('befeach', "beforeEach(() => {});<left><left><left><cr>")
    au FileType javascript call MakeSpacelessBufferIabbrev('itt', "it('HERE', () => {});<left><left><left><cr>")

    " }}}
augroup END

" }}}
" Jira {{{

augroup ft_jira
    au!

    au FileType jira setlocal wrap foldmethod=syntax
augroup END

" }}}
" JSON {{{

augroup ft_json
    au!

    au FileType json setlocal foldmethod=marker
    au FileType json setlocal foldmarker={,}
    au FileType json setlocal foldnestmax=2
augroup END

" }}}
" Mail {{{

augroup ft_mail
    au!

    function! EnableMailProfile() abort "{{{
        let first_line = getline('1')
        if first_line =~? '\vFrom:.*\<matteo\@matteolandi.net\>'
            doautocmd User MailProfilePersonal
        else
            doautocmd User MailProfileWork
        endif
    endfunction " }}}
    au FileType mail call EnableMailProfile()
    au FileType mail setlocal nobackup noswapfile nowritebackup

    function! EnableFormatFlowed() abort "{{{ https://rinzewind.org/blog-en/2017/a-small-trick-for-sending-flowed-mail-in-mutt-with-vim.html
        setlocal textwidth=72
        setlocal formatoptions=watqc
        setlocal nojs
        match ErrorMsg '\s\+$'
    endfunction " }}}
    au User MailProfilePersonal call EnableFormatFlowed()
    au User MailProfilePersonal let b:goobookprg='goobook'
    au User MailProfileWork setlocal textwidth=0 wrap
    au User MailProfileWork let b:goobookprg='aadbook'
augroup END

" }}}
" Markdown {{{

augroup ft_markdown
    au!

    au Filetype markdown setlocal spell
    au FileType markdown let b:delimitMate_nesting_quotes = ['`']

    au FileType markdown nnoremap <localleader>cc :STTConnect
    au FileType markdown nnoremap <localleader>cd :STTDisconnect
    au FileType markdown nnoremap <C-S> :<C-U>call SelectAndSendToTerminal('vap')<cr>
    au FileType markdown xnoremap <C-S> :<C-U>call SendSelectionToTerminal(visualmode())<cr>

    " Use <localleader>1/2/3 to add headings.
    au Filetype markdown nnoremap <buffer> <localleader>1 yypVr=
    au Filetype markdown nnoremap <buffer> <localleader>2 yypVr-
    au Filetype markdown nnoremap <buffer> <localleader>3 yypVr+
    au Filetype markdown nnoremap <buffer> <localleader>4 yypVr*

augroup END

" }}}
" Maven {{{

augroup ft_mvn
    au!

    function! CheckIfMvnProject() abort "{{{
        return filereadable("pom.xml") && !filereadable('package.json')
    endfunction " }}}
    function! InitMvnMappings() abort "{{{
        nnoremap <localleader>m  :Dispatch mvn -B<space>
    endfunction " }}}
    au VimEnter *
                \ if CheckIfMvnProject()
                \ | call InitMvnMappings()
                \ | compiler maven
                \ | let dispatch = 'mvn -B clean test'
                \ | endif
augroup END

" }}}
" Mutt {{{

augroup ft_muttrc
    au!

    au BufRead,BufNewFile *.muttrc set filetype=muttrc

    au FileType muttrc setlocal foldmethod=marker foldmarker={{{,}}}
augroup END

" }}}
" Npm {{{

augroup ft_npm
    au!

    function! CheckIfNpmProject() abort "{{{
        return filereadable("package.json")
    endfunction " }}}
    function! OpenNodeRepl() abort "{{{
        call term_start("bash -c node-rlwrap", {
                    \ "term_finish": "close",
                    \ "vertical": 1
                    \ })
    endfunction "}}}
    function! InitNpmMappings() abort "{{{
        nnoremap <localleader>ni  :Dispatch npm install --save<space>
        nnoremap <localleader>nr  :Dispatch npm run<space>
        nnoremap <localleader>nn  :Dispatch npm<space>

        nnoremap <silent> <localleader>o :call OpenNodeRepl()<cr>
    endfunction " }}}
    au VimEnter *
                \ if CheckIfNpmProject()
                \ | call InitNpmMappings()
                \ | endif

augroup END

" }}}
" Pentadacty {{{

augroup ft_pentadactyl
    au!

    au FileType pentadactyl setlocal foldmethod=marker
    au FileType pentadactyl setlocal foldmarker={{{,}}}
augroup END

" }}}
" plan {{{

augroup ft_plan
    au!

    au FileType plan nnoremap <localleader>cc :STTConnect
    au FileType plan nnoremap <localleader>cd :STTDisconnect
    au FileType plan nnoremap <C-S> :<C-U>call SelectAndSendToTerminal('vap')<cr>
    au FileType plan xnoremap <C-S> :<C-U>call SendSelectionToTerminal(visualmode())<cr>

    au FileType plan setlocal wrap textwidth=0
    function! s:CreateNewPlanEntry() abort "{{{
        let l:last_ai=&autoindent
        setlocal noautoindent

        " Go to the end of the file, and add a new line
        " Create additional empty line
        " Create the damn entry
        execute "normal! Go\<cr># " . strftime("%Y-%m-%d")

        let &l:autoindent=l:last_ai
    endfunction "}}}
    au FileType plan nnoremap <localleader>n :<C-U>call <SID>CreateNewPlanEntry()<cr>o
    au FileType plan nnoremap <localleader>o :silent lgrep '^\?' %<cr>:lopen<cr>:redraw!<cr>
augroup END

" }}}
" PHP {{{

augroup ft_php
    au!

    au FileType php setlocal foldmethod=marker
    au FileType php setlocal foldmarker={,}
augroup END

" }}}
" Puppet {{{

augroup ft_puppet
    au!

    au Filetype puppet setlocal foldmethod=marker
    au Filetype puppet setlocal foldmarker={,}
augroup END

" }}}
" Python {{{

augroup ft_python
    au!

    au FileType python let b:stt_trailing_new_lines = 2

    au FileType python setlocal define=^\s*\\(def\\\\|class\\)

    " Jesus tapdancing Christ, built-in Python syntax, you couldn't let me
    " override this in a normal way, could you?
    au FileType python if exists("python_space_error_highlight") | unlet python_space_error_highlight | endif

    au FileType python let b:delimitMate_nesting_quotes = ['"']

    au FileType python nnoremap <localleader>cc :STTConnect
    au FileType python nnoremap <localleader>cd :STTDisconnect
    au FileType python nnoremap <C-S> :<C-U>call SelectAndSendToTerminal('vap')<cr>
    au FileType python xnoremap <C-S> :<C-U>call SendSelectionToTerminal(visualmode())<cr>

    function! OpenPythonRepl() abort "{{{
        call term_start("bash -c python-rlwrap", {
                    \ "term_finish": "close",
                    \ "vertical": 1
                    \ })
    endfunction "}}}
    au FileType python nnoremap <silent> <localleader>o :call OpenPythonRepl()<cr>
    au FileType python RainbowParenthesesActivate
    au syntax python RainbowParenthesesLoadRound
    au syntax python RainbowParenthesesLoadSquare
    au syntax python RainbowParenthesesLoadBrace

    " Abbreviations {{{

    au FileType python call MakeSpacelessBufferIabbrev('rt', 'return ')

    " }}}
augroup END

" }}}
" QuickFix {{{

augroup ft_quickfix
    au!

    au Filetype qf setlocal colorcolumn=0 nolist nocursorline nowrap
    au FileType qf nnoremap <buffer> q :call CloseOnLast()<cr>
augroup END

" }}}
" REST {{{

augroup ft_rest
    au!

    function! GetRestFold(lnum) abort
        if getline(a:lnum) =~? '\v^(GET|POST|PUT|DELETE)\s'
            return 1
        endif
        if getline(a:lnum) =~? '\v^--\s*'
            return 0
        endif

        return '='
    endfunction

    au FileType rest setlocal foldmethod=expr foldexpr=GetRestFold(v:lnum)
    au FileType rest setlocal shiftwidth=2 softtabstop=2 expandtab
    au Filetype rest nnoremap <buffer> <localleader>1 yypVr=
augroup END

" }}}
" ReStructuredText {{{

augroup ft_rst
    au!

    au Filetype rst setlocal formatlistpat+=\\\|^[-*+]\\s\\+
    au Filetype rst nnoremap <buffer> <localleader>1 yypVr=
    au Filetype rst nnoremap <buffer> <localleader>2 yypVr-
    au Filetype rst nnoremap <buffer> <localleader>3 yypVr~
    au Filetype rst nnoremap <buffer> <localleader>4 yypVr`
augroup END

" }}}
" Scala {{{
augroup ft_scala
    au!

    function! DispatchMavenTest() abort
        let view = winsaveview()
        let zreg = @z

        " Move to the top of the file
        normal! gg

        " Find the spec name
        call search('\vclass (.*Spec)')
        normal! w"zyiw

        let spec = @z

        execute "Dispatch mvn -q -B test -Dtest=" . spec

        let @z = zreg
        call winrestview(view)
    endfunction

    au Filetype scala setlocal foldmethod=marker foldmarker={,}
    au Filetype scala nnoremap <buffer> <localleader>s mz:%!sort-scala-imports<cr>`z
    au Filetype scala nnoremap <buffer> M :call scaladoc#Search(expand("<cword>"))<cr>
    au Filetype scala vnoremap <buffer> M "ry:call scaladoc#Search(@r)<cr>
    au Filetype scala nnoremap <buffer> <localleader>t :call DispatchMavenTest()<cr>
    ")]
augroup END

" }}}
" Scheme {{{

augroup ft_scheme
    au!

    function! HighlightSchemeRepl() abort " {{{
        " set syntax=lisp
        syn match replPrompt /\v^\[([a-z A-Z])+\] [-._a-zA-Z0-9]+\>/
        syn match replComment /\v^;.*/

        " syn match replResult /\v^#\<[^>]+\>$/
        hi def link replResult Debug
        hi def link replComment Comment
    endfunction "}}}

    function! InitializeSchemeRepl() abort "{{{
        call HighlightSchemeRepl()
    endfunction "}}}

    function! OpenSchemeReplChez() abort "{{{
        call term_start("bash -c chez-rlwrap", {
            \ "term_finish": "close",
            \ "vertical": 1
        \ })
        call InitializeSchemeRepl()
    endfunction "}}}

    function! OpenSchemeReplPrompt() abort "{{{
        call term_start("bash -c " . input("? "), {
            \ "term_finish": "close",
            \ "vertical": 1
        \ })
        call InitializeSchemeRepl()
    endfunction "}}}

    function! MotionToSelectTopLevelSchemeForm() abort " {{{
        let motion = '99[(v%'
        if col('.') == 1 && getline('.')[col('.') - 1] == '('
            " We are at the beginning of a top-level form, so select until
            " the matching parenthesis
            let motion = 'v%'
        endif
        return motion
    endfunction "}}}

    function! SelectToplevelSchemeFormAndSendToTerminal() abort "{{{
        call SelectAndSendToTerminal(MotionToSelectTopLevelSchemeForm())
    endfunction "}}}

    function! SelectToplevelSchemeFormAndSendToVlimeREPL() abort "{{{
        let view = winsaveview()
        let reg_save = @@
        let motion = MotionToSelectTopLevelSchemeForm()

        execute "normal! " . motion . "y"
        call vlime#plugin#SendToREPL(@@)

        let @@ = reg_save
        call winrestview(view)
    endfunction "}}}

    function! IndentToplevelSchemeForm() abort "{{{
        let view = winsaveview()
        let reg_save = @@
        let motion = MotionToSelectTopLevelSchemeForm()

        execute "normal! " . motion . "="

        let @@ = reg_save
        call winrestview(view)
    endfunction "}}}

    function! TestSchemeSystem() abort "{{{
        let systems = split(system('ls -1 *.asd | grep -v test | cut -d. -f1 | uniq')) " its fine
        if len(systems) == 0
            echom "Could not find any .asd files..."
            return
        elseif len(systems) > 1
            echom "Found too many .asd files..."
            return
        endif

        call SendToTerminal("(asdf:test-system :" . systems[0] . ")")
    endfunction " }}}

    function! TestSchemePrompt() abort "{{{
        call SendToTerminal("(asdf:test-system :" . input("? ") . ")\n")
    endfunction " }}}

    function! InPackage() abort "{{{
        let packages = split(system('grep "(in-package .*" '. fnameescape(expand("%")) .' --only-matching | cut -d" " -f2 | uniq')) " its fine
        if len(packages) == 0
            echom "Could not find any defpackage lines..."
            return
        elseif len(packages) > 1
            echom "Found too many defpackage lines..."
            return
        endif

        call SendToTerminal("(in-package " . packages[0])
    endfunction " }}}

    au FileType scheme call SetSchemeWords()

    au FileType scheme setlocal iskeyword+=!,?,%,-
    au FileType scheme setlocal suffixesadd+=.scm

    au FileType scheme RainbowParenthesesActivate
    au syntax lisp RainbowParenthesesLoadRound

    " Fix windows:
    " - <c-w>j: select window below -- Vlime Connection
    " - <c-w>J: move it to the far bottom (and expand horizontally)
    " - <c-w>k: select window above --  the actual lisp buffer
    " - <c-w>H: move it to the far right (and expand vertically)
    au FileType scheme nnoremap <buffer> <localleader>W <c-w>j<c-w>J<c-w>k<c-w>H

    au FileType scheme let b:delimitMate_quotes = "\""

    au FileType scheme nnoremap <buffer> <silent> <localleader>o :call OpenSchemeReplChez()<cr>
    au FileType scheme nnoremap <buffer> <silent> <localleader>O :call OpenSchemeReplPrompt()<cr>
    au FileType scheme nnoremap <buffer> <silent> gI :<C-U>call IndentToplevelSchemeForm()<cr>
    au FileType scheme nnoremap <buffer> <silent> <localleader>i :call InPackage()<cr>
    au FileType scheme nnoremap <buffer> <localleader>cc :STTConnect
    au FileType scheme nnoremap <buffer> <localleader>cd :STTDisconnect
    au FileType scheme nnoremap <buffer> <silent> <C-S> :call SelectToplevelSchemeFormAndSendToTerminal()<cr>
    au FileType scheme xnoremap <buffer> <silent> <C-S> :call SendSelectionToTerminal(visualmode())<cr>
augroup END

" }}}
" Sh {{{

augroup ft_sh
    au!

    au FileType sh setlocal foldmethod=marker
augroup END

" }}}
" Typescript {{{

augroup ft_typescript
    au!

    function! TurnOnTypescriptFolding() abort "{{{
        let export       = '%(module\.)?export(s)?%(\.)?.*\{'
        let class        = 'class%(\s+\S+)*\s*\{'
        let method       = '%(async )?%(private )?%(%(get|set) )?%(\S*\.\S*|if|for|switch)@!\S+\s*\([^)]*\)\s*\{'
        let functionwrap = '\s*[a-zA-Z0-9:]*\S*\)\s*\{'
        let functiondec  = '%(async )?function%(\s+\S+)?\s*\([^)]*' . functionwrap
        let functiondef  = '%(%(const|var|let)\s)?\S+\s*\=\s*' . functiondec
        let arrowdefwrap = '\s*[a-zA-Z0-9:]*\)\s*\=\>\s*\{'
        let arrowdef     = '%(%(const|var|let)\s)?\S+\s*\=\s*\([^)]*' . arrowdefwrap

        let folded_statements = [
                    \ export,
                    \ class,
                    \ method,
                    \ functionwrap,
                    \ functiondec,
                    \ functiondef,
                    \ arrowdefwrap,
                    \ arrowdef
                    \ ]

        let b:manual_regexp_folding_statements_re_bare = '\v^\s*%(' . join(folded_statements, '|') . ')\s*$'
        call TurnOnManualRegexpFolding()
    endfunction "}}}
    au FileType typescript silent! call TurnOnTypescriptFolding()
    au FileType typescript silent! call RefreshManualRegexpFolding()

    au FileType typescript setlocal suffixesadd+=.ts,.js

    au Filetype typescript nnoremap <buffer> <C-^> :LspReferences<cr>
    au FileType typescript nnoremap <buffer> <silent> <C-]> :LspDefinition<cr>zvzz
    au FileType typescript nnoremap <buffer> <silent> gd :LspDefinition<cr>zvzz
    au FileType typescript nnoremap <buffer> <silent> ,S :LspRename<cr>
    au FileType typescript nnoremap <buffer> <silent> ◊ :LspCodeAction<cr>
    au FileType typescript nnoremap <buffer> <silent> K :LspHover<cr>
    au FileType typescript setlocal omnifunc=lsp#complete

    au FileType typescript inoremap <c-n> <c-x><c-o>

    au FileType typescript RainbowParenthesesActivate
    au syntax typescript RainbowParenthesesLoadRound
    au syntax typescript RainbowParenthesesLoadSquare
    au syntax typescript RainbowParenthesesLoadBrace

    " Abbreviations {{{

    au FileType typescript call MakeSpacelessBufferIabbrev('clz',  'class ')
    au FileType typescript call MakeSpacelessBufferIabbrev('class',  'NOPENOPENOPE')

    au FileType typescript call MakeSpacelessBufferIabbrev('int',  'interface ')
    au FileType typescript call MakeSpacelessBufferIabbrev('interface',  'NOPENOPENOPE')

    au FileType typescript call MakeSpacelessBufferIabbrev('impl',  'implements ')
    au FileType typescript call MakeSpacelessBufferIabbrev('implements',  'NOPENOPENOPE')

    au FileType typescript call MakeSpacelessBufferIabbrev('ext',  'extends ')
    au FileType typescript call MakeSpacelessBufferIabbrev('extends',  'NOPENOPENOPE')

    au FileType typescript call MakeSpacelessBufferIabbrev('ctor', 'constructor() {}<left><cr><up><end><left><left><left>')
    au FileType typescript call MakeSpacelessBufferIabbrev('constructor', 'NOPENOPENOPE')

    au FileType typescript call MakeSpacelessBufferIabbrev('fn',  'function ')
    au FileType typescript call MakeSpacelessBufferIabbrev('afn', 'function() {}<left><cr><up><end><left><left><left>')
    au FileType typescript call MakeSpacelessBufferIabbrev('function', 'NOPENOPENOPE')

    au FileType typescript call MakeSpacelessBufferIabbrev('rt', 'return ;<left>')
    au FileType typescript call MakeSpacelessBufferIabbrev('return', 'NOPENOPENOPE')

    au FileType typescript call MakeSpacelessBufferIabbrev('rq', 'require('''');<left><left><left>')
    au FileType typescript call MakeSpacelessBufferIabbrev('require', 'NOPENOPENOPE')

    au FileType typescript call MakeSpacelessBufferIabbrev('clog', 'console.log();<left><left>')
    au FileType javascript call MakeSpacelessBufferIabbrev('cerr', 'console.error(HERE);')

    " }}}
augroup END

" }}}
" Ternproject {{{

augroup ft_ternproject
    au!

    au BufNewFile,BufRead .tern-project nnoremap <buffer> q :call CloseOnLast()<cr>
augroup END

" }}}
" Vim {{{

augroup ft_vim
    au!

    au FileType vim setlocal foldmethod=marker
    au FileType help setlocal textwidth=78
    au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
    au BufEnter *.txt if &ft == 'help' | nnoremap <buffer> q :call CloseOnLast()<cr> | endif
augroup END

" }}}
" XML {{{

augroup ft_xml
    au!

    au FileType xml setlocal foldmethod=manual

    " Use <localleader>f to fold the current tag.
    au FileType xml nnoremap <buffer> <localleader>f Vatzf

    " Indent tag
    au FileType xml nnoremap <buffer> <localleader>= Vat=
augroup END

" }}}

" }}}
" Quick editing ----------------------------------------------------------- {{{

nnoremap <leader>eM :vsplit <C-R>=system('tempfile .')<left><left>
nnoremap <leader>eR :vsplit ~/Dropbox/rest<cr>
nnoremap <leader>eb :vsplit ~/dotfiles/.bashrc<cr>
nnoremap <leader>eg :vsplit ~/dotfiles/.gitconfig<cr>
nnoremap <leader>eh :vsplit ~/dotfiles/.hgrc<cr>
nnoremap <leader>em :vsplit ~/.muttrc<cr>
nnoremap <leader>ep :vsplit ~/Dropbox/plan/.plan<cr>
nnoremap <leader>eP :vsplit ~/Dropbox/plan/.<C-D>
nnoremap <leader>et :vsplit ~/dotfiles/.tmux.conf<cr>
nnoremap <leader>ev :vsplit ~/dotfiles/.vimrc<cr>

" }}}
" Quick reload ------------------------------------------------------------ {{{

nnoremap <leader>sv :let stay_sourcevimrc_view = winsaveview()<cr>:source $MYVIMRC<cr>:e<cr>:call winrestview(stay_sourcevimrc_view)<cr>

" }}}
" Convenience mappings ---------------------------------------------------- {{{

" Clean trailing whitespace
nnoremap <leader>w mz:%s/\s\+$//<cr>:let @/=''<cr>`z

" Change case
nnoremap <C-u> gUiw
inoremap <C-u> <esc>gUiwea

" Fix the & command by making sure substituion flags are not lost when
" re-executing
nnoremap & :&&<CR>
xnoremap & :&&<CR>

" Allows you to easily replace the current word and all its occurrences.
nnoremap <Leader>S :%s/<C-r>=expand("<cword>")<cr>//c<left><left>
vnoremap <Leader>S y:%s/<C-r>"//c<left><left>

" Emacs bindings in command line mode
cnoremap <C-a> <home>
cnoremap <C-e> <end>
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
" https://vi.stackexchange.com/a/7794
cnoremap <expr> <C-d> (getcmdpos() == len(getcmdline()) + 1 ? '<C-d>' : '<Del>')
cnoremap <M-b> <S-Left>
cnoremap <M-f> <S-Right>
cnoremap <M-d> <S-right><Delete>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>
cnoremap <Esc>d <S-right><Delete>
cnoremap <C-g> <C-c>

" Skip automatically to next difference
nnoremap do do]c
nnoremap dp dp]c

" Formatting, TextMate-style
nnoremap Q gqip
vnoremap Q gq

" Select pasted stuff
nnoremap <leader>V V`]

" Keep the cursor in place while joining lines
nnoremap J mzJ`z

" Split line (sister to [J]oin lines)
" The normal use of S is covered by cc, so don't worry about shadowing it.
nnoremap S i<cr><esc><right>mwgk:silent! s/\v +$//<cr>:noh<cr>`wh

" Better Completion
set complete=.,w,b,u,t " By deafult it includes 'i', which tells vim to look inside included files too
set completeopt=longest,menuone

" Unfuck my screen
nnoremap U :syntax sync fromstart<cr>:redraw!<cr>

" Quickreturn -- on my terminal, C-CR generates ◊
inoremap ◊ <esc>A<cr>


" Toggle [I]nvisible Characters
nnoremap <leader>I :set list!<cr>

" Window close (all) shortcuts
nnoremap <c-w>qq :q<cr>
nnoremap <c-w>qa :qa<cr>

" Forgot to `sudo vim ...` ?
nnoremap <leader>! :w !sudo tee %

" Select (charwise) the contents of the current line, excluding indentation.
" Great for pasting Python lines into REPLs.
nnoremap vv ^vg_

" Diff mode
nnoremap <localleader>d :windo diffthis<cr>
nnoremap <localleader>D :windo diffoff!<cr>


" Remove ANSI color escape codes for the edited file. This is handy when
" piping colored text into Vim.
function! RemoveAnsiColor() " {{{
    let l:save = winsaveview()
    %s/\[\(\d\{1,2}\(;\d\{1,2}\)\{0,2\}\)\?[m\|K]//
    call winrestview(l:save)
endfunction " }}}
nnoremap <Leader>rac :call RemoveAnsiColor()<cr>

nnoremap <silent> gw :ArgWrap<cr>

" Go back to the previous edited file with backspace
nnoremap <BS> <C-^>
nnoremap <C-^> <Nop>

"JSON prettify mappings
nnoremap <leader>J :%!python -m json.tool<cr>
xnoremap <leader>J :'<,'>!python -m json.tool<cr>

" Easy expansion of the Active File Directory
cnoremap <expr> %% getcmdtype() == ':' ? expand('%') : '%%'
cnoremap <expr> %d getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Quickfix navigation
nnoremap <silent> <left> :cprevious<CR>zvzz
nnoremap <silent> <right> :cnext<CR>zvzz

" Locations navigation
nnoremap <silent> <up> :lprevious<CR>zvzz
nnoremap <silent> <down> :lnext<CR>zvzz

" Folds
nnoremap <silent> [z zMzozkzvzz
nnoremap <silent> ]z zMzozjzvzz

" Macros
nnoremap Q @q
nnoremap <Leader>q :let @t = 'let @q = "' . @q<CR>:<C-f>o<ESC>"tp$a"<Esc>

" Consistency, Christ!
nnoremap Y y$
nnoremap D d$

" Insert Mode Completion {{{

inoremap <c-l> <c-x><c-l>
inoremap <c-f> <c-x><c-f>
inoremap <c-space> <c-x><c-o>
inoremap <c-@> <c-x><c-o>

" Better bindings when the completion menu is open {{{

" if empty('&t_BE')
"     inoremap <expr> <Esc>  pumvisible() ? "\<C-e>" : "\<Esc>"
" else
"     augroup bracketed_paste
"         au!

"         " Thanks: https://github.com/ryanpcmcquen/fix-vim-pasting
"         function! XTermPasteBegin() abort
"             set pastetoggle=<Esc>[201~
"             set paste
"             return ""
"         endfunction
"         au VimEnter * inoremap <special> <expr> <Esc>[200~ XTermPasteBegin()

"         au VimEnter * inoremap <special> <expr> <Esc>  pumvisible() ? "\<C-e>" : "\<Esc>"
"     augroup END
" endif

imap <expr> <CR>       pumvisible() ? "\<C-y>" : "<Plug>delimitMateCR"
imap <expr> <Down>     pumvisible() ? "\<C-n>" : "\<Down>"
imap <expr> <Up>       pumvisible() ? "\<C-p>" : "\<Up>"

" }}}

" }}}

" }}}
" CTags ------------------------------------------------------------------- {{{

nnoremap <leader><cr> :silent !myctags<cr>:redraw!<cr>

" Open tags and files under cursor in a *vertical* split
nnoremap <silent> <c-w><c-]> :exec("vertical stag " . expand("<cword>"))<cr>
nnoremap <silent> <c-w>] :exec("vertical stag " . expand("<cword>"))<cr>
nnoremap <silent> <c-w>f :vertical wincmd f<cr>

" }}}
" Plugin settings --------------------------------------------------------- {{{

" Ack {{{

nnoremap <leader>A :Ack!<space>
let g:ackprg = 'ag --vimgrep --hidden --smart-case --nogroup --nocolor --column'
let g:ack_use_async = 1

" }}}
" Argwrap {{{

let g:argwrap_tail_comma_braces='[{'

" }}}
" cb {{{

vmap <leader>y <Plug>CBCopy
nmap <leader>y <Plug>CBCopy
xmap <leader>d <leader>y<leader>Vd
nmap <leader>Y ggVG<Plug>CBCopy
nmap <leader>p <Plug>CBPasteAfter
nmap <leader>P <Plug>CBPasteBefore

" }}}
" delimitMate {{{

let delimitMate_expand_cr = 1
let delimitMate_expand_space = 0 " or custom abbreviations wouldn't work

" }}}
" Dispatch {{{

nnoremap <leader>d :Dispatch<cr>
nnoremap <leader>m :Dispatch<cr>

" }}}
" Easymotion {{{

let g:EasyMotion_leader_key = '<leader>'

" }}}
" editorconfig-vim {{{

let g:EditorConfig_exclude_patterns = [
         \ 'fugitive://.*',
         \ 'scp://.*'
         \ ]

" }}}
" Emmet {{{

let g:user_emmet_install_global = 0

" }}}
" Fugitive {{{

let g:fugitive_github_domains = []
let g:fugitive_gitlab_domains = ['https://gitlabdev01.iontrading.com']

nnoremap <leader>gcbb :!git cbb<cr>
nnoremap <leader>gd :Shell git diff <C-R>=expand('%')<cr><cr>
nnoremap <leader>gD :Shell git diff<cr>
nnoremap <leader>gp :Gpush<cr>
nnoremap <leader>gP :!git p -f<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>ga :Gadd<cr>
nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>ge :Gedit<cr>
nnoremap <leader>gco :Gcheckout<cr>
nnoremap <leader>gci :Gcommit<cr>
nnoremap <leader>gm :Gmove<cr>
nnoremap <leader>gr :!git r <C-R>=fnameescape(expand('%'))<cr><cr>
nnoremap <leader>gR :!git R<cr>
nnoremap <leader>gl :Shell git pl<cr>
nnoremap <leader>gL :Shell git plll<cr>
nnoremap <leader>gi :Shell git ind<cr>


augroup ft_fugitive
    au!

    au User Fugitive let g:netrw_browsex_viewer = "git web--browse"
    au BufNewFile,BufRead .git/index setlocal nolist
augroup END
augroup ft_shell_g_pl
    au!

    autocmd BufReadPost __Shell_Output__git_pl*  :silent call RemoveAnsiColor()
    autocmd BufReadPost __Shell_Output__git_pl*  setlocal filetype=gitrebase
    autocmd BufReadPost __Shell_Output__git_pl   nnoremap <buffer> R :!git rb <C-R>=split(getline('.'))[0]<CR>^1<CR>:Shell git pl<cr>
    autocmd BufReadPost __Shell_Output__git_pll  nnoremap <buffer> R :!git rb <C-R>=split(getline('.'))[0]<CR>^1<CR>:Shell git pll<cr>
    autocmd BufReadPost __Shell_Output__git_plll nnoremap <buffer> R :!git rb <C-R>=split(getline('.'))[0]<CR>^1<CR>:Shell git plll<cr>
augroup END

" }}}
" FZF {{{

if $OS == 'Windows_NT'
    packadd fzf-ruby
else
    packadd fzf
end
nnoremap <C-P> :<C-u>FZF<CR>

" }}}
" Gundo {{{

nnoremap <F5> :GundoToggle<CR>
let g:gundo_debug = 1
let g:gundo_preview_bottom = 1
let g:gundo_tree_statusline = "Gundo"
let g:gundo_preview_statusline = "Gundo Preview"

" }}}
" JK-Jumps {{{

let g:jk_jumps_minimum_lines = 2

" }}}
" Maven {{{

let g:maven_auto_chdir = 0
let g:maven_keymaps = 0
let g:maven_ignore_globs = [
            \ '*.js'
            \ ]

" }}}
" Neoformat {{{

if filereadable($PWD .'/node_modules/.bin/eslint')
    let g:neoformat_javascript_prettier = {
        \ 'exe': './node_modules/.bin/prettier',
        \ 'args': ['--stdin', '--stdin-filepath', '"%:p"'],
        \ 'stdin': 1,
        \ }
    let g:neoformat_typescript_prettier = {
        \ 'exe': './node_modules/.bin/prettier',
        \ 'args': ['--stdin', '--stdin-filepath', '"%:p"'],
        \ 'stdin': 1,
        \ }
endif

augroup neoformat_neoformat
  autocmd!

  " Thanks: https://github.com/sbdchd/neoformat/issues/134#issuecomment-347180213
  au BufWritePre * try | undojoin | Neoformat | catch /^Vim\%((\a\+)\)\=:E790/ | finally | silent Neoformat | endtry
augroup END

" }}}
" Neomake {{{

let g:neomake_open_list = 0
if filereadable($PWD .'/node_modules/.bin/eslint')
  let g:neomake_javascript_enabled_makers = ['eslint']
  let g:neomake_javascript_eslint_exe = $PWD .'/node_modules/.bin/eslint'
endif
if filereadable($PWD .'/node_modules/.bin/tslint')
  let g:neomake_typescript_enabled_makers = ['tslint']
  let g:neomake_typescript_tslint_exe = $PWD .'/node_modules/.bin/tslint'
endif
if filereadable($PWD .'/node_modules/.bin/markdownlint')
  let g:neomake_markdown_enabled_makers = ['markdownlint']
  let g:neomake_markdown_markdownlint_exe = $PWD .'/node_modules/.bin/markdownlint'
  let g:neomake_markdown_markdownlint_errorformat = '%f:%l %m'
endif

augroup neomake_neomake
    au!

    autocmd BufWritePost * Neomake
augroup END

let g:neomake_error_sign = {
    \ 'text': '●',
    \ 'texthl': 'NeomakeErrorSign'
    \ }
let g:neomake_warning_sign = {
    \ 'text': '●',
    \ 'texthl': 'NeomakeWarningSign',
    \ }
let g:neomake_message_sign = {
    \ 'text': '●',
    \ 'texthl': 'NeomakeMessageSign',
    \ }
let g:neomake_info_sign = {
    \ 'text': '●',
    \ 'texthl': 'NeomakeInfoSign'
    \ }

nnoremap <leader>C :Neomake<cr>

" }}}
" Netrw {{{

let g:netrw_bufsettings="noma nomod nonu nobl nowrap ro rnu"
" netrw's gx is fucking broken: https://github.com/vim/vim/issues/4738
let g:netrw_nogx = 1

" }}}
" OmniSharp {{{

let g:OmniSharp_selector_ui = 'ctrlp'

" }}}
" Projectionist {{{

if !exists('$DISABLE_PROJECTIONIST_HEURISTICS')
    let g:projectionist_heuristics = {
        \ "group_vars/&roles/": {
        \   "roles/*/": {"type": "role"},
        \   "roles/*/defaults/main.yml": {"type": "defaults"},
        \   "roles/*/files/": {"type": "files"},
        \   "roles/*/tasks/main.yml": {"type": "tasks"},
        \   "roles/*/templates/": {"type": "templates"},
        \   "roles/*/vars/": {"type": "vars"}
        \ },
        \ "pom.xml": {
        \   "src/main/java/*.java": {
        \     "alternate": "src/test/java/{}Test.java",
        \     "type": "source"
        \   },
        \   "src/test/java/*Test.java": {
        \     "alternate": "src/main/java/{}.java",
        \     "type": "test"
        \   },
        \   "*.java": {"dispatch": "javac {file}" },
        \   "*": {"make": "mvn"}
        \ },
        \ "package.json": {
        \   "*.js": {
        \     "alternate": "{}.spec.js",
        \     "type": "source"
        \   },
        \   "*.spec.js": {
        \     "alternate": "{}.js",
        \     "type": "test"
        \   },
        \   "*.ts": {
        \     "alternate": "{}.spec.ts",
        \     "type": "source"
        \   },
        \   "*.spec.ts": {
        \     "alternate": "{}.ts",
        \     "type": "test"
        \   }
        \ }}
endif

" }}}
" Python-Mode {{{

let g:pymode_run = 0

let g:pymode_lint = 0

let g:pymode_breakpoint = 1
let g:pymode_breakpoint_key = '<localleader>b'

let g:pymode_virtualenv = 1

let g:pymode_folding = 0

let g:pymode_syntax_all = 0
let g:pymode_syntax_builtin_objs = 1

" }}}
" Rainbow Parentheses {{{

let g:rbpt_max = 1

" }}}
" vim-sexp {{{

let g:sexp_filetypes = ''
let g:sexp_mappings = {}
let g:sexp_insert_after_wrap = 0
let g:sexp_enable_insert_mode_mappings = 0

function! s:vim_sexp_mappings() " {{{
    xmap <silent><buffer>   af   <Plug>(sexp_outer_list)
    omap <silent><buffer>   af   <Plug>(sexp_outer_list)
    xmap <silent><buffer>   if   <Plug>(sexp_inner_list)
    omap <silent><buffer>   if   <Plug>(sexp_inner_list)
    xmap <silent><buffer>   aF   <Plug>(sexp_outer_top_list)
    omap <silent><buffer>   aF   <Plug>(sexp_outer_top_list)
    xmap <silent><buffer>   iF   <Plug>(sexp_inner_top_list)
    omap <silent><buffer>   iF   <Plug>(sexp_inner_top_list)
    xmap <silent><buffer>   as   <Plug>(sexp_outer_string)
    omap <silent><buffer>   as   <Plug>(sexp_outer_string)
    xmap <silent><buffer>   is   <Plug>(sexp_inner_string)
    omap <silent><buffer>   is   <Plug>(sexp_inner_string)
    xmap <silent><buffer>   ae   <Plug>(sexp_outer_element)
    omap <silent><buffer>   ae   <Plug>(sexp_outer_element)
    xmap <silent><buffer>   ie   <Plug>(sexp_inner_element)
    omap <silent><buffer>   ie   <Plug>(sexp_inner_element)

    nmap <silent><buffer>   [[   <Plug>(sexp_swap_element_backward)
    xmap <silent><buffer>   [[   <Plug>(sexp_swap_element_backward)
    nmap <silent><buffer>   ]]   <Plug>(sexp_swap_element_forward)
    xmap <silent><buffer>   ]]   <Plug>(sexp_swap_element_forward)

    nmap <silent><buffer>   {    <Plug>(sexp_move_to_prev_top_element)
    xmap <silent><buffer>   {    <Plug>(sexp_move_to_prev_top_element)
    omap <silent><buffer>   {    <Plug>(sexp_move_to_prev_top_element)
    nmap <silent><buffer>   }    <Plug>(sexp_move_to_next_top_element)
    xmap <silent><buffer>   }    <Plug>(sexp_move_to_next_top_element)
    omap <silent><buffer>   }    <Plug>(sexp_move_to_next_top_element)

    " Easy getting around
    nmap <silent><buffer>   B    <Plug>(sexp_move_to_prev_element_head)
    nmap <silent><buffer>   W    <Plug>(sexp_move_to_next_element_head)
    nmap <silent><buffer>   gE   <Plug>(sexp_move_to_prev_element_tail)
    nmap <silent><buffer>   E    <Plug>(sexp_move_to_next_element_tail)
    xmap <silent><buffer>   B    <Plug>(sexp_move_to_prev_element_head)
    xmap <silent><buffer>   W    <Plug>(sexp_move_to_next_element_head)
    xmap <silent><buffer>   gE   <Plug>(sexp_move_to_prev_element_tail)
    xmap <silent><buffer>   E    <Plug>(sexp_move_to_next_element_tail)
    omap <silent><buffer>   B    <Plug>(sexp_move_to_prev_element_head)
    omap <silent><buffer>   W    <Plug>(sexp_move_to_next_element_head)
    omap <silent><buffer>   gE   <Plug>(sexp_move_to_prev_element_tail)
    omap <silent><buffer>   E    <Plug>(sexp_move_to_next_element_tail)

    " Slurping/Barfing
    "
    " angle bracket indicates the direction, and the parenthesis
    " indicates which end to operate on.
    nmap <silent><buffer>   <(   <Plug>(sexp_capture_prev_element)
    nmap <silent><buffer>   >)   <Plug>(sexp_capture_next_element)
    nmap <silent><buffer>   >(   <Plug>(sexp_emit_head_element)
    nmap <silent><buffer>   <)   <Plug>(sexp_emit_tail_element)

    " Insert head/tail of the current list
    nmap <silent><buffer>   <I   <Plug>(sexp_insert_at_list_head)
    nmap <silent><buffer>   >I   <Plug>(sexp_insert_at_list_tail)
endfunction " }}}

augroup sexp_mappings
    autocmd!
    autocmd FileType scheme,lisp call s:vim_sexp_mappings()
augroup END

" }}}
" vim-goobook {{{

let g:goobookprg="goobook"

" }}}
" vim-lsp {{{

" let g:lsp_log_verbose = 1
" let g:lsp_log_file = expand('~/vim-lsp.log')

let g:lsp_diagnostics_enabled = 0     " disable diagnostics
let g:lsp_diagnostics_echo_cursor = 1 " enable echo under cursor when in normal mode
let g:lsp_diagnostics_use_loclist = 1 " use loc-list instead of quickfix for errors/warnings
let g:lsp_signs_enabled = 1           " enable signs

let g:lsp_signs_error = {'text': '●'}
let g:lsp_signs_warning = {'text': '●' }
let g:lsp_signs_hint = {'text': '●' }

if executable('typescript-language-server')
  au User lsp_setup call lsp#register_server({
        \ 'name': 'typescript-language-server',
        \ 'cmd': { server_info->[&shell, &shellcmdflag, 'typescript-language-server --stdio']},
        \ 'root_uri': { server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'tsconfig.json'))},
        \ 'whitelist': ['typescript'],
        \ })
endif
if executable('js-langserver')
  au User lsp_setup call lsp#register_server({
        \ 'name': 'js-langserver',
        \ 'cmd': { server_info->[&shell, &shellcmdflag, 'js-langserver --stdio']},
        \ 'root_uri': { server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), '.tern-project'))},
        \ 'whitelist': ['javascript']
        \ })
endif

" }}}
" vim-json {{{

let g:vim_json_syntax_conceal = 0

" }}}
" vim-rest-console {{{

let g:vrc_follow_redirects = 1
let g:vrc_include_response_header = 1
let g:vrc_resolve_to_ipv4 = 1
let g:vrc_ssl_secure = 1
let g:vrc_allow_get_request_body = 1
let g:vrc_trigger = '<C-S>'
let g:vrc_horizontal_split = 1

augroup ft_restresponse
    au!

    autocmd BufNewFile __REST_response__ nnoremap <buffer> q :call CloseOnLast()<cr>
augroup END

" }}}
" vim-visual-star-search {{{

xnoremap * :<C-u>call VisualStarSearchSet('/')<CR>/<C-R>=@/<CR><CR><C-O>
xnoremap # :<C-u>call VisualStarSearchSet('?')<CR>?<C-R>=@/<CR><CR><C-O>

" }}}
" vitality {{{

let g:vitality_always_assume_mintty = 1

" }}}
" vlime {{{

if !exists('g:vlime_added_to_rtp')
    let g:vlime_added_to_rtp = 1
    set rtp+=~/my-env/opt/vlime/vim
endif

let g:vlime_window_settings = {
        \ "sldb": {
            \ "pos": "belowright",
            \ "vertical": v:false
        \ },
        \ "xref": {
            \ "pos": "belowright",
            \ "size": 5,
            \ "vertical": v:false
        \ },
        \ "repl": {
            \ "pos": "belowright",
            \ "vertical": v:false
        \ },
        \ "inspector": {
            \ "pos": "belowright",
            \ "vertical": v:false
        \ },
        \ "arglist": {
            \ "pos": "topleft",
            \ "size": 2,
            \ "vertical": v:false
        \ }
    \ }

augroup CustomVlimeInputBuffer
    autocmd!
    autocmd FileType vlime_input inoremap <silent> <buffer> <tab> <c-r>=vlime#plugin#VlimeKey("tab")<cr>
    autocmd FileType vlime_input setlocal omnifunc=vlime#plugin#CompleteFunc
    autocmd FileType vlime_input setlocal indentexpr=vlime#plugin#CalcCurIndent()
augroup end

" }}}
" Writegooder {{{

let g:writegooder_disable_mappings = 1
nnoremap <localleader>W :WritegooderToggle<cr>

" }}}

" }}}
" Utils ------------------------------------------------------------------- {{{

" Synstack {{{

" Show the stack of syntax hilighting classes affecting whatever is under the
" cursor.
function! s:SynStack() "{{{
  echo join(map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")'), " > ")
endfunc "}}}

command! SynStack call s:SynStack()

" }}}

" }}}
" Environments (GUI/Console) ---------------------------------------------- {{{

if has('gui_running')
    "set guifont=Monospace\ 9

    " Remove all the UI cruft
    set go-=T
    set go-=l
    set go-=L
    set go-=r
    set go-=R
    set go-=m

    highlight SpellBad term=underline gui=undercurl guisp=Orange

    " Different cursors for different modes.
    set guicursor=n-v-c:block-Cursor-blinkon0
    set guicursor+=i-c:ver20-Cursor
endif

" }}}
" Plugins wannabe --------------------------------------------------------- {{{

function! s:GetCurrentSelection(type) " {{{
    let reg_save = @@

    if a:type ==# 'v'
        silent execute "normal! `<" . a:type . "`>y"
    elseif a:type ==# 'V'
        silent execute "normal! `<" . a:type . "`>y"
    elseif a:type ==# 'char'
        silent execute "normal! `[v`]y"
    endif
    let selection = @@

    let @@ = reg_save

    return selection
endfunction " }}}

" Highlight Word {{{
"
" This mini-plugin provides a few mappings for highlighting words temporarily.
"
" Sometimes you're looking at a hairy piece of code and would like a certain
" word or two to stand out temporarily.  You can search for it, but that only
" gives you one color of highlighting.  Now you can use <leader>N where N is
" a number from 1-6 to highlight the current word in a specific color.

function! HiInterestingWord(n) " {{{
    " Save our location.
    normal! mz

    " Yank the current word into the z register.
    normal! "zyiw

    " Calculate an arbitrary match ID.  Hopefully nothing else is using it.
    let mid = 86750 + a:n

    " Clear existing matches, but don't worry if they don't exist.
    silent! call matchdelete(mid)

    " Construct a literal pattern that has to match at boundaries.
    let pat = '\V\<' . escape(@z, '\') . '\>'

    " Actually match the words.
    call matchadd("InterestingWord" . a:n, pat, 1, mid)

    " Move back to our original location.
    normal! `z
endfunction " }}}

" Mappings {{{

nnoremap <silent> <leader>1 :call HiInterestingWord(1)<cr>
nnoremap <silent> <leader>2 :call HiInterestingWord(2)<cr>
nnoremap <silent> <leader>3 :call HiInterestingWord(3)<cr>
nnoremap <silent> <leader>4 :call HiInterestingWord(4)<cr>
nnoremap <silent> <leader>5 :call HiInterestingWord(5)<cr>
nnoremap <silent> <leader>6 :call HiInterestingWord(6)<cr>

" }}}
" Default Highlights {{{

hi def InterestingWord1 guifg=#000000 ctermfg=16 guibg=#ffa724 ctermbg=214
hi def InterestingWord2 guifg=#000000 ctermfg=16 guibg=#aeee00 ctermbg=154
hi def InterestingWord3 guifg=#000000 ctermfg=16 guibg=#8cffba ctermbg=121
hi def InterestingWord4 guifg=#000000 ctermfg=16 guibg=#b88853 ctermbg=137
hi def InterestingWord5 guifg=#000000 ctermfg=16 guibg=#ff9eb8 ctermbg=211
hi def InterestingWord6 guifg=#000000 ctermfg=16 guibg=#ff2c4b ctermbg=195

" }}}

"}}}
" Shell {{{

let ShellOutputBufferName = "__Shell_Output__"

command! -complete=shellcmd -nargs=+ Shell call s:Shell(<q-args>)

function! s:Shell(command)
    " Check whether the shell buffer is already created
    let bufName = g:ShellOutputBufferName . a:command
    let bufName = substitute(bufName, ' ',  '_', 'g')
    let bufName = substitute(bufName, '\~', '_tilde_', 'g')
    let bufName = substitute(bufName, '/',  '_slash_', 'g')
    let shl_bufnum = bufnr(bufName)
    if shl_bufnum == -1
        exe "botright vnew " . bufName
    else
        " Shell buffer is already created. Check whether it is open
        " in one of the windows
        let shl_winnum = bufwinnr(shl_bufnum)
        if shl_winnum != -1
            " Jump to the window which has the shell buffer if we are not
            " already in that window
            if winnr() != shl_winnum
                exe shl_winnum . "wincmd w"
            endif
        else
            " Create a new shell buffer
            exe "botright vsplit +buffer" . shl_bufnum
        endif
    endif

    " Dump the output of the command in the buffer
    normal! ggdG
    let output = system(a:command . " 2>&1")
    call append(0, split(output, '\v\n'))
    " Move to the top of the file
    normal! gg
    " Then trigger the event
    doautocmd BufReadPost
endfunction

augroup ft_shelloutput
    au!

    autocmd BufNewFile __Shell_Output__* setlocal buftype=nofile
    autocmd BufNewFile __Shell_Output__* setlocal bufhidden=hide
    autocmd BufNewFile __Shell_Output__* setlocal noswapfile
    autocmd BufNewFile __Shell_Output__* setlocal buflisted
    autocmd BufNewFile __Shell_Output__* nnoremap <buffer> q :call CloseOnLast()<cr>
augroup END

" }}}
" Diffwhite Toggle {{{

set diffopt-=iwhite
let g:diffwhitespaceon = 0
function! ToggleDiffWhitespace()
    if g:diffwhitespaceon
        set diffopt-=iwhite
        let g:diffwhitespaceon = 0
    else
        set diffopt+=iwhite
        let g:diffwhitespaceon = 1
    endif
    diffupdate
endfunc

" TODO: Figure out the diffexpr shit necessary to make this buffer-local.
nnoremap <leader>W :call ToggleDiffWhitespace()<CR>

" }}}
" Ack operator {{{

" Motions to Ack for things.  Works with pretty much everything, including:
"
"   w, W, e, E, b, B, t*, f*, i*, a*, and custom text objects
"
" Awesome.
"
" Note: If the text covered by a motion contains a newline it won't work.  Ack
" searches line-by-line.

nnoremap <silent> <leader>a :set opfunc=<SID>OperatorAckGlobal<CR>g@
xnoremap <silent> <leader>a :<C-U>call <SID>OperatorAckGlobal(visualmode())<CR>
nnoremap <silent> <localleader>a :set opfunc=<SID>OperatorAckLocal<CR>g@
xnoremap <silent> <localleader>a :<C-U>call <SID>OperatorAckLocal(visualmode())<CR>

function! s:OperatorAck(type, add_word_boundaries, current_dir_only) abort
    let escaped = escape(s:GetCurrentSelection(a:type), '#')
    let pattern = shellescape(escaped)
    if a:add_word_boundaries
        let pattern = shellescape('\b'.escaped.'\b')
    endif

    let location = ''
    if a:current_dir_only
        let location = expand('%:h')
    endif

    execute "normal! :Ack! " . pattern . " " . location . "\<cr>"
endfunction

function! s:OperatorAckGlobal(type, ...) abort
    let add_word_boundaries = get(a:, 1, 1)
    call s:OperatorAck(a:type, add_word_boundaries, 0)
endfunction

function! s:OperatorAckLocal(type, ...) abort
    let add_word_boundaries = get(a:, 1, 1)
    call s:OperatorAck(a:type, add_word_boundaries, 1)
endfunction

nnoremap <silent> <leader>* viw:<C-U>call <SID>OperatorAckGlobal(visualmode())<CR>
xnoremap <silent> <leader>* :<C-U>call <SID>OperatorAckGlobal(visualmode())<CR>
nnoremap <silent> g<leader>* viw:<C-U>call <SID>OperatorAckGlobal(visualmode(), 0)<CR>
xnoremap <silent> g<leader>* :<C-U>call <SID>OperatorAckGlobal(visualmode(), 0)<CR>

nnoremap <silent> <localleader>* viw:<C-U>call <SID>OperatorAckLocal(visualmode())<CR>
xnoremap <silent> <localleader>* :<C-U>call <SID>OperatorAckLocal(visualmode())<CR>
nnoremap <silent> g<localleader>* viw:<C-U>call <SID>OperatorAckLocal(visualmode(), 0)<CR>
xnoremap <silent> g<localleader>* :<C-U>call <SID>OperatorAckLocal(visualmode(), 0)<CR>

" }}}
" Manual RegExp Folding {{{

function! UpdateManualRegexpFolds()
    " Save cursor position
    let manual_regexp_folding_view = winsaveview()

    " Delete all folds
    normal zE

    " Do the magic!
    let command = ''
    " For each line matching any of the configured regexps
    let command.= 'g/'.b:manual_regexp_folding_statements_re_bare.'/'
    " Move to the end of the line, create a fold with the _matching_ line
    " It's better to use 'g_' and not '$', as the former would stop to the last
    " non-blank char
    let command.= 'normal g_zf%zo'
    execute command

    " Restore cursor position
    call winrestview(manual_regexp_folding_view)

    " Close all the folds byt the ones the current line is in
    normal! zMzv
endfunction

function! TurnOnManualRegexpFolding()
    setlocal foldmethod=manual

    call UpdateManualRegexpFolds()
endfunction

function! RefreshManualRegexpFolding()
    call UpdateManualRegexpFolds()
endfunction

" }}}
" Numbers {{{

" Motion for numbers.  Great for CSS.  Lets you do things like this:
"
" margin-top: 200px; -> daN -> margin-top: px;
"              ^                          ^
" TODO: Handle floats.

onoremap N :<c-u>call <SID>NumberTextObject(0)<cr>
xnoremap N :<c-u>call <SID>NumberTextObject(0)<cr>
onoremap aN :<c-u>call <SID>NumberTextObject(1)<cr>
xnoremap aN :<c-u>call <SID>NumberTextObject(1)<cr>
onoremap iN :<c-u>call <SID>NumberTextObject(1)<cr>
xnoremap iN :<c-u>call <SID>NumberTextObject(1)<cr>

function! s:NumberTextObject(whole)
    let num = '\v[0-9]'

    " If the current char isn't a number, walk forward.
    while getline('.')[col('.') - 1] !~# num
        normal! l
    endwhile

    " Now that we're on a number, start selecting it.
    normal! v

    " If the char after the cursor is a number, select it.
    while getline('.')[col('.')] =~# num
        normal! l
    endwhile

    " If we want an entire word, flip the select point and walk.
    if a:whole
        normal! o

        while col('.') > 1 && getline('.')[col('.') - 2] =~# num
            normal! h
        endwhile

        normal! o
    endif
endfunction

" }}}
" Poor's man yankstack {{{

function! s:PasteCycle(after) abort " {{{
  let l:now = localtime()
  if (l:now - get(b:, 'paste_cycle_last_run', 0)) > 5
    " This function was run more than 5 seconds ago, so let's assume
    " the user is trying to starting from scratch
    let b:paste_cycle_reg_num = 0
  else
    " Otherwise, paste the last used register + 1 (modulo 10 because Vim
    " supports only 10 _redo_ registers)
    let b:paste_cycle_reg_num = (get(b:, 'paste_cycle_reg_num', -1) + 1) % 10
  endif
  let b:paste_cycle_last_run = l:now
  let l:paste_op_cmd = a:after ? "p" : "P"
  let l:normal_cmd = 'u"' . b:paste_cycle_reg_num . l:paste_op_cmd

  echo 'paste-cycle: ' . l:normal_cmd
  exe 'normal ' . l:normal_cmd
endfunction " }}}

nnoremap gp :call <SID>PasteCycle(1)<CR>
nnoremap gP :call <SID>PasteCycle(0)<CR>

" }}}
" SendToTerminal {{{

let g:stt_trailing_new_lines = 1
let g:stt_command_prompt_regexpes = ['\s*\$\s*', '\s*>\s*']
let g:stt_strip_command_prompt = 1

function! s:STTConnect() abort
    buffers
    let l:buffer = input('Select buffer? ')
    let g:stt_buffnr = str2nr(l:buffer)
endfunction

function! s:STTDisconnect() abort
    if exists('g:stt_buffnr')
      unlet g:stt_buffnr
    endif
endfunction

command! STTConnect call s:STTConnect()
command! STTDisconnect call s:STTDisconnect()

function! s:FindTerminal() abort
    let terminals = filter(range(1, bufnr('$')), "getbufvar(v:val, '&buftype') == 'terminal'")
    if len(terminals) > 0
      let g:stt_buffnr = terminals[0]
      return g:stt_buffnr
    endif
endfunction

function! SendToTerminal(data) abort " {{{
    if !exists('g:stt_buffnr') && !s:FindTerminal()
        echom "No terminal selected"
    elseif !bufexists(g:stt_buffnr)
        echom "Terminal buffer does not exist: " . g:stt_buffnr
    else
        " Strip the last new-line (later we will re-add as many new-lines
        " as required)
        let keys = substitute(a:data, '\n$', '', '')

        " Automatically strip out the command prompt (if told to)
        let strip_command_prompt =
            \ get(b:, 'stt_strip_command_prompt',
                \ get(g:, 'stt_strip_command_prompt', 0))
        if strip_command_prompt
          let command_prompt_regexpes =
              \ get(b:, 'stt_command_prompt_regexpes',
                  \ get(g:, 'stt_command_prompt_regexpes', []))

          let command_prompt_regexp = join(command_prompt_regexpes, '\|')
          let keys = substitute(keys, command_prompt_regexp, '', '')
        endif

        let trailing_new_lines =
            \ get(b:, 'stt_trailing_new_lines',
                \ get(g:, 'stt_trailing_new_lines', 1))
        while trailing_new_lines > 0
          let keys = keys . "\<CR>"
          let trailing_new_lines = trailing_new_lines - 1
        endwhile

        call term_sendkeys(g:stt_buffnr, keys)
    endif
endfunction " }}}
function! SendSelectionToTerminal(type) abort " {{{
    call SendToTerminal(s:GetCurrentSelection(a:type))
endfunction " }}}
function! SelectAndSendToTerminal(motion) abort " {{{
    " Save screen
    let view = winsaveview()

    " Ripped from [vim-glance](https://github.com/arzg/vim-glance/blob/master/autoload/glance.vim)
    " Scrolloff conflicts with the mapping, so we set it to zero after saving
    " its value (for future restoration)
    let s:scrolloffsave = &scrolloff
    set scrolloff=0

    " Select the content of the visible screen
    execute "normal! ". a:motion ."\<Esc>"
    call SendSelectionToTerminal(visualmode())

    " Restore scrolloff
    let &scrolloff = s:scrolloffsave

    " Restore screen
    call winrestview(view)
endfunction "}}}

" }}}
" SendToUrlview {{{

function! s:SendSelectionToUrlview() abort " {{{
    silent execute "'<,'> write !lg-fzf"
endfunction "}}}
function! s:SelectAndSendToUrlview(motion) abort " {{{
    " Save screen
    let view = winsaveview()

    " Ripped from [vim-glance](https://github.com/arzg/vim-glance/blob/master/autoload/glance.vim)
    " Scrolloff conflicts with the mapping, so we set it to zero after saving
    " its value (for future restoration)
    let s:scrolloffsave = &scrolloff
    set scrolloff=0

    " Select the content of the visible screen
    execute "normal! ". a:motion ."\<Esc>"
    call s:SendSelectionToUrlview()

    " Restore scrolloff
    let &scrolloff = s:scrolloffsave

    " Restore screen
    call winrestview(view)
endfunction "}}}

nnoremap gx :<C-U>call <SID>SelectAndSendToUrlview('viW')<CR>
xnoremap gx :<C-U>call <SID>SendSelectionToUrlview()<CR>
nnoremap ,u :<C-U>call <SID>SelectAndSendToUrlview('VHoL')<CR>

" }}}

" }}}
