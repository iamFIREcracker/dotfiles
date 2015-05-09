" Preamble -----------------------------------------------------------------{{{
runtime bundle/vim-pathogen/autoload/pathogen.vim

filetype off
execute pathogen#infect()
execute pathogen#helptags()
filetype plugin indent on
set nocompatible

" }}}
" Basic options ------------------------------------------------------------{{{
set encoding=utf-8
set modelines=0
set mouse=a
set autoindent
set showmode
set showcmd
set hidden
set visualbell
set cursorline
autocmd WinLeave * setlocal nocursorline
autocmd WinEnter * setlocal cursorline
set ttyfast
set ruler
set backspace=indent,eol,start
set relativenumber
autocmd WinLeave * setlocal norelativenumber
autocmd WinEnter * setlocal relativenumber
autocmd InsertEnter * setlocal norelativenumber
autocmd InsertLeave * setlocal relativenumber
set laststatus=2
set history=1000
set undofile
set undoreload=10000
set list
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set shell=bash
set lazyredraw
set matchtime=3
set showbreak=↪
set splitbelow
set splitright
set fillchars=diff:⣿,vert:│
set ttimeout
set notimeout
set nottimeout
set autowrite
set shiftround
set autoread
set title
set linebreak
set dictionary=/usr/share/dict/words
"set clipboard=unnamed

" Make Vim able to edit crontab files again.
set backupskip=/tmp/*,/private/tmp/*"

" Save when losing focus
au FocusLost * :wa

" Resize splits when the window is resized
au VimResized * :wincmd =

" cpoptions+=J, dammit {{{

" Something occasionally removes this.  If I manage to find it I'm going to
" comment out the line and replace all its characters with 'FUCK'.
augroup twospace
    au BufRead * :set cpoptions+=J
augroup END

" }}}
" Trailing whitespace {{{
" Only shown when not in insert mode so I don't go insane.

augroup trailing
    au!
    au InsertEnter * :set listchars-=trail:⌴
    au InsertLeave * :set listchars+=trail:⌴
augroup END

" }}}
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

" Clojure/Leiningen
set wildignore+=classes
"set wildignore+=lib

" }}}
" Line Return {{{

" Make sure Vim returns to the same line when you reopen a file.
" Thanks, Amit
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     execute 'normal! g`"zvzz' |
        \ endif
augroup END

" }}}
" Tabs, spaces, wrapping {{{

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set nowrap
set textwidth=79
set formatoptions=qrn1
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
let g:molokai_original = 1
let g:rehash256 = 1
colorscheme molokai

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" }}}

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

noremap <silent><space> :noh<cr>:call clearmatches()<cr>

runtime macros/matchit.vim
map <tab> %

" Made D behave
nnoremap D d$

" Keep search matches in the middle of the window and pulse the line when moving
" to them.
nnoremap n nzvzz:call PulseCursorLine()<cr>
nnoremap N Nzvzz:call PulseCursorLine()<cr>

" Same when jumping around
nnoremap g; g;zz
nnoremap g, g,zz
nnoremap gd gdzz
nnoremap gD gDzz

" Don't move on * and #
nnoremap * *<C-o>zz
nnoremap # #<C-o>zz

" Window resizing
nnoremap <c-left> 5<c-w>>
nnoremap <c-right> 5<c-w><

" Easier to type, and I never use the default behavior.
noremap H ^
noremap L g_

" Heresy
inoremap <c-a> <esc>I
inoremap <c-e> <esc>A

" Open a Quickfix window for the last search.
nnoremap <silent> <leader>/ :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

" Fix linewise visual selection of various text objects
nnoremap VV V
nnoremap Vit vitVkoj
nnoremap Vat vatV
nnoremap Vab vabV
nnoremap VaB vaBV

" Yank to OS clipboard
noremap <leader>y "*y

" Paste OS clipboard without messing up indent.
noremap <leader>p :set paste<CR>"*p<CR>:set nopaste<CR>

" Error navigation {{{
"
"             Location List     QuickFix Window
"            (e.g. Syntastic)     (e.g. Ack)
"            ----------------------------------
" Next      |     M-j               M-Down     |
" Previous  |     M-k                M-Up      |
"            ----------------------------------
"
nnoremap ∆ :lnext<cr>zvzz
nnoremap ˚ :lprevious<cr>zvzz
inoremap ∆ <esc>:lnext<cr>zvzz
inoremap ˚ <esc>:lprevious<cr>zvzz
"nnoremap ˆ :cnext<cr>zvzz
"nnoremap µ :cprevious<cr>zvzz
"inoremap ˆ <esc>:cnext<cr>zvzz
"inoremap µ <esc>:cprevious<cr>zvzz

" }}}
" Directional Keys {{{

" It's 2011.
noremap j gj
noremap k gk

" Easy buffer navigation
noremap <C-h>  <C-w>h
noremap <C-j>  <C-w>j
noremap <C-k>  <C-w>k
noremap <C-l>  <C-w>l
noremap <leader>v <C-w>v

" }}}
" Highlight word {{{

nnoremap <silent> <leader>h1 :execute 'match InterestingWord1 /\<<c-r><c-w>\>/'<cr>
nnoremap <silent> <leader>h2 :execute '2match InterestingWord2 /\<<c-r><c-w>\>/'<cr>
nnoremap <silent> <leader>h3 :execute '3match InterestingWord3 /\<<c-r><c-w>\>/'<cr>

" }}}

" }}}
" Folding ----------------------------------------------------------------- {{{

set foldlevelstart=0

" Space to toggle folds.
nnoremap <space> za
vnoremap <space> za

" Use ,z to "focus" the current fold.
nnoremap <leader>z zMzvzz

" Fix automatic unfolding while entering insert mode
" http://stackoverflow.com/questions/4630892/vim-folds-open-up-when-giving-an-unmatched-opening-brace-parenthesis
autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif

" }}}
" Destroy infuriating keys ------------------------------------------------ {{{

" Fuck you, help key.
noremap  <F1> <nop>
inoremap <F1> <nop>

" Fuck you too, manual key.
nnoremap K <nop>

" Stop it, hash key.
inoremap # X<BS>#

" }}}
" Various filetype-specific stuff ----------------------------------------- {{{

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
    function! GenerateTransformAndCopy() " {{{
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
" CSS and LessCSS {{{

augroup ft_css
    au!

    au BufNewFile,BufRead *.less setlocal filetype=less

    au Filetype less,css setlocal foldmethod=marker
    au Filetype less,css setlocal foldmarker={,}
    au Filetype less,css setlocal omnifunc=csscomplete#CompleteCSS
    au Filetype less,css setlocal iskeyword+=-

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
    au BufNewFile,BufRead *.less,*.css nnoremap <buffer> <localleader>S ?{<CR>jV/\v^\s*\}?$<CR>k:sort<CR>:noh<CR>
    " Make {<cr> insert a pair of brackets in such a way that the cursor is correctly
    " positioned inside of them AND the following code doesn't get unfolded.
    au BufNewFile,BufRead *.less,*.css inoremap <buffer> {<cr> {<cr><space><space><space><space>.<cr><esc>kA<bs>
augroup END

" }}}
" Diff {{{

augroup ft_diff
    au!
    au Filetype diff setlocal nolist
augroup END

" }}}
" HTML {{{

augroup ft_html
    au!

    au BufNewFile,BufRead *.html setlocal filetype=htmldjango
    au FileType html,jinja,htmldjango setlocal foldmethod=manual

    " Use Shift-Return to turn this:
    "     <tag>|</tag>
    "
    " into this:
    "     <tag>
    "         |
    "     </tag>
    au FileType html,jinja,htmldjango nnoremap <buffer> <s-cr> vit<esc>a<cr><esc>vito<esc>i<cr><esc>
augroup END

" }}}
" Javascript {{{

augroup ft_javascript
    au!

    au FileType javascript setlocal foldmethod=marker
    au FileType javascript setlocal foldmarker={,}

    " Make {<cr> insert a pair of brackets in such a way that the cursor is correctly
    " positioned inside of them AND the following code doesn't get unfolded.
    au Filetype javascript inoremap <buffer> {<cr> {<cr><space><space><space><space>.<cr><esc>kA<bs>
augroup END

" }}}
" SQL {{{

augroup ft_sql
    au!

    au FileType sql setlocal foldmethod=marker
    au FileType sql setlocal foldmarker={{{,}}}
augroup END

" }}}
" Pentadacty {{{

augroup ft_pentadactyl
    au!

    au FileType pentadactyl setlocal foldmethod=marker
    au FileType pentadactyl setlocal foldmarker={{{,}}}
augroup END

" }}}
" Vagrant {{{

augroup ft_vagrant
    au!
    au BufRead,BufNewFile Vagrantfile set ft=ruby
augroup END

" }}}
" Vim {{{

augroup ft_vim
    au!

    au FileType vim setlocal foldmethod=marker
    au FileType help setlocal textwidth=78
    au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
augroup END

" }}}
" Mail {{{

augroup ft_mail
    au!

    au Filetype mail setlocal spell
augroup END

" }}}
" Markdown {{{

augroup ft_markdown
    au!

    au BufNewFile,BufRead *.m*down setlocal filetype=markdown

    au Filetype markdown setlocal spell

    " Use <localleader>1/2/3 to add headings.
    au Filetype markdown nnoremap <buffer> <localleader>1 yypVr=
    au Filetype markdown nnoremap <buffer> <localleader>2 yypVr-
    au Filetype markdown nnoremap <buffer> <localleader>3 yypVr+
    au Filetype markdown nnoremap <buffer> <localleader>4 yypVr*

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

    au FileType python setlocal define=^\s*\\(def\\\\|class\\)
    au FileType man nnoremap <buffer> <cr> :q<cr>

    " Jesus tapdancing Christ, built-in Python syntax, you couldn't let me
    " override this in a normal way, could you?
    au FileType python if exists("python_space_error_highlight") | unlet python_space_error_highlight | endif

    " Send to tmux with localleader e
    au FileType python nnoremap <buffer> <silent> <localleader>e :let python_tslime_view = winsaveview()<cr>vip"ry:call SendToTmux(@r)<cr>:call winrestview(python_tslime_view)<cr>
augroup END

" }}}
" QuickFix {{{

augroup ft_quickfix
    au!
    au Filetype qf setlocal colorcolumn=0 nolist nocursorline nowrap
augroup END

" }}}
" ReStructuredText {{{

augroup ft_rest
    au!

    au Filetype rst nnoremap <buffer> <localleader>1 yypVr=
    au Filetype rst nnoremap <buffer> <localleader>2 yypVr-
    au Filetype rst nnoremap <buffer> <localleader>3 yypVr~
    au Filetype rst nnoremap <buffer> <localleader>4 yypVr`
augroup END

" }}}
" Ruby {{{

augroup ft_ruby
    " Send to tmux with localleader e
    au FileType ruby nnoremap <buffer> <silent> <localleader>e :let ruby_tslime_view = winsaveview()<cr>vip"ry:call SendToTmux(@r)<cr>:call winrestview(ruby_tslime_view)<cr>
augroup END

" }}}
" Java {{{

augroup ft_java
    au!

    au FileType java setlocal tabstop=4 shiftwidth=4 softtabstop=4
    au FileType java setlocal foldmethod=marker foldmarker={,}
    au Filetype java setlocal textwidth=120
    au Filetype java compiler maven
    au Filetype java let b:dispatch = 'mvn -B package install'
augroup END

" }}}
" Scala {{{
augroup ft_scala
    au!

    function! DispatchMavenTest()
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

    au FileType scala setlocal tabstop=2 shiftwidth=2 softtabstop=2
    au Filetype scala setlocal foldmethod=marker foldmarker={,}
    au Filetype scala setlocal textwidth=100
    au Filetype scala compiler maven
    au Filetype scala let b:dispatch = 'mvn -B package install'
    au Filetype scala nnoremap <buffer> <localleader>s mz:%!sort-scala-imports<cr>`z
    au Filetype scala nnoremap <buffer> M :call scaladoc#Search(expand("<cword>"))<cr>
    au Filetype scala vnoremap <buffer> M "ry:call scaladoc#Search(@r)<cr>
    au Filetype scala nnoremap <buffer> <localleader>t :call DispatchMavenTest()<cr>
    au Filetype scala nmap <buffer> <localleader>( ysiwbi
    au Filetype scala nmap <buffer> <localleader>[ ysiwri
    ")]
augroup END

" }}}
" Vagrant {{{

augroup ft_vagrant
    au!
    au BufRead,BufNewFile Vagrantfile set ft=ruby
augroup END

" }}}
" Vim {{{

augroup ft_vim
    au!

    au FileType vim setlocal foldmethod=marker
    au FileType help setlocal textwidth=78
    au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
augroup END

" }}}

" }}}
" Quick editing ----------------------------------------------------------- {{{

nnoremap <leader>ev <C-w>v<C-w>j:e $MYVIMRC<cr>
nnoremap <leader>eh <C-w>v<C-w>j:e ~/.hgrc<cr>
nnoremap <leader>em <C-w>v<C-w>j:e ~/.muttrc<cr>
nnoremap <leader>eb <C-w>v<C-w>j:e ~/.bashrc<cr>

" }}}
" Convenience mappings ---------------------------------------------------- {{{

" Go to previous buffer
nnoremap <leader>b :b#<CR>

" Clean trailing whitespace
nnoremap <leader>w :%s/\s\+$//<cr>:let @/=''<cr>

" Change case
nnoremap <C-u> gUiw
inoremap <C-u> <esc>gUiwea

" Substitute the character under the cursor
nnoremap <leader>s yl:%Subvert/<C-R>0//c<left><left>
" Substitute the word under the cursor
nnoremap <leader>S :%Subvert/<c-r>=expand("<cword>")<cr>//c<left><left>

" Emacs bindings in command line mode
cnoremap <C-a> <home>
cnoremap <C-e> <end>
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <C-d> <Delete>
cnoremap <M-b> <S-Left>
cnoremap <M-f> <S-Right>
cnoremap <M-d> <S-right><Delete>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>
cnoremap <Esc>d <S-right><Delete>
cnoremap <C-g> <C-c>

" Add some bash editing shortcuts
inoremap <C-K> <ESC>lC
inoremap <C-D> <ESC>lxi

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

" Less chording
nnoremap ; :

" Marks and Quotes
noremap ' `
noremap ` '

" Better Completion
set completeopt=longest,menuone,preview
autocmd InsertLeave * if pumvisible() == 0|pclose|endif
inoremap <expr> <Tab> pumvisible() ? "<C-Y>" : "<Tab>"

" Unfuck my screen
nnoremap U :syntax sync fromstart<cr>:redraw!<cr>

" Quickreturn
inoremap <c-cr> <esc>A<cr>
inoremap <s-cr> <esc>A:<cr>

" Toggle [I]nvisible Characters
nnoremap <leader>I :set list!<cr>

" Window close (all) shortcuts
nnoremap <c-w>qq :q<cr>
nnoremap <c-w>qa :qa<cr>

" Vimgrep shortcut
nnoremap <leader>g :vimgrep / */**<left><left><left><left><left>
nnoremap <leader>G :vimgrep /<c-r>=expand("<cword>")<cr> */**

" Forgot to `sudo vim ...` ?
nnoremap <leader>! :w !sudo tee %

"Don't clobber the unnamed register when pasting over text in visual mode.
vnoremap p pgvy

" Ack!!!
nnoremap <leader>a :Ack 
nnoremap <leader>A :Ack <c-r>=expand("<cword>")<cr>

" Move to next line after 'reindent' operation -- IntelliJ style
nnoremap == ==j

" Select (charwise) the contents of the current line, excluding indentation.
" " Great for pasting Python lines into REPLs.
nnoremap vv ^vg_

" TSlime2 general
nnoremap <silent> <localleader>E :ConnectToTmux<cr>
vnoremap <silent> <localleader>e :SendSelectionToTmux<cr>

" Block Colors {{{

let g:blockcolor_state = 0
function! BlockColor() " {{{
    if g:blockcolor_state
        let g:blockcolor_state = 0
        call matchdelete(77880)
        call matchdelete(77881)
        call matchdelete(77882)
        call matchdelete(77883)
    else
        let g:blockcolor_state = 1
        call matchadd("BlockColor1", '^ \{4}.*', 1, 77880)
        call matchadd("BlockColor2", '^ \{8}.*', 2, 77881)
        call matchadd("BlockColor3", '^ \{12}.*', 3, 77882)
        call matchadd("BlockColor4", '^ \{16}.*', 4, 77883)
    endif
endfunction " }}}
nnoremap <leader>B :call BlockColor()<cr>

" }}}
" Insert Mode Completion {{{

inoremap <c-l> <c-x><c-l>
inoremap <c-f> <c-x><c-f>

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

" Ctrl-P {{{

let g:ctrlp_dont_split = 'NERD_tree_2'
let g:ctrlp_jump_to_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_match_window_reversed = 1
let g:ctrlp_split_window = 0
let g:ctrlp_max_height = 20
let g:ctrlp_extensions = ['tag']

let g:ctrlp_prompt_mappings = {
\ 'PrtSelectMove("j")':   ['<c-j>'],
\ 'PrtSelectMove("k")':   ['<c-k>'],
\ 'PrtHistory(-1)':       ['<c-n>'],
\ 'PrtHistory(1)':        ['<c-p>']
\ }

let ctrlp_filter_greps = "".
    \ "egrep -iv '\\.(" .
    \ "jar|class|swp|swo|log|so|o|pyc|jpe?g|png|gif|mo|po" .
    \ ")$' | " .
    \ "egrep -v '^(\\./)?(" .
    \ "deploy/|lib/|classes/|libs/|deploy/vendor/|.git/|.hg/|.svn/|.*migrations/" .
    \ ")'"

let my_ctrlp_user_command = "" .
    \ "find %s '(' -type f -or -type l ')' -maxdepth 15 -not -path '*/\\.*/*' | " .
    \ ctrlp_filter_greps

let my_ctrlp_git_command = "" .
    \ "cd %s && git ls-files | " .
    \ ctrlp_filter_greps

let g:ctrlp_user_command = ['.git/', my_ctrlp_git_command, my_ctrlp_user_command]

nnoremap <C-P> :CtrlP<cr>

" }}}
" DelimitMate {{{

let delimitMate_expand_cr = 1

" }}}
" Easymotion {{{

let g:EasyMotion_leader_key = '<leader>'

" }}}
" Gundo {{{

nnoremap <F5> :GundoToggle<CR>
let g:gundo_debug = 1
let g:gundo_preview_bottom = 1
let g:gundo_tree_statusline = "Gundo"
let g:gundo_preview_statusline = "Gundo Preview"

" }}}
" JK-Jumps {{{
"
let g:jk_jumps_minimum_lines = 2

" }}}
" Powerline {{{

"let g:Powerline_symbols = 'fancy'

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
" Rainbox Parentheses {{{

nnoremap <leader>R :RainbowParenthesesToggle<cr>
let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['black',       'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]
let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 0
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces


" }}}
" SnipMate {{{

imap <C-J> <Plug>snipMateTrigger
imap <C-J> <Plug>snipMateNextOrTrigger
smap <C-J> <Plug>snipMateNextOrTrigger
imap <S-Tab> <Plug>snipMateBack
smap <S-Tab> <Plug>snipMateBack
imap <C-R><C-J> <Plug>snipMateShow

" }}}
" Supertab {{{

let g:SuperTabDefaultCompletionType = 'context'
let g:SuperTabLongestHighlight = 1
let g:SuperTabLongestEnhanced = 1
let g:SuperTabCrMapping = 0
let g:SuperTabClosePreviewOnPopupClose = 1

" }}}
" tslime2 {{{

let g:tslime2_ensure_trailing_newlines = 1

" }}}
" YankRing {{{

let g:yankring_replace_n_pkey = ''
let g:yankring_replace_p_pkey = ''

function! YRRunAfterMaps()
    nnoremap Y :<C-U>YRYankCount 'y$'<CR>
    omap <expr> L YRMapsExpression("", "$")
    omap <expr> H YRMapsExpression("", "^")
endfunction


" }}}
" Dispatch {{{

nnoremap <leader>d :Dispatch<cr>
nnoremap <leader>m :Dispatch<cr>

" }}}
" Maven {{{

let g:maven_disable_mappings = 1

" }}}

" }}}
" Text objects ------------------------------------------------------------ {{{

" Shortcut for [] {{{

onoremap ir i[
onoremap ar a[
vnoremap ir i[
vnoremap ar a[

" }}}
" Next and Last {{{

" Motion for "next/last object".  "Last" here means "previous", not "final".
" Unfortunately the "p" motion was already taken for paragraphs.
"
" Next acts on the next object of the given type in the current line, last acts
" on the previous object of the given type in the current line.
"
" Currently only works for (, [, {, b, r, B, ', and ".
"
" Some examples (C marks cursor positions, V means visually selected):
"
" din'  -> delete in next single quotes                foo = bar('spam')
"                                                      C
"                                                      foo = bar('')
"                                                                C
"
" canb  -> change around next parens                   foo = bar('spam')
"                                                      C
"                                                      foo = bar
"                                                               C
"
" vil"  -> select inside last double quotes            print "hello ", name
"                                                                        C
"                                                      print "hello ", name
"                                                             VVVVVV

onoremap an :<c-u>call <SID>NextTextObject('a', 'f')<cr>
xnoremap an :<c-u>call <SID>NextTextObject('a', 'f')<cr>
onoremap in :<c-u>call <SID>NextTextObject('i', 'f')<cr>
xnoremap in :<c-u>call <SID>NextTextObject('i', 'f')<cr>

onoremap al :<c-u>call <SID>NextTextObject('a', 'F')<cr>
xnoremap al :<c-u>call <SID>NextTextObject('a', 'F')<cr>
onoremap il :<c-u>call <SID>NextTextObject('i', 'F')<cr>
xnoremap il :<c-u>call <SID>NextTextObject('i', 'F')<cr>

function! s:NextTextObject(motion, dir)
  let c = nr2char(getchar())

  if c ==# "b"
      let c = "("
  elseif c ==# "B"
      let c = "{"
  elseif c ==# "r"
      let c = "["
  endif

  exe "normal! ".a:dir.c."v".a:motion.c
endfunction

" }}}

" }}}
" Utils ------------------------------------------------------------------- {{{

" Synstack {{{

" Show the stack of syntax hilighting classes affecting whatever is under the
" cursor.
function! SynStack() "{{{
  echo join(map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")'), " > ")
endfunc "}}}

nnoremap <F7> :call SynStack()<CR>

" }}}

" }}}
" Environments (GUI/Console) ---------------------------------------------- {{{

if has('gui_running')
    set guifont=Monospace\ 9

    " Remove all the UI cruft
    set go-=T
    set go-=l
    set go-=L
    set go-=r
    set go-=R
    set go-=m

    highlight SpellBad term=underline gui=undercurl guisp=Orange

    " Use a line-drawing char for pretty vertical splits.
    set fillchars+=vert:│

    " Different cursors for different modes.
    set guicursor=n-v-c:block-Cursor-blinkon0
    set guicursor+=i-c:ver20-Cursor
else
    " Console Vim
endif

" }}}
" Training -----------------------------------------------------------------{{{

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
" Use CTRL-H like a boss
nnoremap <backspace> <nop>
inoremap <backspace> <nop>
"" Train to use /S/ for splitting lines.
"nnoremap i<CR> <esc>:echo "Use S!"<cr>
"nnoremap a<CR> <esc>:echo "Use S!"<cr>
"nnoremap s<CR> <esc>:echo "Use S!"<cr>
" Train to use <c-e> to move at the end of a line
inoremap <esc>A <esc>:echo "Use <c-e>!"<cr>
" Use Ctrl!
inoremap jj <esc>:echo "Use Ctrl!"<cr>i


" }}}
" Pulse ------------------------------------------------------------------- {{{
function! PulseCursorLine()
	let current_window = winnr()

	windo set nocursorline
	execute current_window . 'wincmd w'

	setlocal cursorline

	redir => old_hi
	silent execute 'hi CursorLine'
	redir END
	let old_hi = split(old_hi, '\n')[0]
	let old_hi = substitute(old_hi, 'xxx', '', '')

	hi CursorLine guibg=#2a2a2a
	redraw
	sleep 30m

	hi CursorLine guibg=#333333
	redraw
	sleep 30m

	hi CursorLine guibg=#3a3a3a
	redraw
	sleep 30m

	hi CursorLine guibg=#444444
	redraw
	sleep 30m

	hi CursorLine guibg=#4a4a4a
	redraw
	sleep 30m

	hi CursorLine guibg=#555555
	redraw
	sleep 30m

	execute 'hi ' . old_hi

	windo set cursorline
	execute current_window . 'wincmd w'
endfunction
" }}}

autocmd BufRead /*svneditor* se norelativenumber
autocmd BufRead /*hgeditor* se norelativenumber
autocmd BufRead *.muttrc se filetype=muttrc
