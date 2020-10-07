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
set number
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
set wildignore+=classes                          " Clojure/Leiningen

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

set termguicolors
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
    " autocmd ColorScheme goodwolf
    "         \ hi CursorLineNr cterm=none guifg=#bbbbbb guibg=#af00ff
    " autocmd ColorScheme goodwolf
    "         \ hi! CursorLineNr cterm=none guifg=#af00ff guibg=#141413
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
            \ call GoodWolfHL('CursorLineNr', 'dalespale', '', 'none') |
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
" Resize splits when the window is resized {{{

augroup auto_win_resize
    au!
    au VimResized * :wincmd =
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

    au InsertEnter * let w:save_cwd = getcwd() | set autochdir
    au InsertLeave,WinLeave * if exists('w:save_cwd') | set noautochdir | execute 'lchdir' fnameescape(w:save_cwd) | endif
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

" Open a Quickfix window for the last search.
nnoremap <silent> <leader>/ :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

" Fix linewise visual selection of various text objects
nnoremap VV V
nnoremap Vit vitVkoj
nnoremap Vat vatV
nnoremap Vab vabV
nnoremap VaB vaBV

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

" Fix the & command by making sure substituion flags are not lost when
" re-executing
nnoremap & :&&<CR>
xnoremap & :&&<CR>

" Allows you to easily replace the current word and all its occurrences.
nnoremap <Leader>S :%s/<C-r>=expand("<cword>")<cr>//c<left><left>
vnoremap <Leader>S y:%s/<C-r>"//c<left><left>

" Heresy
inoremap <C-a> <home>
cnoremap <C-a> <home>
inoremap <C-e> <end>
cnoremap <C-e> <end>
inoremap <C-d> <Del>
cnoremap <C-d> <Del>

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

" Select (charwise) the contents of the current line, excluding indentation.
" Great for pasting Python lines into REPLs.
nnoremap vv ^vg_

" Diff mode
nnoremap <localleader>d :windo diffthis<cr>
nnoremap <localleader>D :windo diffoff!<cr>


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
nnoremap <silent> zk zkzv[z
nnoremap <silent> zj zjzv]z

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
" editorconfig-vim {{{

let g:EditorConfig_exclude_patterns = [
         \ 'fugitive://.*',
         \ 'scp://.*'
         \ ]

" }}}
" Fugitive {{{

let g:fugitive_github_domains = []
let g:fugitive_gitlab_domains = ['https://gitlabdev01.iontrading.com']

nnoremap <leader>gd :Shell git diff <C-R>=expand('%')<cr><cr>
nnoremap <leader>gD :Shell git diff<cr>
nnoremap <leader>gp :Gpush<cr>
nnoremap <leader>gP :!git p -f<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>ge :Gedit<cr>
nnoremap <leader>gci :Gcommit<cr>
nnoremap <leader>gr :!git r <C-R>=fnameescape(expand('%'))<cr><cr>
nnoremap <leader>gR :!git R<cr>
nnoremap <leader>gl :Shell git pl <bar> strip-escapes<cr>
nnoremap <leader>gL :Shell git plll <bar> strip-escapes<cr>
nnoremap <leader>gi :Shell git ind<cr>
nnoremap <silent> <leader>y<C-G> :<C-U>call cb#copy(fugitive#Object(@%))<CR>


augroup ft_fugitive " {{{
    au!

    " Copy :GBrowse URLs into the OS clipboard.
    "
    " Note:
    " - `cb` reads from stdin
    " - netrw will pass the URL as argument
    "
    " Hence the use of one-line heredoc (<<<)
    au User Fugitive let g:netrw_browsex_viewer = "cb <<<"
    au BufNewFile,BufRead .git/index setlocal nolist
augroup END " }}}
augroup ft_shell_g_pl " {{{
    au!

    autocmd BufReadPost __Shell_Output__git_pl_* setlocal filetype=gitrebase
    autocmd BufReadPost __Shell_Output__git_pl_* nnoremap <buffer> ri :!git rb <C-R>=split(getline('.'))[0]<CR>^1<CR>:Shell git pl <bar> strip-escapes<cr>
augroup END " }}}
augroup ft_shell_g_pll " {{{
    au!

    autocmd BufReadPost __Shell_Output__git_plll_*  setlocal filetype=gitrebase
    autocmd BufReadPost __Shell_Output__git_plll_* nnoremap <buffer> ri :!git rb <C-R>=split(getline('.'))[0]<CR>^1<CR>:Shell git plll <bar> strip-escapes<cr>
augroup END " }}}

" }}}
" FZF {{{

if $OS == 'Windows_NT'
    packadd fzf-ruby
else
    packadd fzf
end
nnoremap <C-P> :<C-u>FZF<CR>

" }}}
" JK-Jumps {{{

let g:jk_jumps_minimum_lines = 2

" }}}
" Neoformat {{{

if filereadable($PWD .'/node_modules/.bin/prettier')
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
let g:vrc_trigger = '<C-J>'
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
    autocmd FileType vlime_input inoremap <buffer> <buffer> <tab> <c-r>=vlime#plugin#VlimeKey("tab")<cr>
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
" Get current selection {{{

function! GetCurrentSelection(type) " {{{
    let reg_save = @@

    if a:type ==# 'v'
        silent execute "normal! `<" . a:type . "`>y"
    elseif a:type ==# 'V'
        silent execute "normal! `<" . a:type . "`>y"
    elseif a:type ==# 'char'
        silent execute "normal! `[v`]y"
    elseif a:type ==# 'line'
        silent execute "normal! `[V`]y"
    endif
    let selection = @@

    let @@ = reg_save

    return selection
endfunction " }}}

" }}}
" Close on last {{{

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

 " }}}

" }}}
" Plugins wannabe --------------------------------------------------------- {{{

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
    let bufName = substitute(bufName, '|',  '_bar_', 'g')
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
    let escaped = escape(GetCurrentSelection(a:type), '#')
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
let g:stt_command_prompt_regexpes = ['^\s*\$\s*', '^\s*>\s*']
let g:stt_strip_command_prompt = 1
let g:stt_substitute_eol_with = '\n'

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

        let substitute_eol_with =
            \ get(b:, 'stt_substitute_eol_with',
                \ get(g:, 'stt_substitute_eol_with', '\n'))
        let keys = substitute(keys, '\n', substitute_eol_with, 'g')

        call term_sendkeys(g:stt_buffnr, keys)
    endif
endfunction " }}}

function! SendSelectionToTerminal(type) abort " {{{
    call SendToTerminal(GetCurrentSelection(a:type))
endfunction " }}}
function! SelectAndSendToTerminal(motion) abort " {{{
    " Save screen
    let view = winsaveview()

    " Ripped from [vim-glance](https://github.com/arzg/vim-glance/blob/master/autoload/glance.vim)
    " Scrolloff conflicts with the mapping, so we set it to zero after saving
    " its value (for future restoration)
    let s:scrolloffsave = &scrolloff
    set scrolloff=0

    " Select the content of the visible screen.  Couple of notes:
    " 1. Certain commands might fail under certain conditions (e.g. [z will
    "    fail when on the first line of a fold already); `silent!` should take
    "    care of that.
    " 2. We use `normal` instead of `normal!` because we expect users/plugins
    "    to use custom visual eselectors that would not work with the latter
    execute "silent! normal ". a:motion ."\<Esc>"
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
