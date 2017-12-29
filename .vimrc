" Preamble ---------------------------------------------------------------- {{{

set nocompatible
filetype off
filetype plugin indent on

" }}}
" Basic options ----------------------------------------------------------- {{{
set encoding=utf-8
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
set termguicolors
" https://www.reddit.com/r/vim/comments/57huhd/any_idea_why_terminal_vim_isnt_correctly/
if &term =~ '256color'
    " Disable Background Color Erase (BCE) so that color schemes
    " render properly when inside 256-color tmux and GNU screen.
    set t_ut=
endif


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

" Clojure/Leiningen
set wildignore+=classes
"set wildignore+=lib

" }}}
" Tabs, spaces, wrapping {{{

set tabstop=4
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
            \ hi NeomakeErrorSign ctermfg=red ctermbg=235 |
            \ hi NeomakeWarningSign ctermfg=yellow ctermbg=235
    autocmd ColorScheme goodwolf
            \ hi! link DiffAdd diffAdded |
            \ hi! link DiffDelete diffRemoved |
            \ call GoodWolfHL('DiffText', 'orange', 'deepergravel', 'none')
augroup END

colorscheme goodwolf

" }}}
" Auttogroups {{{

" Save when losing focus {{{

augroup auto_save
    au!

    au FocusLost * :wa
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

inoreabbr ml@ matteo@matteolandi.net
inoreabbr lenght length
inoreabbr mainteinance maintainance

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
map <tab> %

" Keep search matches in the middle of the window
nnoremap n nzvzz
nnoremap N Nzvzz

" Same when jumping around
nnoremap g; g;zvzz
nnoremap g, g,zvzz
nnoremap gd gdzvzz
nnoremap gD gDzvzz

" Window resizing
nnoremap <c-left> 5<c-w>>
nnoremap <c-right> 5<c-w><

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

"" Smarcase for */# -- and don't automatically jump around {{{

nnoremap * :let stay_star_view = winsaveview()<cr>/\<<C-R>=expand('<cword>')<CR>\><CR>:call winrestview(stay_star_view)<cr>
nnoremap # :let stay_star_view = winsaveview()<cr>?\<<C-R>=expand('<cword>')<CR>\><CR>:call winrestview(stay_star_view)<cr>

"" }}}
" Directional Keys {{{

" It's 2011.
noremap <silent> <expr> j (v:count == 0 ? 'gj' : 'j')
noremap <silent> <expr> k (v:count == 0 ? 'gk' : 'k')

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

" Automatic unfolding {{{
" Fix automatic unfolding while entering insert mode
" http://stackoverflow.com/questions/4630892/vim-folds-open-up-when-giving-an-unmatched-opening-brace-parenthesis
augroup unfolding
    au!
    au InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
    au InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif
augroup END

" }}}
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

" Save
nnoremap S :w<cr>

" }}}
" Various filetype-specific stuff ----------------------------------------- {{{

" Angular {{{

augroup ft_angular
    au!

    function! CheckIfAngularProject() " {{{
        return filereadable(".angular-cli.json")
    endfunction " }}}
    function! InitAngularMappings() " {{{
        nnoremap <localleader>Ns  :Dispatch! ng serve<cr>
        nnoremap <localleader>Ngc :Dispatch ng generate component --spec false<space>
        nnoremap <localleader>Ngd :Dispatch ng generate directive --spec false<space>
    endfunction " }}}
    au VimEnter *
                \ if CheckIfAngularProject()
                \ | call InitAngularMappings()
                \ | endif

    au BufNewFile,BufRead *.template.html,*.tpl.html,*.component.html
                \ if CheckIfAngularProject()
                \ | setlocal filetype=angular_html
                \ | endif

    function! DirectiveUnderCursor() " {{{
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
    function! FindDirectiveOccurrences() " {{{
        let directive = DirectiveUnderCursor()

        exe "normal! :Ack! --literal " . directive . "\<cr>"
    endfunction "}}}
    function! GuessDirectiveFilename() " {{{
        let inex_save = &inex

        function! FilenameOfDirectiveUnderCursor() " {{{
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
" C# {{{

augroup ft_csharp
    au!
    au FileType cs setlocal foldmethod=marker foldmarker={,}
    au FileType cs setlocal ts=4 sts=4 sw=4 noexpandtab

    au FileType cs nnoremap <c-]> :OmniSharpGotoDefinition<cr>
    au FileType cs nnoremap <C-^> :OmniSharpFindUsages<cr>
augroup END

" }}}
" Common Lisp {{{

augroup ft_commonlisp
    au!

    function! OpenLispReplSBCL() "{{{
        call term_start("bash -c sbcl-vlime", {
            \ "term_finish": "close",
            \ "vertical": 1
        \ })
    endfunction "}}}

    au FileType lisp RainbowParenthesesActivate
    au syntax lisp RainbowParenthesesLoadRound

    " Force omnicompletion (vlime's)
    au FileType lisp inoremap <c-n> <c-x><c-o>

    au FileType lisp nnoremap <buffer> <silent> <localleader>Os :call OpenLispReplSBCL()<cr>
augroup END

" }}}
" CSS, SCSS, and LessCSS {{{

augroup ft_css
    au!

    au BufNewFile,BufRead *.less setlocal filetype=less

    au FileType less,css,scss setlocal ts=2 sw=2 sts=2
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
" Diff {{{

augroup ft_diff
    au!

    function! DiffFold(lnum)
        let line = getline(a:lnum)
        if line =~ '^\(diff\|---\|+++\|@@\) '
            return 0
        elseif line[0] =~ '[-+ ]'
            return 1
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
augroup END

" }}}
" Erlang {{{

augroup ft_erlang
    au!

    " Send to tmux with localleader e
    au FileType erlang nnoremap <buffer> <silent> <localleader>e :let erlang_tslime_view = winsaveview()<cr>vip"ry:call SendToTmux(@r)<cr>:call winrestview(erlang_tslime_view)<cr>

    " Abbreviations {{{

    au FileType erlang call MakeSpacelessBufferIabbrev('mod',  '-module().<left><left>')
    au FileType erlang call MakeSpacelessBufferIabbrev('cpl',  '-compile().<left><left>')
    au FileType erlang call MakeSpacelessBufferIabbrev('rec',  'receive<cr>end.<up><end>')
    au FileType erlang call MakeSpacelessBufferIabbrev('aft',  'after  -><left><left><left>')

    " }}}
augroup END

" }}}
" Eslintrc {{{

augroup ft_eslintrc
    au!

    au BufNewFile,BufRead .eslintrc nnoremap <buffer> q :bd<cr>
augroup END

" }}}
" HTML {{{

augroup ft_html
    au!

    au BufNewFile,BufRead *.html setlocal filetype=htmldjango
    au FileType html,jinja,htmldjango setlocal ts=2 sw=2 sts=2
    au FileType html,jinja,htmldjango setlocal foldmethod=manual
    au FileType html,jinja,htmldjango EmmetInstall

    " Invoke emmet instead of supertab
    au FileType html,jinja,htmldjango imap <buffer> <expr> <c-n> emmet#expandAbbrIntelligent("\<c-n>")

    function! HtmlWrap() " {{{
        " Back up x registry
        let old_x = @x

        " Copy html element to x registry
        normal! vi<"xy

        if len(split(@x, '\n', 0)) != 1
            " Unwrap
            normal! vi<J
        else
            " Wrap
            let @x = substitute(@x, '\v\s*(\*?\[?\(?\w+\)?\]?)\=', '\n\1\=', 'g')

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

    " Use Shift-Return (✝) in insert mode to turn this:
    "     <tag>|</tag>
    "
    " into this:
    "     <tag>
    "         |
    "     </tag>
    au FileType html,jinja,htmldjango inoremap <buffer> ✝ <esc>cit<cr><esc>ko
    " Use Shift-Return (✝) in normal mode to turn this:
    "     <tag>something|else</tag>
    "
    " into this:
    "     <tag>
    "         |something else
    "     </tag>
    au FileType html,jinja,htmldjango nnoremap <buffer> ✝ <esc>vit<esc>a<cr><esc>vito<esc>i<cr><esc>
augroup END

" }}}
" Grunt-ion {{{

augroup ft_gruntion
    au!

    function! DetectGruntIon()
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
" Jade {{{

augroup ft_jade
    au!

    au FileType jade setlocal tabstop=2 shiftwidth=2 softtabstop=2
augroup END

" }}}
" Java {{{

augroup ft_java
    au!
    function! TurnOnJavaFolding() "{{{
        let modifier     = '%(public|private|protected)*\s*'
        let returntype   = '' " XXX to implement
        let class        = modifier.'class%(\s+\S+)*\s*\{'
        let method       = modifier.returntype.'%(\S*\.\S*|if|for|while|switch)@!\S+\s*\([^)]*\)\s*\{'
        let functionwrap = '\s*[a-zA-Z0-9:]*\S*\)\s*\{'

        let folded_statements = [
                    \ class,
                    \ method,
                    \ functionwrap
                    \ ]

        let b:manual_regexp_folding_statements_re_bare = '\v^\s*%(' . join(folded_statements, '|') . ')\s*$'
        call TurnOnManualRegexpFolding()
    endfunction "}}}
    au FileType java silent! call TurnOnJavaFolding()
    au FileType java nnoremap <buffer> <localleader>F :call UpdateManualRegexpFolds()<cr>


    au FileType java setlocal omnifunc=javacomplete#Complete
    au FileType java setlocal tabstop=2 shiftwidth=2 softtabstop=2
    au Filetype java setlocal textwidth=120
    au Filetype java compiler maven
    au Filetype java let b:dispatch = 'mvn -B clean test'
augroup END

" }}}
" Javascript {{{

augroup ft_javascript
    au!

    function! TurnOnJavascriptFolding() "{{{
        let export       = '%(module\.)?export(s)?%(\.)?.*\{'
        let class        = 'class%(\s+\S+)*\s*\{'
        let method       = '%(\S*\.\S*|if|for|switch)@!\S+\s*\([^)]*\)\s*\{'
        let functionwrap = '\s*[a-zA-Z0-9:]*\S*\)\s*\{'
        let functiondec  = 'function%(\s+\S+)?\s*\([^)]*' . functionwrap
        let functiondef  = '%(%(const|var|let)\s)?\S+\s*\=\s*' . functiondec
        let arrowdefwrap = '\s*[a-zA-Z0-9:]*\)\s*\=\>\s*\{'
        let arrowdef     = '%(%(const|var|let)\s)?\S+\s*\=\s*\([^)]*' . arrowdefwrap
        let router       = 'router\.\S+\([^}]*\{'
        let mocha_descr  = 'describe\([^}]*\{'
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
    au FileType javascript nnoremap <buffer> <localleader>F :call UpdateManualRegexpFolds()<cr>

    au FileType javascript setlocal suffixesadd+=.js
    au FileType javascript setlocal ts=2 sw=2 sts=2
    au Filetype javascript setlocal textwidth=100

    au FileType javascript nnoremap <buffer> <silent> <C-]> :TernDef<cr>zvzz
    au FileType javascript nnoremap <buffer> <silent> gd :TernDef<cr>zvzz
    au FileType javascript nnoremap <buffer> <silent> <C-^> :TernRefs<cr>

    au Filetype javascript nnoremap <buffer> <leader>d :call RunAllSpecs()<cr>
    au Filetype javascript nnoremap <buffer> <localleader>t :call RunNearestSpec()<cr>

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
    au FileType javascript call MakeSpacelessBufferIabbrev('pclog', 'console.log(require("util").inspect(HERE,{showHidden:false,depth:null}));')
    au FileType javascript call MakeSpacelessBufferIabbrev('dolog', 'do(console.log)')
    au FileType javascript call MakeSpacelessBufferIabbrev('maplog', 'map(e => console.log(e) \|\| e)')
    au FileType javascript call MakeSpacelessBufferIabbrev('thenlog', 'then(e => console.log(e) \|\| e)')

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

    au FileType json setlocal ts=2 sw=2 sts=2
    au FileType json setlocal foldnestmax=99
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

    au Filetype markdown setlocal spell
    au FileType markdown let b:delimitMate_nesting_quotes = ['`']

    " Use <localleader>1/2/3 to add headings.
    au Filetype markdown nnoremap <buffer> <localleader>1 yypVr=
    au Filetype markdown nnoremap <buffer> <localleader>2 yypVr-
    au Filetype markdown nnoremap <buffer> <localleader>3 yypVr+
    au Filetype markdown nnoremap <buffer> <localleader>4 yypVr*

augroup END

" }}}
" Npm {{{

augroup ft_npm
    au!

    function! CheckIfNpmProject() " {{{
        return filereadable("package.json")
    endfunction " }}}
    function! InitNpmMappings() " {{{
        nnoremap <localleader>nr  :Dispatch npm run<space>
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

    let b:tslime_ensure_trailing_newlines = 2

    au FileType python setlocal define=^\s*\\(def\\\\|class\\)

    " Jesus tapdancing Christ, built-in Python syntax, you couldn't let me
    " override this in a normal way, could you?
    au FileType python if exists("python_space_error_highlight") | unlet python_space_error_highlight | endif

    " Send to tmux with localleader e
    au FileType python nnoremap <buffer> <silent> <localleader>e :let python_tslime_view = winsaveview()<cr>vip"ry:call SendToTmux(@r)<cr>:call winrestview(python_tslime_view)<cr>

    au FileType python let b:delimitMate_nesting_quotes = ['"']

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
    au FileType qf nnoremap <buffer> q :bd<cr>
augroup END

" }}}
" REST {{{

augroup ft_rest
    au!

    au FileType rest setlocal ts=2 sw=2 sts=2
    au Filetype rest setlocal foldmethod=marker foldmarker={{{,}}}
    au Filetype rest nnoremap <buffer> <localleader>1 yypVr=
augroup END

" }}}
" ReStructuredText {{{

augroup ft_rst
    au!

    au Filetype rst nnoremap <buffer> <localleader>1 yypVr=
    au Filetype rst nnoremap <buffer> <localleader>2 yypVr-
    au Filetype rst nnoremap <buffer> <localleader>3 yypVr~
    au Filetype rst nnoremap <buffer> <localleader>4 yypVr`
augroup END

" }}}
" Robot {{{

augroup ft_robot
    au!

    au FileType robot setlocal noexpandtab

augroup END

" }}}
" Ruby {{{

augroup ft_ruby
    au!

    " Send to tmux with localleader e
    au FileType ruby nnoremap <buffer> <silent> <localleader>e :let ruby_tslime_view = winsaveview()<cr>vip"ry:call SendToTmux(@r)<cr>:call winrestview(ruby_tslime_view)<cr>
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
    ")]
augroup END

" }}}
" SQL {{{

augroup ft_sql
    au!

    au FileType sql setlocal foldmethod=marker
    au FileType sql setlocal foldmarker={{{,}}}
    "
    " Send to tmux with localleader e
    au FileType sql nnoremap <buffer> <silent> <localleader>e :let sql_tslime_view = winsaveview()<cr>vip"ry:call SendToTmux(@r)<cr>:call winrestview(sql_tslime_view)<cr>

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

    function! TurnOnTypescriptFolding() "{{{
        let export       = '%(module\.)?export(s)?%(\.)?.*\{'
        let class        = 'class%(\s+\S+)*\s*\{'
        let method       = '%(\S*\.\S*|if|for|switch)@!\S+\s*\([^)]*\)\s*\{'
        let functionwrap = '\s*[a-zA-Z0-9:]*\S*\)\s*\{'
        let functiondec  = 'function%(\s+\S+)?\s*\([^)]*' . functionwrap
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
    au FileType typescript nnoremap <buffer> <localleader>F :call UpdateManualRegexpFolds()<cr>

    au FileType typescript setlocal ts=2 sw=2 sts=2
    au FileType typescript setlocal suffixesadd+=.ts

    au FileType typescript nnoremap <buffer> <silent> <C-]> :TsuDefinition<cr>zvzz
    au FileType typescript nnoremap <buffer> <silent> gd :TsuDefinition<cr>zvzz

    au FileType typescript nnoremap <buffer> <silent> ,S :TsuRenameSymbol<cr>

    " Force omnicompletion (tsu's)
    au FileType typescript inoremap <c-n> <c-x><c-o>

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
    au FileType typescript call MakeSpacelessBufferIabbrev('console', 'NOPENOPENOPE')

    " }}}
augroup END

" }}}
" Ternproject {{{

augroup ft_ternproject
    au!

    au BufNewFile,BufRead .tern-project nnoremap <buffer> q :bd<cr>
augroup END

" }}}
" Vim {{{

augroup ft_vim
    au!

    au FileType vim setlocal foldmethod=marker
    au FileType help setlocal textwidth=78
    au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
    au BufEnter *.txt if &ft == 'help' | nnoremap <buffer> q :bd<cr> | endif
augroup END

" }}}
" XML {{{

augroup ft_xml
    au!

    au FileType xml setlocal ts=2 sw=2 sts=2
    au FileType xml setlocal foldmethod=manual

    " Use <localleader>f to fold the current tag.
    au FileType xml nnoremap <buffer> <localleader>f Vatzf

    " Indent tag
    au FileType xml nnoremap <buffer> <localleader>= Vat=
augroup END

" }}}
" YAML {{{

augroup ft_yaml
    au!

    au FileType yaml setlocal ts=2 sw=2 sts=2
augroup END

" }}}

" }}}
" Quick editing ----------------------------------------------------------- {{{

nnoremap <leader>eb :vsplit ~/.bashrc<cr>
nnoremap <leader>eg :vsplit ~/.gitconfig<cr>
nnoremap <leader>eh :vsplit ~/.hgrc<cr>
nnoremap <leader>em :vsplit <C-R>=system('tempfile .mail')<cr><cr>
nnoremap <leader>et :vsplit ~/.tmux.conf<cr>
nnoremap <leader>ev :vsplit $MYVIMRC<cr>

" }}}
" Quick reload ------------------------------------------------------------ {{{

nnoremap <leader>sv :let stay_sourcevimrc_view = winsaveview()<cr>:source $MYVIMRC<cr>:call winrestview(stay_sourcevimrc_view)<cr>

" }}}
" Convenience mappings ---------------------------------------------------- {{{

" Clean trailing whitespace
nnoremap <leader>w :%s/\s\+$//<cr>:let @/=''<cr>

" Change case
nnoremap <C-u> gUiw
inoremap <C-u> <esc>gUiwea

" Fix the & command by making sure substituion flags are not lost when
" re-executing
nnoremap & :&&<CR>
xnoremap & :&&<CR>

" Allows you to easily replace the current word and all its occurrences.
nnoremap <C-S> :%s/
vnoremap <C-S> :s/
nnoremap <Leader>S :%s/<C-r><C-w>//cw<left><left><left>
vnoremap <Leader>S y:%s/<C-r>"//cw<left><left><left>

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

" Skip automatically to next difference
nnoremap do do]c
nnoremap dp dp]c

" Formatting, TextMate-style
nnoremap Q gqip
vnoremap Q gq

function! g:FuckingCopyTheTextPlease()
    let view = winsaveview()
    let old_z = @z
    normal! gv"zy
    call system('cb', @z)
    let @z = old_z
    call winrestview(view)
endfunction

function! g:FuckingCopyAllTheTextPlease()
    let view = winsaveview()
    let old_z = @z
    normal! ggVG"zy
    call system('cb', @z)
    let @z = old_z
    call winrestview(view)
endfunction

vnoremap <leader>y :<c-u>call g:FuckingCopyTheTextPlease()<cr>
nnoremap <leader>y VV:<c-u>call g:FuckingCopyTheTextPlease()<cr>
nnoremap <leader>Y :<c-u>call g:FuckingCopyAllTheTextPlease()<cr>

nnoremap <leader>p :set paste<CR>:read !cb<CR>:set nopaste<CR><leader>V=
nnoremap <leader>P O<esc>:set paste<CR>:read !cb<CR>:set nopaste<CR>kdd

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

" Quickreturn -- on my terminal, C-CR generates ✠
inoremap ✠ <esc>A<cr>


" Toggle [I]nvisible Characters
nnoremap <leader>I :set list!<cr>

" Window close (all) shortcuts
nnoremap <c-w>qq :q<cr>
nnoremap <c-w>qa :qa<cr>

" Forgot to `sudo vim ...` ?
nnoremap <leader>! :w !sudo tee %

" Select (charwise) the contents of the current line, excluding indentation.
" " Great for pasting Python lines into REPLs.
nnoremap vv ^vg_

" TSlime general
nmap <silent> <localleader>E <Plug>SetTmuxVars
vmap <silent> <localleader>e <Plug>SendSelectionToTmux

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

"JSON prettify mappings
nnoremap <leader>J :%!python -m json.tool<cr>
xnoremap <leader>J :'<,'>!python -m json.tool<cr>

" Easy expansion of the Active File Directory
cnoremap <expr> %% getcmdtype() == ':' ? expand('%') : '%%'
cnoremap <expr> %d getcmdtype() == ':' ? expand('%:h').'/' : '%%'

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
inoremap <c-space> <c-x><c-o>
inoremap <c-@> <c-x><c-o>

" }}}
" Consistent navigation of buffers, quickfixes, locations -- borrowed from vim-unimpaired {{{

" Buffers
nnoremap <silent> [b :bprevious<CR>
nnoremap <silent> ]b :bnext<CR>
" Quickfix
nnoremap <silent> [q :cprevious<CR>zvzz
nnoremap <silent> ]q :cnext<CR>zvzz
" Older quickfix invocations
nnoremap <silent> [Q :colder<CR>
nnoremap <silent> ]Q :cnewer<CR>
" Locations
nnoremap <silent> [l :lprevious<CR>zvzz
nnoremap <silent> ]l :lnext<CR>zvzz
" Folds
nnoremap <silent> [z zMzkzvzz
nnoremap <silent> ]z zMzjzvzz

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

nnoremap <leader>a :Ack!<space>
nnoremap <localleader>a :Ack!  %:h<left><left><left><left>
let g:ackprg = 'ag --hidden --smart-case --nogroup --nocolor --column'

" }}}
" Airline {{{

let g:airline_powerline_fonts = 1

" }}}
" Argwrap {{{

let g:argwrap_tail_comma_braces='[{'

" }}}
" Ctrl-P {{{

let g:ctrlp_jump_to_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_match_window = 'bottom,order:btt,max:20'

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
    \ "deploy/|classes/|libs/|deploy/vendor/|.git/|.hg/|.svn/|.*migrations/|node_modules/" .
    \ ")'"

let my_ctrlp_user_command = "" .
    \ "ag %s -l --nocolor -g '' | " .
    \ ctrlp_filter_greps

let my_ctrlp_git_command = "" .
    \ "cd %s && git ls-files | " .
    \ ctrlp_filter_greps

let my_ctrlp_svn_command = "" .
    \ "cd %s && svn ls -R | " .
    \ ctrlp_filter_greps

let g:ctrlp_user_command = {
\ 'types': {
    \ 1: ['.git', my_ctrlp_git_command],
    \ 2: ['.svn', my_ctrlp_svn_command],
\ },
\ 'fallback': my_ctrlp_user_command
\ }

nnoremap <C-P> :CtrlP<cr>
nnoremap Ř :CtrlPRoot<cr>

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
" Emmet {{{

let g:user_emmet_install_global = 0

" }}}
" Fugitive {{{

let g:fugitive_github_domains = ['github.banksimple.com']

nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gD :Shell git diff<cr>
nnoremap <leader>gp :Gpush<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>ga :Gadd<cr>
nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>ge :Gedit<cr>
nnoremap <leader>gco :Gcheckout<cr>
nnoremap <leader>gci :Gcommit<cr>
nnoremap <leader>gm :Gmove<cr>
nnoremap <leader>gr :Git ra<cr>
nnoremap <leader>gR :Gremove<cr>
nnoremap <leader>gl :Shell git ll<cr>
nnoremap <leader>gi :Shell git ind<cr>

augroup ft_fugitive
    au!

    au BufNewFile,BufRead .git/index setlocal nolist
augroup END
augroup ft_shell_g_ll
    au!

    autocmd BufReadPost __Shell_Output__git_ll :silent call RemoveAnsiColor()
    autocmd BufReadPost __Shell_Output__git_ll setlocal filetype=gitrebase
    function! GitRebase() "{{{
        setlocal keywordprg=git\ rebase\ --interactive
        Git shelve
        normal! j " Move to the next commit
        normal! K " Do rebase
        Git unshelve
        Shell git ll
    endfunction "}}}
    autocmd BufReadPost __Shell_Output__git_ll nnoremap <buffer> R :call GitRebase()<CR>:Shell git ll<cr>
augroup END

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

let g:maven_disable_mappings = 1

" }}}
" Neomake {{{

let g:neomake_open_list = 0
let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_javascript_eslint_exe = $PWD .'/node_modules/.bin/eslint'
let g:neomake_typescript_enabled_makers = []

" autocmd! BufRead * Neomake
autocmd! BufWritePost * Neomake

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

" }}}
" OmniSharp {{{

let g:OmniSharp_selector_ui = 'ctrlp'

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
" tslime {{{

let g:tslime_ensure_trailing_newlines = 1

" }}}
" Tsuquyomi {{{

let g:tsuquyomi_use_vimproc=1
let g:tsuquyomi_disable_default_mappings=1
let g:tsuquyomi_use_dev_node_module=2
let g:tsuquyomi_tsserver_path=$PWD .'/node_modules/.bin/tsserver'

" }}}
" Supertab {{{

let g:SuperTabDefaultCompletionType = "<c-n>"
let g:SuperTabLongestHighlight = 1
let g:SuperTabCrMapping = 1

" }}}
" vim-Mocha {{{

let g:mocha_js_command =
            \ 'Dispatch -compiler=mocha-wrapper ' .
            \ fnameescape(globpath(&runtimepath, 'compiler/mocha-wrapper.py')) .
            \ ' --recursive --no-colors {spec}'

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
let g:vrc_trigger = '<localleader>e'

augroup ft_restresponse
    au!

    autocmd BufNewFile __REST_response__ nnoremap <buffer> q :bd<cr>
augroup END

" }}}
" vim-visual-star-search {{{

xnoremap * :<C-u>call VisualStarSearchSet('/')<CR>/<C-R>=@/<CR><CR><C-O>
xnoremap # :<C-u>call VisualStarSearchSet('?')<CR>?<C-R>=@/<CR><CR><C-O>

" }}}
" vim-yankstack {{{

function! YankStackAfterSetup()
    nmap Y y$
    nmap D d$
    nmap gp <Plug>yankstack_substitute_older_paste
    nmap gP <Plug>yankstack_substitute_newer_paste
endfunction

let g:yankstack_after_setup = 'YankStackAfterSetup'

" }}}
" vlime {{{

set rtp+=~/my-env/opt/vlime/vim

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
function! SynStack() "{{{
  echo join(map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")'), " > ")
endfunc "}}}

nnoremap <F7> :call SynStack()<CR>

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
else
    " Mouse support
    set mouse=a
endif

" }}}
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
    let bufName = g:ShellOutputBufferName . substitute(a:command, ' ', '_', '')
    let shl_bufnum = bufnr(bufName)
    if shl_bufnum == -1
        " open a new shell buffer
        exe "vnew " . bufName
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
            exe "vsplit +buffer" . shl_bufnum
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
    autocmd BufNewFile __Shell_Output__* nnoremap <buffer> q :bd<cr>
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
" Ack motions {{{

" Motions to Ack for things.  Works with pretty much everything, including:
"
"   w, W, e, E, b, B, t*, f*, i*, a*, and custom text objects
"
" Awesome.
"
" Note: If the text covered by a motion contains a newline it won't work.  Ack
" searches line-by-line.

" nnoremap <silent> <leader>A :set opfunc=<SID>AckMotion<CR>g@
" vnoremap <silent> <leader>A :<C-U>call <SID>AckMotion(visualmode())<CR>
" xnoremap <silent> <leader>A :<C-U>call <SID>AckMotion(visualmode())<CR>
"
nnoremap <silent> <leader>* :Ack! '\b<c-r><c-w>\b'<cr>
vnoremap <silent> <leader>* :<C-U>call <SID>AckMotion(visualmode())<CR>
xnoremap <silent> <leader>* :<C-U>call <SID>AckMotion(visualmode())<CR>

nnoremap <silent> <localleader>* :Ack! '\b<c-r><c-w>\b' %:h<CR>
vnoremap <silent> <localleader>* :<C-U>call <SID>AckLocalMotion(visualmode())<CR>
xnoremap <silent> <localleader>* :<C-U>call <SID>AckLocalMotion(visualmode())<CR>

function! s:CopyMotionForType(type)
    if a:type ==# 'v'
        silent execute "normal! `<" . a:type . "`>y"
    elseif a:type ==# 'char'
        silent execute "normal! `[v`]y"
    endif
endfunction

function! s:AckMotion(type) abort
    let reg_save = @@

    call s:CopyMotionForType(a:type)

    execute "normal! :Ack! --literal " . shellescape(@@) . "\<cr>"

    let @@ = reg_save
endfunction

function! s:AckLocalMotion(type) abort
    let reg_save = @@

    call s:CopyMotionForType(a:type)

    execute "normal! :Ack! --literal " . shellescape(@@) . " " . expand('%:h') . "\<cr>"

    let @@ = reg_save
endfunction

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
    let command.= 'normal $zf%zo'
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

" }}}
