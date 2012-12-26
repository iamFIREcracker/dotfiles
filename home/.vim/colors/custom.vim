"--------------------------------------------------
" NR-16   NR-8    COLOR NAME 
" 0       0       Black
" 1       4       DarkBlue
" 2       2       DarkGreen
" 3       6       DarkCyan
" 4       1       DarkRed
" 5       5       DarkMagenta
" 6       3       Brown, DarkYellow
" 7       7       LightGray, LightGrey, Gray, Grey
" 8       0*      DarkGray, DarkGrey
" 9       4*      Blue, LightBlue
" 10      2*      Green, LightGreen
" 11      6*      Cyan, LightCyan
" 12      1*      Red, LightRed
" 13      5*      Magenta, LightMagenta
" 14      3*      Yellow, LightYellow
" 15      7*      White
"-------------------------------------------------- 

set background=light
hi clear
if exists("syntax_on")
	syntax reset
endif
hi Constant        cterm=bold    ctermfg=3
hi Comment         cterm=bold    ctermfg=0
hi Statement       cterm=bold    ctermfg=1
hi Function        cterm=bold    ctermfg=6
hi SpecialKey      cterm=bold    ctermfg=0
hi LineNr          cterm=bold    ctermfg=2
hi Visual          term=none   cterm=none    ctermfg=7 ctermbg=5
"--------------------------------------------------
" set background=dark
" hi clear
" if exists("syntax_on")
" 	syntax reset
" endif
" let colors_name = "custom"
" 
" hi PreProc         cterm=none   ctermfg=5
" hi Function        cterm=none   ctermfg=2
" hi Comment         cterm=bold   ctermfg=0
" hi Constant        cterm=none   ctermfg=5
" hi Statement       cterm=bold   ctermfg=2
" hi Type            cterm=bold   ctermfg=2
" 
" hi StatusLine      cterm=bold   ctermfg=7   ctermbg=5
" hi StatusLineNC    cterm=none   ctermfg=0   ctermbg=7
" hi Visual          cterm=bold   ctermfg=7   ctermbg=5
" hi Error           cterm=bold   ctermfg=7   ctermbg=1
" hi SpecialKey      cterm=bold   ctermfg=0
" 
" hi Pmenu           cterm=bold           ctermfg=7   ctermbg=5
" hi PmenuSel        cterm=reverse,bold   ctermfg=7   ctermbg=5
" "--------------------------------------------------
" " "hi ErrorMsg       ctermbg=NONE          ctermfg=red
" " "hi FoldColumn     cterm=standout        ctermfg=NONE
" " "hi Folded         cterm=standout        ctermfg=NONE
" "-------------------------------------------------- 
" "--------------------------------------------------
" " "hi Identifier     ctermfg=Cyan
" " "hi Ignore         ctermfg=black
" " "hi IncSearch      cterm=reverse         ctermfg=NONE
" " hi LineNr          cterm=NONE      ctermfg=8
" " "hi ModeMsg        cterm=bold            ctermfg=NONE
" " "hi NonText        cterm=bold            ctermfg=NONE
" " "hi Operator       ctermfg=Red
" "-------------------------------------------------- 
" "--------------------------------------------------
" " "hi Question       cterm=standout        ctermfg=NONE
" " "hi Repeat         ctermfg=White
" " "hi Search         cterm=underline       ctermbg=3
" " "hi Special        cterm=bold            ctermfg=NONE
" "-------------------------------------------------- 
" "--------------------------------------------------
" " "hi Title          ctermfg=10
" " "hi Todo           ctermbg=Yellow
" "-------------------------------------------------- 
" "--------------------------------------------------
" " "hi Underlined     cterm=underline       ctermfg=NONE
" " "hi VertSplit      cterm=reverse         ctermfg=NONE
" "-------------------------------------------------- 
" "--------------------------------------------------
" " "hi VisualNOS      cterm=bold,underline  ctermfg=NONE
" " "hi WarningMsg     cterm=standout        ctermfg=NONE
" " "hi WildMenu       cterm=standout        ctermfg=NONE
" "--------------------------------------------------
"-------------------------------------------------- 
