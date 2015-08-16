"============================================================================
"Compiler:    Mocha
"Maintainer:  Matteo Landi <matteo@matteolandi.net>
"License:     MIT/X11
"============================================================================

" if exists("current_compiler")
"   finish
" endif
let current_compiler = "mocha"

let s:cpo_save = &cpo
set cpo-=C"endif

let &l:makeprg=fnameescape(globpath(&runtimepath, 'compiler/mocha-wrapper.py'))

setlocal errorformat=%n:%f:%l:%m

let &cpo = s:cpo_save
unlet s:cpo_save
