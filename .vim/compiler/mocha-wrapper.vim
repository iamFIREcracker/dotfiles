"============================================================================
"Compiler:    mocha-wrapper
"Maintainer:  Matteo Landi <matteo@matteolandi.net>
"License:     MIT/X11
"============================================================================

if exists("current_compiler")
    finish
endif
let current_compiler = "mocha-wrapper"

let s:cpo_save = &cpo
set cpo-=C

let &makeprg=fnameescape(globpath(&runtimepath, 'compiler/mocha-wrapper.py'))
let &errorformat='%n:%f:%l:%m'

let &cpo = s:cpo_save
unlet s:cpo_save
