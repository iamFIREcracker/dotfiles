setlocal foldmethod=expr
setlocal foldexpr=GetClojureFold()

let s:export    = '(module\.)?export.*(\(|\{)'
let s:class     = 'class\s+\S+\s*\{'
let s:method    = '(\S*.\S*|if|for|switch)@!\S+\s*\([^)]*\)\s*\{'
let s:function  = 'function(\s+\S+)?\s*\([^)]*\)\s*\{'
let s:router    = 'router.\S+\([^}]*\{'

let s:folded_statements = [
            \ s:export,
            \ s:class,
            \ s:method,
            \ s:function,
            \ s:router
            \ ]

let s:statements_re_bare = '\v^\s*(' . join(s:folded_statements, '|') . ')\s*$'
let s:blankline_re       = '\v^\s*$'

function! s:NextNonBlankLineContents(start)
    let lnum = a:start
    let max = line("$")

    while 1
        let lnum += 1

        " If we've run off the end of the file, return a blank string as
        " a sentinel.
        if lnum > max
            return ""
        endif

        " Otherwise, get the contents.
        let contents = getline(lnum)

        " If they're non-blank, return it.  Otherwise we'll loop to the next
        " line.
        if contents =~ '\v\S'
            return contents
        endif
    endwhile
endfunction

function! GetClojureFold()
    let line = getline(v:lnum)

    if line =~ s:statements_re_bare
        " We're on one of the forms we want to fold.

        let nextline = s:NextNonBlankLineContents(v:lnum)

        " If we've run off the end of the file, this means we're on a top-level
        " form with no later nonblank lines in the file.  This has to be a one
        " liner, because there's no content left that could be closing parens!
        if nextline == ""
            return 0
        elseif nextline =~ '\v^\s+'
            " If it's indented, this almost certainly isn't a one-liner.  Fold
            " away!
            return ">1"
        else
            " Otherwise, the next non-blank line after this one is not
            " indented.  This means we're on a one-liner, so we don't want to
            " fold.
            return 0
        endif
    elseif line =~ '^\s*$'
        " We need to look at the next non-blank line to determine how to fold
        " blank lines.
        let nextline = s:NextNonBlankLineContents(v:lnum)

        " If we've run off the end of the file, this means we're on one of
        " a series of blank lines ending the file.  They shouldn't be folded
        " with anything.
        if nextline == ""
            return 0
        elseif nextline =~ '\v^\s+'
            " If it's indented, we're in the middle of an existing form.
            " Just fold with that.
            return "="
        else
            " Otherwise, the next non-blank line after this one is not
            " indented.  That means we need to close any existing folds
            " here.
            return "<1"
        endif
    elseif line =~ '\v^\s+\S'
        " Indented content, fold it into any existing folds.
        return "="
    else
        " We are sitting on a non-blank, non-indented line, but it's not one of
        " our special top-level forms, so we'll just leave it alone.
        return 0
    endif
endfunction

function! GetJavascriptFold(lnum)
    if getline(a:lnum) =~? s:blankline_re
        " the level of this line is 'undefined'. Vim will interpret this as
        " 'the foldlevel of this line is equal to the foldlevel of the line
        " above or below it, whichever is smaller'.
        return '-1'
    endif

    let this_level = IndentLevel(PrevDefinition(a:lnum))
    let next_level = IndentLevel(PrevDefinition(NextNonBlankLine(a:lnum)))

    if next_level == this_level
        return this_level
    elseif next_level < this_level
        return this_level
    elseif next_level > this_level
        return '>' . next_level
    endif
endfunction

function! PrevDefinition(lnum)
    if getline(a:lnum) =~? '\v^\s*}*\)*;?\s*$'
        return PrevDefinition(PrevNonBlankLine(a:lnum))
    endif

    let lindent = IndentLevel(a:lnum)
    let current = a:lnum

    while current > 0
        let currentindent = IndentLevel(current)
        if currentindent < lindent
            let line = getline(current)
            if line =~? s:statements_re_bare
                return current
            endif
        endif

        let current -= 1
    endwhile

    return -2
endfunction

function! PrevNonBlankLine(lnum)
    let current = a:lnum - 1

    while current > 0
        if getline(current) =~? '\v\S'
            return current
        endif

        let current -= 1
    endwhile

    return -2
endfunction

function! NextNonBlankLine(lnum)
    let numlines = line('$')
    let current = a:lnum + 1

    while current <= numlines
        if getline(current) =~? '\v\S'
            return current
        endif

        let current += 1
    endwhile

    return -2
endfunction

function! IndentLevel(lnum)
    if a:lnum == -2
        return 0
    endif

    return indent(a:lnum) / &shiftwidth + 1
endfunction
