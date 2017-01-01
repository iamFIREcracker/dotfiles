#!/bin/bash

FORCE=1
for i; do
    if [ "$i" == '--force' ]; then
        FORCE=0
    fi
done

WORKDIR="$(pwd)"
OS_WIN=$(uname -s | grep CYGWIN)

set -u
set -e
set -x

function ensure_link {
    test $FORCE -eq 0 && remove "$HOME/$2"
    test -L "$HOME/$2" || create_link "$WORKDIR/$1" "$HOME/$2"
}

function create_link {
    echo "L $2 -> $1"
    ln -s "$1" "$2"
}

function ensure_dir {
    test $FORCE -eq 0 && remove "$HOME/$1"
    test -d "$HOME/$1" || create_dir "$HOME/$1"
}

function remove {
    echo "R $1"
    rm -rf "$1"
}

function create_dir {
    echo "D $1"
    mkdir -p $1
}

(
    cd home/.vim/bundle/omnisharp-vim/server
    if which xbuild 2>/dev/null; then
        xbuild
    elif which msbuild.exe 2>/dev/null; then
        msbuild.exe
    fi
)

(
    cd home/.vim/bundle/vimproc.vim/
    test $FORCE -eq 0 && make clean
    make
)

(
    cd home/.vim/bundle/tern_for_vim/
    test $FORCE -eq 0 && rm -rf node_modules
    npm install
)

(
    cd home/.vim/bundle/tsuquyomi/
    test $FORCE -eq 0 && rm -rf node_modules
    npm install
)


test -z "$OS_WIN" && ensure_dir ".titanium"


                     ensure_link "bin"                    "bin"
                     ensure_link "lib"                    "lib"
                     ensure_link ".ackrc"                 ".ackrc"
                     ensure_link ".bash_profile"          ".bash_profile"
                     ensure_link ".bashrc"                ".bashrc"
                     ensure_link ".bashrc_ion"            ".bashrc_ion"
                     ensure_link ".ctags"                 ".ctags"
                     ensure_link ".gitconfig"             ".gitconfig"
                     ensure_link ".gitignore_global"      ".gitignore_global"
                     ensure_link ".hgignore"              ".hgignore"
                     ensure_link ".hgrc"                  ".hgrc"
                     ensure_link ".inputrc"               ".inputrc"
test -n "$OS_WIN" && ensure_link ".minttyrc"              ".minttyrc"
                     ensure_link ".npmrc"                 ".npmrc"
                     ensure_link ".vimfx"                 ".vimfx"
                     ensure_link ".pythonrc.py"           ".pythonrc.py"
                     ensure_link ".tmuxinator"            ".tmuxinator"
                     ensure_link ".tmux.conf"             ".tmux.conf"
                     ensure_link ".vim"                   ".vim"
                     ensure_link ".vimrc"                 ".vimrc"
