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


                     ensure_link "home/bin"                    "bin"
                     ensure_link "home/lib"                    "lib"
                     ensure_link "home/.ackrc"                 ".ackrc"
                     ensure_link "home/.bash_profile"          ".bash_profile"
                     ensure_link "home/.bashrc"                ".bashrc"
                     ensure_link "home/.bashrc_ion"            ".bashrc_ion"
                     ensure_link "home/.ctags"                 ".ctags"
                     ensure_link "home/.gitconfig"             ".gitconfig"
                     ensure_link "home/.gitignore_global"      ".gitignore_global"
                     ensure_link "home/.hgignore"              ".hgignore"
                     ensure_link "home/.hgrc"                  ".hgrc"
                     ensure_link "home/.inputrc"               ".inputrc"
test -n "$OS_WIN" && ensure_link "home/.minttyrc"              ".minttyrc"
                     ensure_link "home/.npmrc"                 ".npmrc"
                     ensure_link "home/.vimfx"                 ".vimfx"
                     ensure_link "home/.pythonrc.py"           ".pythonrc.py"
                     ensure_link "home/.tmuxinator"            ".tmuxinator"
                     ensure_link "home/.tmux.conf"             ".tmux.conf"
                     ensure_link "home/.vim"                   ".vim"
                     ensure_link "home/.vimrc"                 ".vimrc"
