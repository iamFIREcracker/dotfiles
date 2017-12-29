#!/usr/bin/env bash

FORCE=0
for i; do
    if [ "$i" == '--force' ]; then
        FORCE=1
    fi
done

WORKDIR="$(pwd)"
OS_MAC=$(uname -s | grep Darwin)
OS_WIN=$(uname -s | grep CYGWIN)

set -u
set -e
set -x

function ensure_link {
    test $FORCE -eq 1 && remove "$HOME/$2"
    test -L "$HOME/$2" || create_link "$WORKDIR/$1" "$HOME/$2"
}

function create_link {
    echo "L $2 -> $1"
    ln -s "$1" "$2"
}

function ensure_dir {
    test $FORCE -eq 1 && remove "$HOME/$1"
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
    cd .vim/pack/bundle/start/omnisharp-vim/server
    if which xbuild 2>/dev/null; then
        xbuild
    elif which msbuild.exe 2>/dev/null; then
        msbuild.exe
    fi
)

(
    if ! which make 2>/dev/null; then
        echo "Missing command: make"
    else
        cd .vim/pack/bundle/start/vimproc.vim/
        test $FORCE -eq 1 && make clean
        make
    fi
)

(
    if ! which npm 2>/dev/null; then
        echo "Missing command: npm"
    else
        cd .vim/pack/bundle/start/tern_for_vim/
        test $FORCE -eq 1 && rm -rf node_modules
        npm install
    fi
)

(
    if ! which npm 2>/dev/null; then
        echo "Missing command: npm"
    else
        cd .vim/pack/bundle/start/tsuquyomi/
        test $FORCE -eq 1 && rm -rf node_modules
        npm install
    fi
)

(
    if ! which mvn 2>/dev/null; then
        echo "Missing command: mvn"
    elif ! which javac 2>/dev/null; then
        echo "Missing command: javac"
    else
        cd .vim/pack/bundle/start/vim-javacomplete2/libs/javavi/
        mvn compile
    fi
)

test -z "$OS_WIN" && ensure_dir ".titanium"


                     ensure_link "bin"                    "bin"
                     ensure_link "lib"                    "lib"
                     ensure_link ".agignore"              ".agignore"
                     ensure_link ".bash_profile"          ".bash_profile"
                     ensure_link ".bashrc"                ".bashrc"
                     ensure_link ".bashrc_ion"            ".bashrc_ion"
                     ensure_link ".ctags"                 ".ctags"
                     ensure_link ".gitconfig"             ".gitconfig"
                     ensure_link ".gitignore_global"      ".gitignore_global"
                     ensure_link ".hgignore"              ".hgignore"
                     ensure_link ".hgrc"                  ".hgrc"
                     ensure_link ".inputrc"               ".inputrc"
test -n "$OS_MAC" && ensure_link ".config/karabiner"      ".config/karabiner"
test -n "$OS_WIN" && ensure_link ".minttyrc"              ".minttyrc"
                     ensure_link ".npmrc"                 ".npmrc"
                     ensure_link ".pythonrc.py"           ".pythonrc.py"
test -n "$OS_MAC" && ensure_link ".slate"                 ".slate"
                     ensure_link ".tmuxinator"            ".tmuxinator"
                     ensure_link ".tmux-plugins"          ".tmux-plugins"
                     ensure_link ".tmux.conf"             ".tmux.conf"
                     ensure_link ".vim"                   ".vim"
                     ensure_link ".vimfx"                 ".vimfx"
                     ensure_link ".vimrc"                 ".vimrc"
