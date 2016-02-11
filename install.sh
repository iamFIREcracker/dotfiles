#!/bin/bash

test "$1" == "--force"
FORCE=$?

WORKDIR="$(pwd)"
OS_MAC=$(uname -s | grep Darwin)
OS_WIN=$(uname -s | grep CYGWIN)

set -u
set -e

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


test -z "$OS_WIN" && ensure_dir ".titanium"


                     ensure_link "home/bin"                    "bin"
                     ensure_link "home/lib"                    "lib"
                     ensure_link "home/opt"                    "opt"
                     ensure_link "home/.ackrc"                 ".ackrc"
                     ensure_link "home/.bash_profile"          ".bash_profile"
                     ensure_link "home/.bashrc"                ".bashrc"
                     ensure_link "home/.ctags"                 ".ctags"
                     ensure_link "home/.gitconfig"             ".gitconfig"
                     ensure_link "home/.gitignore_global"      ".gitignore_global"
                     ensure_link "home/.hgignore"              ".hgignore"
                     ensure_link "home/.hgrc"                  ".hgrc"
test -n "$OS_WIN" && ensure_link "home/.minttyrc"              ".minttyrc"
                     ensure_link "home/.npmrc"                 ".npmrc"
                     ensure_link "home/.pentadactylrc"         ".pentadactylrc"
                     ensure_link "home/.pythonrc.py"           ".pythonrc.py"
                     ensure_link "home/.ssh"                   ".ssh"
test -n "$OS_MAC" && ensure_link "home/.slate"                 ".slate"
test -z "$OS_WIN" && ensure_link "home/.tishadow.json"         ".tishadow.json"
test -z "$OS_WIN" && ensure_link "home/.titanium/config.json"  ".titanium/config.json"
                     ensure_link "home/.tmuxinator"            ".tmuxinator"
                     ensure_link "home/.tmux.conf"             ".tmux.conf"
                     ensure_link "home/.vim"                   ".vim"
                     ensure_link "home/.vimrc"                 ".vimrc"
