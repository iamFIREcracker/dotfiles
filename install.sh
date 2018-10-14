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

function error {
    echo -e "\e[01;31m$@\e[0m"
}

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
    if ! which make 2>/dev/null; then
        error "Missing command: make"
    else
        cd .vim/pack/bundle/start/vimproc.vim/
        test $FORCE -eq 1 && make clean
        make
    fi
)

(
    if ! which npm 2>/dev/null; then
        error "Missing command: npm"
    else
        cd .vim/pack/bundle/start/tsuquyomi/
        test $FORCE -eq 1 && rm -rf node_modules
        if [ ! -d node_modules ]; then
            npm install
        fi
    fi
)

(
    if ! which mvn 2>/dev/null; then
        error "Missing command: mvn"
    elif ! which javac 2>/dev/null; then
        error "Missing command: javac"
    else
        cd .vim/pack/bundle/start/vim-javacomplete2/libs/javavi/
        mvn compile
    fi
)

(
    if ! which cargo 2>/dev/null; then
        error "Missing command: cargo"
    else
        cd .vim/pack/bundle/start/parinfer-rust/
        cargo build --release
    fi
)

                     ensure_dir ".gnupg"

                     ensure_link "bin"                    "bin"
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
                     ensure_link ".msmtprc"               ".msmtprc"
                     ensure_link ".mutt"                  ".mutt"
                     ensure_link ".npmrc"                 ".npmrc"
                     ensure_link ".offlineimaprc"         ".offlineimaprc"
                     ensure_link ".pypirc"                ".pypirc"
                     ensure_link ".urlview"               ".urlview"
                     ensure_link ".pythonrc.py"           ".pythonrc.py"
test -n "$OS_MAC" && ensure_link ".slate"                 ".slate"
                     ensure_link ".tmuxinator"            ".tmuxinator"
                     ensure_link ".tmux-plugins"          ".tmux-plugins"
                     ensure_link ".tmux.conf"             ".tmux.conf"
                     ensure_link ".vim"                   ".vim"
                     ensure_link ".vimfx"                 ".vimfx"
                     ensure_link ".vimrc"                 ".vimrc"
                     ensure_link ".gnupg/gpg.conf"        ".gnupg/gpg.conf"
