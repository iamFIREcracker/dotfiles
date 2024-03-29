#!/usr/bin/env bash

FORCE=0
for i; do
    if [ "$i" == '--force' ]; then
        FORCE=1
    fi
done

WORKDIR="$(pwd)"
OS_MAC=$(uname -s | grep Darwin)
OS_WIN=$(uname -rs | grep -e CYGWIN -e Microsoft)

set -u
set -e
set -x

function ensure_link {
    test $FORCE -eq 1 && remove "$HOME/$2"
    test -L "$HOME/$2" || create_link "$WORKDIR/$1" "$HOME/$2"
}

function create_link {
    ln -s "$1" "$2"
}

function ensure_dir {
    test $FORCE -eq 1 && remove "$HOME/$1"
    test -d "$HOME/$1" || create_dir "$HOME/$1"
}

function remove {
    rm -rf "$1"
}

function create_dir {
    mkdir -p $1
}

                     ensure_dir ".gnupg"

                     ensure_link "bin"                    "bin"
                     ensure_link ".agignore"              ".agignore"
                     ensure_link ".bash_profile"          ".bash_profile"
                     ensure_link ".bashrc"                ".bashrc"
                     ensure_dir  ".config"
test -n "$OS_MAC" && ensure_link ".config/alacritty"      ".config/alacritty"
test -n "$OS_MAC" && ensure_link ".config/karabiner"      ".config/karabiner"
                     ensure_link ".config/nixpkgs"        ".config/nixpkgs"
                     ensure_link ".config/nvim"           ".config/nvim"
                     ensure_link ".cgrc"                  ".cgrc"
                     ensure_link ".ctags"                 ".ctags"
                     ensure_link ".gitconfig"             ".gitconfig"
                     ensure_link ".hammerspoon"           ".hammerspoon"
                     ensure_link ".hgignore"              ".hgignore"
                     ensure_link ".hgrc"                  ".hgrc"
                     ensure_link ".inputrc"               ".inputrc"
                     ensure_link ".lisp"                  ".lisp"
test -n "$OS_WIN" && ensure_link ".minttyrc"              ".minttyrc"
                     ensure_link ".mutt"                  ".mutt"
                     ensure_link ".newsboat"              ".newsboat"
                     ensure_link ".node"                  ".node"
                     ensure_link ".pypirc"                ".pypirc"
                     ensure_link ".pythonrc.py"           ".pythonrc.py"
                     ensure_link ".sbclrc"                ".sbclrc"
                     ensure_link ".tmuxinator"            ".tmuxinator"
                     ensure_link ".tmux-plugins"          ".tmux-plugins"
                     ensure_link ".tmux.conf"             ".tmux.conf"
                     ensure_link ".vim"                   ".vim"
                     ensure_link ".vimfx"                 ".vimfx"
                     ensure_link ".w3m"                   ".w3m"
                     ensure_link ".gnupg/gpg.conf"        ".gnupg/gpg.conf"
                     ensure_link "ccl-init.lisp"          "ccl-init.lisp"

(
    fzf_dir=
    if [ -n "$OS_WIN" ]; then
      fzf_dir=.vim/pack/bundle/opt/fzf-ruby
    else 
      fzf_dir=.vim/pack/bundle/opt/fzf
    fi

    cd $fzf_dir
    # the install script checks if `fzf' ware already installed
    # and if found, would symlink it to ./bin
    #
    # the problem is, I have a wrapper for `fzf' under ~/bin, so
    # I want the `install' script to download the binary (instead
    # of symlinking my wrapper...lol) I have to make sure ~/bin
    # is not covered by `$PATH'
    test $FORCE -eq 1 && rm -f bin/fzf bin/fzf.exe
    if [ ! -f bin/fzf -a ! -f bin/fzf.exe ]; then
        PATH=/bin:/usr/bin ./install --bin
    fi
    cd -

    ensure_link "$fzf_dir/bin/fzf" "local/bin/fzf"
    ensure_link "$fzf_dir/bin/fzf-tmux" "local/bin/fzf-tmux"
    if [ -f "$fzf_dir/man/man1/fzf-tmux.1" ]; then
      ensure_link "$fzf_dir/man/man1/fzf-tmux.1" "local/man/man1/fzf-tmux.1"
    fi
    ensure_link "$fzf_dir/man/man1/fzf.1" "local/man/man1/fzf.1"
)

vim -c 'helptags ALL | quit'
