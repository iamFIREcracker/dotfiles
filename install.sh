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
                     ensure_link ".abclrc"                ".abclrc"
                     ensure_link ".agignore"              ".agignore"
                     ensure_link ".bash_profile"          ".bash_profile"
                     ensure_link ".bashrc"                ".bashrc"
                     ensure_link ".bunfig.toml"           ".bunfig.toml"
                     ensure_dir  ".config"

                     ensure_dir  ".config/claude"
                     ensure_link ".claude/CLAUDE.md"		".config/claude/CLAUDE.md"           
                     ensure_link ".claude/settings.json"	".config/claude/settings.json"       
                     ensure_link ".claude/statusline.sh"	".config/claude/statusline.sh"       
                     ensure_link ".claude/hooks"		".config/claude/hooks"               
                     ensure_link ".claude/skills"		".config/claude/skills"              
                     ensure_dir  ".config/claude-work"
                     ensure_link ".claude/CLAUDE.md"		".config/claude-work/CLAUDE.md"      
                     ensure_link ".claude/settings.json"	".config/claude-work/settings.json"  
                     ensure_link ".claude/statusline.sh"	".config/claude-work/statusline.sh"  
                     ensure_link ".claude/hooks"		".config/claude-work/hooks"          
                     ensure_link ".claude/skills"		".config/claude-work/skills"         

test -n "$OS_MAC" && ensure_link ".config/karabiner"      ".config/karabiner"
                     ensure_link ".config/home-manager"   ".config/home-manager"
                     ensure_link ".config/git"            ".config/git"

                     ensure_dir  ".config/opencode"
                     ensure_link ".config/opencode/opencode.json" ".config/opencode/opencode.json"
                     ensure_link ".config/opencode/tui.json"      ".config/opencode/tui.json"

                     ensure_link ".config/nix"            ".config/nix"
                     ensure_link ".config/nixpkgs"        ".config/nixpkgs"
                     ensure_link ".config/nvim"           ".config/nvim"
                     ensure_link ".config/tridactyl"      ".config/tridactyl"
                     ensure_link ".cgrc"                  ".cgrc"
                     ensure_link ".ctags"                 ".ctags"
                     ensure_link ".eclrc"                 ".eclrc"
                     ensure_link ".gitconfig"             ".gitconfig"
                     ensure_link ".hammerspoon"           ".hammerspoon"
                     ensure_link ".hgignore"              ".hgignore"
                     ensure_link ".hgrc"                  ".hgrc"
                     ensure_link ".ignore"                ".ignore"
                     ensure_link ".inputrc"               ".inputrc"
                     ensure_link ".lisp"                  ".lisp"
test -n "$OS_WIN" && ensure_link ".minttyrc"              ".minttyrc"
                     ensure_link ".mutt"                  ".mutt"
                     ensure_link ".npmrc"                 ".npmrc"
                     ensure_link ".newsboat"              ".newsboat"
                     ensure_link ".node"                  ".node"
                     ensure_link ".pypirc"                ".pypirc"
                     ensure_link ".pythonrc.py"           ".pythonrc.py"
                     ensure_link ".projections.json"      ".projections.json"
                     ensure_link ".sbclrc"                ".sbclrc"
                     ensure_link ".tmuxinator"            ".tmuxinator"
                     ensure_link ".tmux-plugins"          ".tmux-plugins"
                     ensure_link ".tmux.conf"             ".tmux.conf"
                     ensure_link ".tmux-login.conf"       ".tmux-login.conf"
                     ensure_link ".vim"                   ".vim"
                     ensure_link ".vimfx"                 ".vimfx"
                     ensure_link ".w3m"                   ".w3m"
                     ensure_link ".gnupg/gpg.conf"        ".gnupg/gpg.conf"
                     ensure_link "ccl-init.lisp"          "ccl-init.lisp"

vim -c 'helptags ALL | quit'
