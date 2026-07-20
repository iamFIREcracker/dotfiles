# If shell is not interactive: do nothing
if [[ $- != *i* ]] ; then
  return
fi

# Do nothing if inside specific applications
PPROC=$(ps -o comm= $PPID)
case $PPROC in
    *vim*) return;;
    *aider*) return;;
esac

OS_MAC=
case $HOSTNAME in
    beast.local) OS_MAC=true;;
    skinny.local) OS_MAC=true;;
esac

if [ -f ~/.env ]; then
    set -a; source ~/.env; set +a
fi
if [ -f ~/.env.properties ]; then
    set -a; source ~/.env.properties; set +a
fi

# Bash

# Reset
PROMPT_COMMAND=

# Abort piped command ASAP
set -o pipefail

# Update window size after every command
shopt -s checkwinsize

# merge / append histories
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

# Stop terminal from swalloing C-S
stty -ixon

# Stop terminal from swalloing C-Q
stty -ixoff

# Don't 'susp' with C-Z (default)
stty susp undef

# Disable colors (manually opt-in, when needed)
# I am sure I am going to regret this pretty soon
# https://no-color.org/
export NO_COLOR=1

# Avoid duplicate entries, and skip entries with a leading whitespace
export HISTCONTROL="erasedups:ignoreboth:ignorespace"

export HISTSIZE=1000000

# Don't record some commands
export HISTIGNORE="&:exit:ls:bg:fg:history:hs:clear"

# Use standard ISO 8601 timestamp
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='%F %T '

# Colors
D=$'\e[0m'
BOLD=$'\e[1m'
ITALIC=$'\e[3m'
UNDERLINE=$'\e[4m'
REVERSE=$'\e[7m'
GREEN=$'\e[0;32m'
ORANGE=$'\e[0;33m'
BLUE=$'\e[0;34m'
PINK=$'\e[0;35m'
CYAN=$'\e[0;36m'
RED=$'\e[0;31m'
WHITE=$'\e[0;97m'

eval "$(gdircolors ~/.vim/pack/bundle/start/vim-bruin/contrib/bruin.dircolors)"

# Cursor
BLOCK=$'\e[2 q'
BEAM=$'\e[5 q'

# FZF
export FZF_TMUX=1
export FZF_DEFAULT_COMMAND='rg --files --hidden --smart-case --glob "!.git/*"'
# removed --exact to test emac-ivy mode
export FZF_DEFAULT_OPTS='
  --sort
  --reverse
  --highlight-line
  --height=~13
  --pointer=
  --marker=
  --scrollbar=
  --info=inline-right
  --input-border=bottom
  --color="
    bw
    list-fg:reverse
    list-bg:reverse
    current-fg:regular:bright-white
    current-bg:regular:bright-magenta
    current-hl:regular:bright-white
    selected-fg:regular:bright-white
    selected-bg:regular:cyan
    hl:reverse
    prompt:magenta
    input-bg:regular
    query:regular
  "
  --bind="end:accept,ctrl-k:kill-line,ctrl-n:down,ctrl-p:up,ctrl-r:previous-history,alt-r:next-history,alt-a:toggle-all,ctrl-g:become(printf \"%s\n\" {} | vipe)"
'


# Bash history search integration -- CTRL-R
# Borrowed from: ~/.vim/pack/bundle/opt/fzf/shell/key-bindings.bash
__fzfcmd() {
  [[ -n "$TMUX_PANE" ]] && { [[ "${FZF_TMUX:-0}" != 0 ]] || [[ -n "$FZF_TMUX_OPTS" ]]; } &&
    echo "fzf-tmux ${FZF_TMUX_OPTS:--d${FZF_TMUX_HEIGHT:-40%}} -- " || echo "fzf"
}
__fzf_history__() {
  local output
  output=$(
    builtin fc -lnr -2147483648 |
      last_hist=$(HISTTIMEFORMAT='' builtin history 1) perl -n -l0 -e 'BEGIN { getc; $/ = "\n\t"; $HISTCMD = $ENV{last_hist} + 1 } s/^[ *]//; print $HISTCMD - $. . "\t$_" if !$seen{$_}++' |
      FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort,ctrl-z:ignore $FZF_CTRL_R_OPTS +m --read0" $(__fzfcmd) --query "$READLINE_LINE"
  ) || return
  READLINE_LINE=${output#*$'\t'}
  if [[ -z "$READLINE_POINT" ]]; then
    echo "$READLINE_LINE"
  else
    READLINE_POINT=0x7fffffff
  fi
}
# CTRL-R - Paste the selected command from history into the command line
bind -m emacs-standard -x '"\C-r": __fzf_history__'

# Bash completion ---------------------------------------------------------{{{
# Homebrew {{{

if [[ -r "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh" ]]
then
  source "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"
else
  for COMPLETION in "${HOMEBREW_PREFIX}/etc/bash_completion.d/"*
  do
    [[ -r "${COMPLETION}" ]] && source "${COMPLETION}"
  done
fi

# }}}
# Nix {{{

if [ -f $HOME/.nix-profile/etc/profile.d/bash_completion.sh ]; then
    # We get basic bash completion from the OS.
    # However, nix's bash completion would refuse to load
    # if BASH_COMPLETION_VERSINFO is not empty (it wants
    # to avoid double loading).
    # So here we temporarily unset BASH_COMPLETION_VERSINFO
    # to force load completions
    BASH_COMPLETION_VERSINFO= . $HOME/.nix-profile/etc/profile.d/bash_completion.sh
fi

# }}}
# Everything else {{{

if [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
# elif [ -f /usr/local/etc/profile.d/bash_completion.sh ]; then
#     # brew bash-completion@2
#     source /usr/local/etc/profile.d/bash_completion.sh
# elif [ -f /usr/local/etc/bash_completion ]; then
#     # brew bash-completion
#     source /usr/local/etc/bash_completion
elif [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
elif [ -f /usr/local/share/bash-completion/bash_completion ]; then
    source /usr/local/share/bash-completion/bash_completion
fi

# }}}
# Aider {{{

if [ ! -f ${XDG_CACHE_DIR-$HOME/.cache}/aider-chat/bash-completion.sh ]; then
    mkdir -p ${XDG_CACHE_DIR-$HOME/.cache}/aider-chat
    aider --shell-completions bash > ${XDG_CACHE_DIR-$HOME/.cache}/aider-chat/bash-completion.sh
fi
eval "$(cat ${XDG_CACHE_DIR-$HOME/.cache}/aider-chat/bash-completion.sh)"
# the above will add completion for `aider'
# here we do the same for `aiderw', our custom wrapper
complete -o filenames -F _shtab_aider aiderw

# }}}
# Beads {{{

if [ ! -f ${XDG_CACHE_DIR-$HOME/.cache}/beads/bash-completion.sh ]; then
    mkdir -p ${XDG_CACHE_DIR-$HOME/.cache}/beads
    beads --shell-completions bash > ${XDG_CACHE_DIR-$HOME/.cache}/beads/bash-completion.sh
fi
eval "$(cat ${XDG_CACHE_DIR-$HOME/.cache}/beads/bash-completion.sh)"
# }}}
# }}}

# General

export EDITOR="vim"
export PAGER="less"
# export BROWSER=pn
export BROWSER=br

# Let's speed things up!
BASH_ONCE_DIR=${XDG_CACHE_DIR-$HOME/.cache}/bash-once
mkdir -p $BASH_ONCE_DIR
if [ ! -f "${BASH_ONCE_DIR}/lock" ]; then
    date > "${BASH_ONCE_DIR}/lock"
    set -x
    ta daemon start &
    # (pyenv init - bash > ${BASH_ONCE_DIR}/pyenv_init) &
    # (nodenv init - bash > ${BASH_ONCE_DIR}/nodenv_init) &
    (rbenv init - bash > ${BASH_ONCE_DIR}/rbenv_init) &
    (goenv init - bash > ${BASH_ONCE_DIR}/goenv_init) &
    (mise activate bash > ${BASH_ONCE_DIR}/mise_init) &

    set +x
    wait
fi

eval "$(cat ${BASH_ONCE_DIR}/mise_init)"

function lazy_load_pyenv() {
  if [ "${PYENV_LAZY_LOADED:-0}" == "0" ]; then
    export PYENV_LAZY_LOADED=1
    eval "$(cat ${BASH_ONCE_DIR}/pyenv_init)"
  fi
}
# function lazy_load_nodenv() {
#   if [ "${NODENV_LAZY_LOADED:-0}" == "0" ]; then
#     export NODENV_LAZY_LOADED=1
#     eval "$(cat ${BASH_ONCE_DIR}/nodenv_init)"
#   fi
# }
function lazy_load_rbenv() {
  if [ "${RBENV_LAZY_LOADED:-0}" == "0" ]; then
    export RBENV_LAZY_LOADED=1
    eval "$(cat ${BASH_ONCE_DIR}/rbenv_init)"
  fi
}
function lazy_load_goenv() {
  if [ "${GOENV_LAZY_LOADED:-0}" == "0" ]; then
    export GOENV_LAZY_LOADED=1
    eval "$(cat ${BASH_ONCE_DIR}/goenv_init)"
  fi
}

# Tool version management

# Java et al.

export MAVEN_OPTS="-Xmx2048m -Xss2M -XX:ReservedCodeCacheSize=128m -XX:+TieredCompilation -XX:TieredStopAtLevel=1"
export _JAVA_OPTIONS="-Djava.awt.headless=true"

headed_java() {
    echo "Chaning _JAVA_OPTIONS"
    echo "from: $_JAVA_OPTIONS"
    export _JAVA_OPTIONS=""
    echo "  to: $_JAVA_OPTIONS"
}
headless_java() {
    echo "Chaning _JAVA_OPTIONS"
    echo "from: $_JAVA_OPTIONS"
    export _JAVA_OPTIONS="-Djava.awt.headless=true"
    echo "  to: $_JAVA_OPTIONS"
}


# Python

export PYTHONSTARTUP="~/.pythonrc.py"
export VIRTUAL_ENV_DISABLE_PROMPT=1


# Shortcuts

# # For God knows what reason, `git` completion function, __git_complete, is not
# # available until the first time command completion is invoked...
# # loaded until you manually trigger them:
# #
# #   > git <tab>
# #
# # Or, the completion file is manually sourced...
# #
# # Here is me, manually sourcing that file, so we can get completion for `g`
# if [ -f $HOME/.nix-profile/share/bash-completion/completions/git ]; then
#   source $HOME/.nix-profile/share/bash-completion/completions/git
# fi
# __git_complete g __git_main


alias n=npm
complete -o default -F _npm_completion n

alias mux=tmuxinator
# Courtesy of: https://github.com/tmuxinator/tmuxinator/blob/master/completion/tmuxinator.bash
_tmuxinator() {
    COMPREPLY=()
    local word
    word="${COMP_WORDS[COMP_CWORD]}"

    if [ "$COMP_CWORD" -eq 1 ]; then
        local commands="$(compgen -W "$(tmuxinator commands)" -- "$word")"
        local projects="$(compgen -W "$(tmuxinator completions start)" -- "$word")"

        COMPREPLY=( $commands $projects )
    elif [ "$COMP_CWORD" -eq 2 ]; then
        local words
        words=("${COMP_WORDS[@]}")
        unset words[0]
        unset words[$COMP_CWORD]
        local completions
        completions=$(tmuxinator completions "${words[@]}")
        COMPREPLY=( $(compgen -W "$completions" -- "$word") )
    fi
}
complete -F _tmuxinator tmuxinator mux

alias v=vim

# Useful functions

# Quick editing

ea()  { vim ~/.config/alacritty/alacritty.toml; }
eS()  { vim ~/.ssh/config; }
eT()  { vim ~/.tmuxinator/$(tmux display-message -p '#S').yml; }
eb()  { vim ~/dotfiles/.bashrc; }
eB()  { vim ~/dotfiles/.bash_profile; }
eb1() { vim ~/my-env/opt/bunny1/b1_custom.py; }
eg()  { vim ~/dotfiles/.gitconfig; }
ek()  {
  if [ -z $OS_MAC ]; then
    vim ~/my-env/Windows/AutoHotkey/KeyMappings.ahk
  else
    vim ~/.config/karabiner/karabiner.json
  fi
}
em()  { vim ~/.muttrc; }
ep()  {
  if [ -z "$1" ]; then
      cd ~/plan; vim .plan; cd -
  elif [ "$1" == "private" ]; then
      cd ~/plan/private; vim .; cd -
  elif [ "$1" == "work" ]; then
      cd ~/plan/work; vim .; cd -
  fi
}
eP()  { vim ~/plan/; }
es()  { vim ~/dotfiles/.slate; }
et()  { vim ~/dotfiles/.tmux.conf; }
ev()  { vim ~/dotfiles/.vim/vimrc; }
eV()  { vim ~/dotfiles/.vim/; }

function ew() { vim $(which "$1"); }
complete -c ew -w which
function cw() { cat $(which "$1"); }
complete -c cw -w which

elinks() { $EDITOR ~/Dropbox/links.txt; }
etodos() { $EDITOR ~/Dropbox/todos.txt; }
enburls() { $EDITOR ~/.newsboat/urls; }


function ..() {    cd ..; pwd; ll; }


function banner() { figlet -w9999 "$@" | cowsay -W 9999 -n -p | lolcat; }
function brewski() { brew update && brew upgrade && brew cleanup; brew doctor; }
function cleancodes() { sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"; }
function collapse() { sed -e 's/  */ /g'; }
function cuts() { cut -d' ' "$@"; }
function fucking-kill-nfsd() {
    # https://github.com/hashicorp/vagrant/issues/8103
    sudo sh -c "> /etc/exports"
    sudo nfsd restart
}
function fucking-restart-bluetooth() {
    # https://gist.github.com/nicolasembleton/afc19940da26716f8e90
    sudo kextunload -b com.apple.iokit.BroadcomBluetoothHostControllerUSBTransport
    sudo kextload -b com.apple.iokit.BroadcomBluetoothHostControllerUSBTransport
}

function hn() { head -n "$@"; }
function hn1() { hn 1; }

function j() {

    if [ "$1" == "-" ]; then
        cd -
    elif [ "$1" == "." ]; then
        cd "$(find . -type d | fzf --select-1 --query "${*}")"
    else
        cd "$(mydirs | fzf --select-1 --query "${*}")"
    fi
    ls
}


function l() { l1 "$@"; }
function l1() { tree --dirsfirst -ChFL 1 "$@"; }
function l2() { tree --dirsfirst -ChFL 2 "$@"; }
function l3() { tree --dirsfirst -ChFL 3 "$@"; }
function l4() { tree --dirsfirst -ChFL 4 "$@"; }
function l5() { tree --dirsfirst -ChFL 5 "$@"; }
function l6() { tree --dirsfirst -ChFL 6 "$@"; }
function ll() { ll1 "$@"; }
function ll1() { tree --dirsfirst -ChFupDaL 1 "$@"; }
function ll2() { tree --dirsfirst -ChFupDaL 2 "$@"; }
function ll3() { tree --dirsfirst -ChFupDaL 3 "$@"; }
function ll4() { tree --dirsfirst -ChFupDaL 4 "$@"; }
function ll5() { tree --dirsfirst -ChFupDaL 5 "$@"; }
function ll6() { tree --dirsfirst -ChFupDaL 6 "$@"; }
# maven {{{

function m() {
    mvn --batch-mode --threads 1.0C "$@" | mvn-colorify
}
function mvn-colorify() {
    sed --unbuffered \
        -e "s/Tests run: \([^,]*\), Failures: \([^,]*\), Errors: \([^,]*\), Skipped: \([^,]*\)/${GREEN}Tests run: \1${D}, Failures: ${ORANGE}\2${D}, Errors: ${RED}\3${D}, Skipped: ${CYAN}\4${D}/g" \
        -e "s/\[INFO\] \(--- .* ---\)/$BOLD\1$D/g" \
        -e "s/\[INFO\] \(Building [^jar].*\)/$CYAN\1$D/g" \
        -e "s/\[INFO\] \(BUILD SUCCESS\)/$GREEN\1$D/g" \
        -e "s/\[INFO\] \(BUILD FAILURE\)/$RED\1$D/g" \
        -e "s/\[WARNING\] \(.*\)/$ORANGE\1$D/g" \
        -e "s/\[ERROR\] \(.*\)/$RED\1$D/g" \
        -e "s/\(.*\)| \(PASS\) |/\1| ${GREEN}\2$D |/g" \
        -e "s/\(.*\)| \(FAIL\) |/\1| ${RED}\2$D |/g" \
    | \
    sed --unbuffered \
        -e "s/\[INFO\] \(.*\)/\1/g"
}


# mutt

function muttw() {
    (cd ~/Downloads && "$(which mutt)" "$@")
}
function mutt()      { muttw -F ~/Dropbox/mutt/matteo-matteolandi.net.muttrc; }
function mutt-work() { muttw -F ~/Dropbox/mutt/matteo.landi-iongroup.com.muttrc; }
function mutt-pec()  { muttw -F ~/Dropbox/mutt/landimatte-pec.it.muttrc; }


function password() {
  cat /dev/urandom | LC_ALL=C tr -dc _A-Z-a-z-0-9 | head -c${1:-32}
  echo # new lines are good!
}
function pip() {
    if [ -n "$VIRTUAL_ENV" ]; then
        $(which pip) "$@"
    else
        echo "Not currently in a venv -- use pip-sys to work system-wide."
    fi
}
function pip-sys() { $(which pip) "$@"; }
function ports { sudo lsof -iTCP -sTCP:LISTEN -P -n | grep --color "${1-.}"; }

function sb() { . ~/.bashrc; }
function sB() { . ~/.bash_profile; }

function sleepless() {
    pmset -g assertions | egrep '(PreventUserIdleSystemSleep|PreventUserIdleDisplaySleep)'
}


function vw() { vim -R -; }

# Work-on

# export PYENV_VERSION=${PYENV_VERSION-}

# function pyenvuse() {
#     lazy_load_pyenv

#     export PYENV_VERSION="$@"
#     pyenv shell "$@"
# }

# function wopyenv() {
#     local wd=${1-$(pwd)}

#     if [ $wd == "$HOME" ]; then
#         return 1
#     fi

#     local pythonversionrc=$wd/.python-version

#     if [ -e "$pythonversionrc" ]; then
#         pyenvuse "$(cat $pythonversionrc)"
#         return 0
#     else
#         wopyenv $(dirname $wd)
#         return $?
#     fi
# }


function wovenv() {
    local wd=${1-$(pwd)}

    if [ $wd == "$HOME" ]; then
        return 1
    fi
    local venvactivate=$(ls "$wd"/**/bin/activate 2>/dev/null)

    if [ -e "$venvactivate" ]; then
        . ${venvactivate}
        return 0
    else
        wovenv $(dirname $wd)
        return $?
    fi
}

# export NODENV_VERSION=${NODENV_VERSION-}

# function nodenvuse() {
#     lazy_load_nodenv

#     export NODENV_VERSION="$@"
#     nodenv shell "$@"
#     eval "$(npm completion)"
# }

# function wonodenv() {
#     local wd=${1-$(pwd)}

#     if [ $wd == "$HOME" ]; then
#         return 1
#     fi

#     local nodeversionrc=$wd/.node-version

#     if [ -e "$nodeversionrc" ]; then
#         nodenvuse "$(cat $nodeversionrc)"
#         return 0
#     else
#         wonodenv $(dirname $wd)
#         return $?
#     fi
# }

function rvmuse() {
    . "$HOME/.rvm/scripts/rvm"
    rvm use
}

function worvm() {
    local wd=${1-$(pwd)}

    if [ $wd == "$HOME" ]; then
        return 1
    fi

    local rubyversion=$wd/.ruby-version

    if [ -e "$rubyversion" ]; then
        rvmuse
        return 0
    else
        worvm $(dirname $wd)
        return $?
    fi
}

function wonix() {
    local wd=${1-$(pwd)}

    if [ $wd == "$HOME" ]; then
        return 1
    fi

    local nixshell=$wd/shell.nix
    echo $nixshell
    if [ -e "$nixshell" ]; then
        nix-shell $nixshell
        return 0
    else
        wonix $(dirname $wd)
        return $?
    fi
}


function wo() {
    wopyenv
    wovenv
    # wonodenv
    worvm
    # Keep this last... if successful, it will spawn a new shell
    wonix
}

function zombies() {  ps ex | awk "\$3==\"Z\"{print \$0}"; }

# Prompt

compact_cwd() {
    local location=$(pwd | sed "s,$HOME,~,")
    local limit=31
    while true; do
        local next=${location#*/}
        if [ ${#location} -le 31 ] || [ "$next" = "$location" ]; then
            # echo " in ${UNDERLINE}${location}${D}"
            # echo " in ${WHITE}${location}${D}"
            # echo " in ${BOLD}${location}${D}"
            echo "[${location}]"
            break
        fi
        location=...$next
    done
}

git_ps1() {
    local branch=$(git currentbranch)
    local status=$(git_prompt_status)
    # echo "on ${ITALIC}${branch}${BOLD}${status}${D}"
    echo "[${branch}${status}]"
}

rcs_ps1() {
    if git root >/dev/null 2>&1; then
        echo " "$(git_ps1)
    fi
}

pyenv_ps1() {
    [ $PYENV_VERSION ] && echo "Python: ${PYENV_VERSION}\n"
}

venv_ps1() {
    local prompt
    # [ $VIRTUAL_ENV ] && echo " ${ORANGE}>>$(basename $VIRTUAL_ENV)<<${D}"
    if [ $VIRTUAL_ENV ]; then
        prompt=$(echo $VIRTUAL_ENV | tr '/' '\n' | tail -n2 | tr '\n' '/')
        echo "Venv: ${prompt%/}\n"
    fi
}

# nodenv_ps1() {
#     [ $NODENV_VERSION ] && echo "Node: ${NODENV_VERSION}\n"
# }

prompt_string() {
    local prompt=""

    if [ -n "$IN_NIX_SHELL" ]; then
        if [ -z "$prompt" ]; then
            prompt=nix
        else
            prompt="$prompt,nix"
        fi
    fi

    if [ -n "$prompt" ]; then
        echo -n "[$prompt]"
    fi
    echo -n "$"
}

actual_prompt() {
    local lvl=$SHLVL  exit=$1

    if [[ -n "$TMUX" ]]; then
        if [[ -n "$OS_MAC" ]]; then
            lvl=$(($lvl - 3))
        else
            lvl=$(($lvl - 2))
        fi
    fi
    if [[ $lvl -gt 1 ]]; then
        echo -n "[$((lvl - 1))]"
    fi
    if [[ $exit -eq 0 ]]; then
        echo -n "$(prompt_string)"
    else
        echo -n "$exit $(prompt_string)"
    fi
}

ring_a_bell() {
  echo -n -e "\a"
}

refresh_env() {
    [ -z "$TMUX" ] && return

    local line

    while read line; do
        case $line in
            -*)
                # Strip the leading `-'
                line=${line:1}
                unset $line
                ;;

            *)
                # Add quotes around the var value
                line=${line/=/=\"}
                line=${line/%/\"}
                eval export $line
                ;;
        esac
    done < <(tmux show-environment)
}

export LAST_DIR=${LAST_DIR-}
poor_man_direnv() {
    if [ "$LAST_DIR" != "$PWD" ]; then
        export LAST_DIR=$PWD
        case "${PWD#"$HOME"/}" in
            Workspace/aider)
                # lazy_load_pyenv
                # pyenv shell 3.12.10
                wovenv || (python -m venv venv && wovenv)
                ;;
            Workspace/grep-ast)
                # lazy_load_pyenv
                # pyenv shell 3.12.10
                wovenv || (python -m venv venv && wovenv)
                ;;
            Workspace/job/license-server*)
                set -a; source .env; set +a
                # wonodenv
                ;;
            Workspace/job/token-server*)
                set -a; source .env; set +a
                # wonodenv
                ;;
            Workspace/job/ConnectION/connection*)
                # wonodenv
                wopyenv
                ;;
            Workspace/job/ConnectION/cnxt*)
                # wonodenv
                ;;
            Workspace/job/ConnectION/ingest-alogs)
                set -a; source .env; set +a
                wopyenv
                wovenv || (python -m venv venv && wovenv)
                ;;
            Workspace/job/Tracker/web-portal)
                set -a; source .env; set +a
                ;;
            Workspace/projections-scripts)
                # lazy_load_pyenv
                # pyenv shell 3.12.10
                wovenv || (python -m venv venv && wovenv)
                ;;
        esac

    fi

}

cursor_style() {
    echo -n "$BEAM"
}


# Inspired by: https://gist.github.com/3083586
prompt_command() {
    local actual=$(actual_prompt $?)

    ring_a_bell

    refresh_env

    poor_man_direnv

    # Record each line as it gets issued
    history -a

    # OSC 133 -- prompt marker
    printf '\e]133;A\e'


    # The following sets up a prompt like the following (the first leading empty line
    # is intentional... it separates prompts better):
    #
    #   > some command
    #   ...
    #
    #   matteolandi at hairstyle.local in /Users/matteolandi/my-env/dotfiles on master!?
    #   >
    #
    # Note: if we want to _style_ the last line (the actual prompt line), we
    # will have to wrap non-visible escape codes inside \[ and \], or bad things
    # will happen: https://github.com/alacritty/alacritty/issues/3512
    #
    # Read more about escaping non-printing characters, here:
    # https://superuser.com/questions/301353/escape-non-printing-characters-in-a-function-for-a-bash-prompt
    PS1=
    PS1="$PS1\n"                                  # gracious new line
    PS1="$PS1\n"                                  # gracious new line 2x
    PS1="$PS1[$HOSTNAME]"                         # hostname
    PS1="$PS1 $(compact_cwd)"                     # cwd
    PS1="$PS1$(rcs_ps1)"                          # git/mercurial/svn
    PS1="$PS1 $(date +'%e - %I:%M %p')"           # Now
    PS1="$PS1\n"
    PS1="$PS1$(pyenv_ps1)"                        # pyenv
    PS1="$PS1$(venv_ps1)"                         # virtualenv
    # PS1="$PS1$(nodenv_ps1)"                       # nodenv
    PS1="$PS1\[${REVERSE}\]"
    PS1="$PS1${actual}"                           # the actual prompt
    PS1="$PS1\[$D\]"                              # reset
    PS1="$PS1 "                                   # gratuitus space
    PS1="$PS1\[$(cursor_style)\]"                 # cursor style
    export PS1
}


export PROMPT_COMMAND="prompt_command $PROMPT_COMMAND"

# Use full path here.. ~/bin will be added to PATH only later
~/bin/clear-screen-and-position-cursor-at-the-bottom
