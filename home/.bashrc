# Bash {{{

if [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
elif [ -f /usr/local/etc/bash_completion ]; then
    source /usr/local/etc/bash_completion
elif [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
fi


# Abort piped command ASAP
set -o pipefail

# Update window size after every command
shopt -s checkwinsize

# Display matches for ambiguous patterns at first tab press
bind "set show-all-if-ambiguous on"

# merge / append histories
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:hs:clear:*fpp"

# Use standard ISO 8601 timestamp
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='%F %T '

# Allows space to complete and expand !$ eg:
# $ ls Projects
# $ cd !$<space> # completes to `cd Projects`
bind Space:magic-space

# Resume suspended program with C-Z -- and not `fg`
stty susp undef
bind '"\C-z":"fg\015"'

# Pipe last command to fpp with C-P
bind '"\C-p": "!! | fpp\015"'
# Sudo last command with C-<enter>
bind '"✠": "sudo !!\015"'


# Colors
N=$'\e[0m'
BOLD=$'\e[1m'
D=$'\e[37;40m'
PINK=$'\e[35;40m'
GREEN=$'\e[32;40m'
ORANGE=$'\e[33;40m'
CYAN=$'\e[36;40m'
RED=$'\e[31;40m'

# }}}
# Vim mode {{{

set -o vi

# Handy bindings
bind -m vi-command '"gg": beginning-of-history'
bind -m vi-command '"G": end-of-history'
bind -m vi-command '"H": beginning-of-line'
bind -m vi-command '"L": end-of-line'

# I give up
alias :q=exit
alias :qa=exit

# }}}
# Environment variables {{{

export HISTCONTROL=erasedups
export HISTSIZE=10000
export TERM=screen-256color
export EDITOR="vim"
export PAGER="/usr/bin/less"
export MANPAGER="/bin/sh -c \"col -b | vim -c 'set ft=man ts=8 nomod nolist nonu nornu noma' -\""
export PATH="${HOME}/npm/bin:$PATH"
export PATH="${HOME}/opt/PathPicker:$PATH"
export HGEDITOR="~/bin/hgeditor"

# Java et al. {{{

if [ -z "$JAVA_HOME" ]; then
    if [ -f /usr/libexec/java_home ]; then
        export JAVA_HOME=$(/usr/libexec/java_home)
    fi
fi
export MAVEN_OPTS="-Xmx512M -XX:+TieredCompilation -XX:TieredStopAtLevel=1"
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

# }}}
# Python {{{

export PYTHONPATH="~/lib/python:$PYTHONPATH"
export PYTHONSTARTUP="~/.pythonrc.py"
export VIRTUAL_ENV_DISABLE_PROMPT=1

# }}}
# }}}
# Extra {{{

if [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
elif [ -f /usr/local/etc/bash_completion ]; then
    source /usr/local/etc/bash_completion
fi

if [ -f ~/lib/bash/mobile.sh ]; then
    source ~/lib/bash/mobile.sh
fi

export LOADED_SCRIPTS=${BASH_SOURCE}

load_if_present() {
    if [ -f "$1" ]; then
        LOADED_SCRIPTS="$1:$LOADED_SCRIPTS"
        source "$1"
    fi
}

load_if_present ~/opt/z/z.sh
load_if_present ~/opt/fabric-completion/fabric-completion.bash
load_if_present ~/opt/vagrant-bash-completion/etc/bash_completion.d/vagrant
load_if_present ~/.bashrc_ion

# }}}
# Function completion {{{

# wrap_alias takes three arguments:
# $1: The name of the alias
# $2: The command used in the alias
# $3: The arguments in the alias all in one string
# Generate a wrapper completion function (completer) for an alias
# based on the command and the given arguments, if there is a
# completer for the command, and set the wrapper as the completer for
# the alias.
function wrap_alias() {
  [[ "$#" == 3 ]] || return 1

  local alias_name="$1"
  local aliased_command="$2"
  local alias_arguments="$3"
  local num_alias_arguments=$(echo "$alias_arguments" | wc -w)

  # The completion currently being used for the aliased command.
  local completion=$(complete -p $aliased_command 2> /dev/null)

  # Only a completer based on a function can be wrapped so look for -F
  # in the current completion. This check will also catch commands
  # with no completer for which $completion will be empty.
  echo $completion | grep -q -- -F || return 0

  local namespace=alias_completion::

  # Extract the name of the completion function from a string that
  # looks like: something -F function_name something
  # First strip the beginning of the string up to the function name by
  # removing "* -F " from the front.
  local completion_function=${completion##* -F }
  # Then strip " *" from the end, leaving only the function name.
  completion_function=${completion_function%% *}

  # Try to prevent an infinite loop by not wrapping a function
  # generated by this function. This can happen when the user runs
  # this twice for an alias like ls='ls --color=auto' or alias l='ls'
  # and alias ls='l foo'
  [[ "${completion_function#$namespace}" != $completion_function ]] && return 0

  local wrapper_name="${namespace}${alias_name}"

  eval "
function ${wrapper_name}() {
  let \"COMP_CWORD+=$num_alias_arguments\"
  args=( \"${alias_arguments}\" )
  COMP_WORDS=( $aliased_command \${args[@]} \${COMP_WORDS[@]:1} )
  $completion_function
  }
"

  # To create the new completion we use the old one with two
  # replacements:
  # 1) Replace the function with the wrapper.
  local new_completion=${completion/-F * /-F $wrapper_name }
  # 2) Replace the command being completed with the alias.
  new_completion="${new_completion% *} $alias_name"

  eval "$new_completion"
}

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

# }}}
# Useful functions {{{

eb()  { vim ~/dotfiles/home/.bashrc; }
eb1() { vim ~/dotfiles/home/opt/bunny1/b1_custom.py; }
eg()  { vim ~/dotfiles/.gitconfig; }
ej()  { vim $(mktemp ${TMPDIR-/tmp}/message.XXXXXX) -c 'se spell wrap nonu noru ft=jira'; }
eh()  { vim ~/dotfiles/home/.hgrc; }
em()  { vim $(mktemp ${TMPDIR-/tmp}/message.XXXXXX) -c 'se spell wrap nonu noru'; }
es()  { vim ~/dotfiles/home/.slate; }
et()  { vim ~/dotfiles/home/.tmux.conf; }
ev()  { vim ~/dotfiles/home/.vimrc; }
eV()  { vim ~/dotfiles/home/.vimperatorrc; }

function ..() {    cd ../"$@"; }
function ...() {   cd ../../"$@"; }
function ....() {  cd ../../../"$@"; }
function .....() { cd ../../../../"$@"; }



function -() {
    if [ $# == 1 ]; then
        grep -v -E "$1" | hl "$@"
    elif [ $# == 2 ]; then
        grep -v -E "$1|$2" | hl "$@"
    elif [ $# == 3 ]; then
        grep -v -E "$1|$2|$3" | hl "$@"
    elif [ $# == 4 ]; then
        grep -v -E "$1|$2|$3|$4" | hl "$@"
    elif [ $# == 5 ]; then
        grep -v -E "$1|$2|$3|$4|$5" | hl "$@"
    elif [ $# == 6 ]; then
        grep -v -E "$1|$2|$3|$4|$5|$6" | hl "$@"
    elif [ $# -gt 6 ]; then
        grep -v -E "$1|$2|$3|$4|$5|$6" | - "${@:7}"
    fi
}
# Ack {{{

function a() {
    local _ack

    if hash ack 2>/dev/null; then
        _ack=ack
    elif hash ack-grep 2>/dev/null; then
        _ack=ack-grep
    fi
    ${_ack} "$@"
}
function ai() { a -i "$@"; }

# }}}
function b() { bower "$@"; }
function banner() { figlet -f ogre -w9999 "$@" | cowsay -W 9999 -n -p | lolcat; }
function b1() { ~/opt/bunny1/venv/bin/python ~/opt/bunny1/b1_custom.py --test "$*"; }
function cleancodes() { sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"; }
function collapse() { sed -e 's/  */ /g'; }
function cuts() { cut -d' ' "$@"; }
function de() { deactivate; }
function uniqdiff() {
    local input=/tmp/uniqdiff_all.$$
    trap "kill -TERM $PID; rm '${input}'" TERM INT
    cat > ${input}
    </dev/tty vimdiff <(cat ${input}) <(cat ${input} | uniq "$@")
    rm "${input}"
}
function uniqdiff1() { uniqdiff --skip-fields 1; }
function edit-pasteboard() { cb | vipe | cb; }
function from() { tac "$1" | sed "/$2/q" | tac; }
# Git {{{

function g() { git "$@"; }
wrap_alias g git ''

function _git_pl() {
    _git_log
}

function _git_rb() {
    _git_rebase
}

# }}}

gimmeurjson() {
    local url=$1
    local method=${2:-GET}
    local data="$3"
    local line

    [ ${method} = "GET" ] && url="${url}?${data}"

    curl -i -s -w'\n' \
        --header 'Accept: application/json' \
        --header 'Content-Type: application/json' \
        "${url}" -X "${method}" -d "${data}"
}

function grep() { $(which grep) --line-buffered "$@"; }
function gc() {
    if [ $# == 1 ]; then
        grep -E "$1" | hl "$@"
    elif [ $# == 2 ]; then
        grep -E "$1|$2" | hl "$@"
    elif [ $# == 3 ]; then
        grep -E "$1|$2|$3" | hl "$@"
    elif [ $# == 4 ]; then
        grep -E "$1|$2|$3|$4" | hl "$@"
    elif [ $# == 5 ]; then
        grep -E "$1|$2|$3|$4|$5" | hl "$@"
    elif [ $# == 6 ]; then
        grep -E "$1|$2|$3|$4|$5|$6" | hl "$@"
    else
        exit Too many arguments
    fi
}
# Mercurial {{{

function h() { hg "$@"; }
wrap_alias h hg ''

# }}}
function hn() { head -n "$@"; }
function hn1() { hn 1; }
function hl() {
    if [ $# == 1 ]; then
        hl1 "$1"
    elif [ $# == 2 ]; then
        hl1 "$1" | hl2 "$2"
    elif [ $# == 3 ]; then
        hl1 "$1" | hl2 "$2" | hl3 "$3"
    elif [ $# == 4 ]; then
        hl1 "$1" | hl2 "$2" | hl3 "$3" | hl4 "$4"
    elif [ $# == 5 ]; then
        hl1 "$1" | hl2 "$2" | hl3 "$3" | hl4 "$4" | hl5 "$5"
    elif [ $# == 6 ]; then
        hl1 "$1" | hl2 "$2" | hl3 "$3" | hl4 "$4" | hl5 "$5" | hl6 "$6"
    elif [ $# -gt 6 ]; then
        hl1 "$1" | hl2 "$2" | hl3 "$3" | hl4 "$4" | hl5 "$5" | hl6 "$6" | hl "${@:7}"
    fi
}
function hl1() { GREP_COLOR="1;31" grep -E --color=always "$1|\$"; }
function hl2() { GREP_COLOR="1;32" grep -E --color=always "$1|\$"; }
function hl3() { GREP_COLOR="1;33" grep -E --color=always "$1|\$"; }
function hl4() { GREP_COLOR="1;34" grep -E --color=always "$1|\$"; }
function hl5() { GREP_COLOR="1;35" grep -E --color=always "$1|\$"; }
function hl6() { GREP_COLOR="1;36" grep -E --color=always "$1|\$"; }
function hs() { history "$@"; }
function j() { z "$@"; }
# Join lines {{{

function J() {
    tr '^M' '\n' | tr -s '\n' '      '
}

# }}}
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
function ls() { fortune; }
# maven {{{

function m() {
    mvn --batch-mode --threads 1.0C "$@" | mcolorify
}
function mcolorify() {
    sed --unbuffered \
        -e "s/Tests run: \([^,]*\), Failures: \([^,]*\), Errors: \([^,]*\), Skipped: \([^,]*\)/${GREEN}Tests run: \1${D}, Failures: ${ORANGE}\2${D}, Errors: ${RED}\3${D}, Skipped: ${CYAN}\4${D}/g" \
        -e "s/\[INFO\] \(--- .* ---\)/$BOLD\1$N/g" \
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

# }}}
function n() { npm "$@"; }
function median() { percentile 50; }
function o() { open "$@"; }
function oo() { open .; }
function p() { ping "$@"; }
function percentile() { awk "{ a[i++]=\$0; } END { print a[int(i*$1/100)]; }"; }
function pip() {
    if [ -n "$VIRTUAL_ENV" ]; then
        $(which pip) "$@"
    else
        echo "Not currently in a venv -- use pip-sys to work system-wide."
    fi
}
function pipf() { pip freeze > requirements.txt; }
function pipir() { pip install -r requirements.txt; }
function pip-sys() { $(which pip) "$@"; }
function ports { sudo lsof -iTCP -sTCP:LISTEN -P -n; }
function psg() { ps auxww | grep -i --color=always "$@" | grep -v grep | collapse | cuts -f 2,11-; }
# react-native {{{

function rn() { react-native "$@"; }
function rnri() { rn run-ios "$@"; }
function rnri5s() { rnri --simulator "iPhone 5s"; }

# }}}
function s() {
    local oldifs host customcmd uberscript cmd

    oldifs=$IFS
    host="$1"
    customcmd="${2:-true}"
    IFS=:
    set -- $LOADED_SCRIPTS
    uberscript=$(cat "$@" | base64)
    IFS=$oldifs

    cmd="${cmd} $customcmd;"
    cmd="${cmd} echo '$uberscript' | base64 --decode > /tmp/.bashrc_temp;"
    cmd="${cmd} bash --rcfile /tmp/.bashrc_temp"
    ssh -R 5556:localhost:5556 -t $host "$cmd"
}
function sb() { . ~/.bashrc; }
function serve-this() { python -m SimpleHTTPServer "$@"; }
# strip colors {{{

function strip-colors() { perl -pe 's/\e\[?.*?[\@-~]//g'; }

# }}}
function sum() { awk '{s+=$1}END{print s}'; }
# tac {{{
if hash gtac 2>/dev/null; then
    _tac=gtac
elif hash tac 2>/dev/null; then
    _tac=$(which tac)
fi
function tac() {
    ${_tac} "$@"
}

# }}}
function tmuxlist() { tmux list-sessions; }
function tmuxattach() {
    tmuxlist
    read sessionname
    tmux attach -t "$sessionname"
}
function to() { sed "/$1/q"; }
function tf() { tail -f "$@"; }
function urldecode() { python -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])" "$@"; }
function urlencode() { python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);" "$@"; }
# Vagrant {{{

v() { vagrant "$@"; }
vh() { v halt; }
vs() { v ssh -- -R 5556:localhost:5556; }
vu() { v up; }

# }}}
vipe() {
    # http://stackoverflow.com/a/10686830
    local tmpfile
    tmpfile=`mktemp /tmp/vipe.bashXXXXXXXX`
    cat > ${tmpfile}
    vim ${tmpfile} < /dev/tty > /dev/tty
    cat ${tmpfile}
    rm ${tmpfile}
}
function vw() { vim -R -; }
function wo() {
    local wd=`pwd`

    while [ $wd != '/' ]; do
        local venvactivate=$(find . | grep '/bin/activate$')

        if [ ! -e $venvactivate ]; then
            wd=`dirname $wd`
        else
            . ${venvactivate}
            return
        fi
    done
}
function wpk() {
    kill `cat .bgrun.pid`
}
function wpr() {
    wpk; bgrun "python run_app.py"
    watchmedo shell-command \
        --recursive \
        --wait \
        --patterns='*.py;*.html;*.js' \
        --command='echo "${watch_src_path}"; bash -c "kill `cat .bgrun.pid`; bgrun \"python run_app.py\""'
}
function x() {
    if [ $# == 1 ]; then
        grep -o -E "$1"
    elif [ $# == 2 ]; then
        grep -o -E "$1|$2"
    elif [ $# == 3 ]; then
        grep -o -E "$1|$2|$3"
    elif [ $# == 4 ]; then
        grep -o -E "$1|$2|$3|$4"
    elif [ $# == 5 ]; then
        grep -o -E "$1|$2|$3|$4|$5"
    elif [ $# == 6 ]; then
        grep -o -E "$1|$2|$3|$4|$5|$6"
    else
        exit Too many arguments
    fi
}
function xvim() {
    xargs "$@" sh -c 'vim "$@" </dev/tty' dummy_script_name
}
function zombies() {  ps ex | awk "\$3==\"Z\"{print \$0}"; }

# }}}
# Prompt {{{

git_ps1() {
    if git root >/dev/null 2>&1; then
        local branch=$(git currentbranch)
        local status=$(git_prompt_status)
        echo "on ${PINK}${branch}${D}${GREEN}${status}${D}"
    fi
}

hg_ps1() {
    if hg st >/dev/null 2>&1; then
        local branch=$(hg branch)
        local status=$(hg_prompt_status)
        echo "on ${PINK}${branch}${D}${GREEN}${status}${D}"
    fi
}

rcs_ps1() {
    if [ -n "$PROMPT_NO_RCS" ]; then
        echo
    else
        hg_ps1
        git_ps1
    fi
}

venv_ps1() {
    [ $VIRTUAL_ENV ] && echo "${ORANGE}>>"`basename $VIRTUAL_ENV`"<<${D}"
}

actual_prompt() {
    if [[ $EXITVAL == 0 ]]; then
        echo -n "> "
    else
        echo -n "$EXITVAL > "
    fi
}

# Inspired by: https://gist.github.com/3083586
prompt_command() {
    EXITVAL=$?
    PS1=$(actual_prompt)

    z --add `pwd`

    # Record each line as it gets issued
    history -a

    echo -e "\n${PINK}${USER}${D} at ${ORANGE}${HOSTNAME}${D} in ${GREEN}${PWD}${D} $(rcs_ps1) $(venv_ps1)"
}
export PROMPT_COMMAND='prompt_command'

# }}}
