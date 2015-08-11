# Bash {{{

# merge / append histories
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
elif [ -f /usr/local/etc/bash_completion ]; then
    . /usr/local/etc/bash_completion
fi

# }}}
# Vim mode {{{

# I give up
alias :q=exit
alias :qa=exit

bind -m vi-command '"H":beginning-of-line'
bind -m vi-command '"L":end-of-line'

set -o vi

# }}}
# Environment variables {{{

export HISTCONTROL=erasedups
export HISTSIZE=10000
export TERM=screen-256color
export EDITOR="vim"
export PAGER="/usr/bin/less"
export PATH="${HOME}/npm/bin:$PATH"
export HGEDITOR="~/bin/hgeditor"
export GIT_EDITOR="~/bin/giteditor"

# Java et al. {{{

if [ -z "$JAVA_HOME" ]; then
    if [ -f /usr/libexec/java_home ]; then
        export JAVA_HOME=$(/usr/libexec/java_home)
    fi
fi
export MAVEN_OPTS="-Xmx512M -XX:MaxPermSize=512M"
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

export PYTHONPATH="$HOME/lib/python:$PYTHONPATH"
export PYTHONSTARTUP="$HOME/.pythonrc.py"
export VIRTUAL_ENV_DISABLE_PROMPT=1

# }}}
# }}}
# Z {{{

. ${HOME}/opt/z/z.sh

# }}}
# Useful functions {{{

eb() { vim ~/.bashrc; }
ef() { vim ~/.config/fish/config.fish; }
eg() { vim ~/.gitconfig; }
eh() { vim ~/.hgrc; }
ep() { vim ~/.pentadactylrc; }
es() { vim ~/.slate; }
ev() { vim ~/.vimrc; }

function ..() {     cd ../"$@"; }
function ...() {    cd ../../"$@"; }
function ....() {   cd ../../../"$@"; }
function .....() {   cd ../../../../"$@"; }



function a() { ack "$@"; }
function bcvi() { ${HOME}/opt/bcvi/bin/bcvi "$@"; }
function collapse() { sed -e 's/  */ /g'; }
function cuts() { cut -d' ' "$@"; }
function de() { deactivate; }
function edit-pasteboard() { cb | vipe | cb; }
function g() { git "$@"; }
function gc() { $(which grep) --color=always --line-buffered "$@"; }
function h() { hg "$@"; }
function hn() { head -n "$@"; }
function hl() {
    if [ $# == 1 ]; then
        hl1 $1
    elif [ $# == 2 ]; then
        hl1 $1 | hl2 $2
    elif [ $# == 3 ]; then
        hl1 $1 | hl2 $2 | hl3 $3
    elif [ $# == 4 ]; then
        hl1 $1 | hl2 $2 | hl3 $3 | hl4 $4
    elif [ $# == 5 ]; then
        hl1 $1 | hl2 $2 | hl3 $3 | hl4 $4 | hl5 $5
    elif [ $# == 6 ]; then
        hl1 $1 | hl2 $2 | hl3 $3 | hl4 $4 | hl5 $5 | hl6 $6
    elif [ $# -gt 6 ]; then
        hl1 $1 | hl2 $2 | hl3 $3 | hl4 $4 | hl5 $5 | hl6 $6 | hl "${@:7}"
    fi
}
function hl1() { GREP_COLOR="1;31" grep -E --color=always --line-buffered "$1|\$"; }
function hl2() { GREP_COLOR="1;32" grep -E --color=always --line-buffered "$1|\$"; }
function hl3() { GREP_COLOR="1;33" grep -E --color=always --line-buffered "$1|\$"; }
function hl4() { GREP_COLOR="1;34" grep -E --color=always --line-buffered "$1|\$"; }
function hl5() { GREP_COLOR="1;35" grep -E --color=always --line-buffered "$1|\$"; }
function hl6() { GREP_COLOR="1;36" grep -E --color=always --line-buffered "$1|\$"; }
function j() { z "$@"; }
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
function m() { mvn --batch-mode "$@"; }
function md() { mkdir -p "$@"; }
function median() { percentile 50; }
function o() { open "$@"; }
function oo() { open .; }
function percentile() { awk "{ a[i++]=\$0; } END { print a[int(i*$1/100)]; }"; }
function pip() {
    if [ -n "$VIRTUAL_ENV" ]; then
        `which pip` "$@"
    else
        echo "Not currently in a venv -- use pip-sys to work system-wide."
    fi
}
function pipf() { pip freeze > requirements.txt; }
function pip-sys() { $(which pip) "$@"; }
function psg() { ps auxww | grep -i --color=always "$@" | grep -v grep | collapse | cuts -f 2,11-; }
function sb() { . ~/.bashrc; }
function serve-this() { python -m SimpleHTTPServer; }
function ssh() { bcvi --wrap-ssh --; }
function tf() { tail -f "$@"; }
function urldecode() { python -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])" "$@"; }
function urlencode() { python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);" "$@"; }
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

# Mobile dev {{{

ANDROIDSDK="${HOME}"/opt/android-sdk

android() { "${ANDROIDSDK}"/tools/android; }

adb() {
    local ADB="${ANDROIDSDK}"/platform-tools/adb
    case $1 in
        ls)
            ${ADB} devices -l
            ;;
        sc)
            local dest=${2:-screenshot.png}
            ${ADB} shell screencap -p | perl -pe 's/\x0D\x0A/\x0A/g' > ${dest}
            ;;
        *)
            ${ADB} "$@"
            ;;
    esac
}

# iOS emulator id
_iei() { xcrun simctl list devices | grep -v unavailable | grep 'iPhone 5s' | awk '{print $3}' | tr -d \(\); }


# Emulators
ea4() { ${ANDROIDSDK}/tools/emulator -avd nexus4 -scale 0.40 & }
ea5() { ${ANDROIDSDK}/tools/emulator -avd nexus5 -scale 0.29 & }
ei4s() { open -a "iOS Simulator" --args -CurrentDeviceUDID $(_iei 'iPhone 4s'); }
ei5s() { open -a "iOS Simulator" --args -CurrentDeviceUDID $(_iei 'iPhone 5s'); }
ei6() { open -a "iOS Simulator" --args -CurrentDeviceUDID $(_iei 'iPhone 6'); }

# Titanium {{{

ti() { myreattach-to-user-namespace appc ti --no-color --shadow "$@"; }

tiba() { ti build --platform android "$@"; }
tibad() { ti build --platform android "$@" --target device; }
tiba4() { ti build --platform android "$@" --device-id nexus4; }
tiba5() { ti build --platform android "$@" --device-id nexus5; }

tibi() { ti build --platform ios "$@"; }
tibid() { tibi --target device; }
tibi4s() { tibi -C $(_iei 'iPhone 4s') "$@"; }
tibi5s() { tibi -C $(_iei 'iPhone 5s') "$@"; }
tibi6() { tibi -C $(_iei 'iPhone 6') "$@"; }

ticl() { ti clean; rm -rf build Resources; }

tilog() {
    local package=$(xpath tiapp.xml '//ti:app/id/text()')
    adb logcat | grep `adb shell ps | grep $package | cut -c10-15`
}

# }}}
# }}}
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

wrap_alias g git ''
wrap_alias h hg ''

# }}}
# Prompt {{{

D=$'\e[37;40m'
PINK=$'\e[35;40m'
GREEN=$'\e[32;40m'
ORANGE=$'\e[33;40m'
CYAN=$'\e[36;40m'
RED=$'\e[31;40m'

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
    hg_ps1
    git_ps1
}

venv_ps1() {
    [ $VIRTUAL_ENV ] && echo "${ORANGE}>>"`basename $VIRTUAL_ENV`"<<${D}"
}

rcs_promp() {
    echo '$'
}

prompt() {
    if [[ $EXITVAL == 0 ]]; then
        echo -n "$"
    else
        echo -n "${RED}$EXITVAL \$${D}"
    fi
}

# Inspired by: https://gist.github.com/3083586
prompt_command() {
    EXITVAL=$?; 

    z --add `pwd`
}
export PROMPT_COMMAND='prompt_command'
export PS1='\n${PINK}\u${D} at ${ORANGE}\h${D} in ${GREEN}\w${D} $(rcs_ps1) $(venv_ps1)\n$(prompt) '

# }}}
