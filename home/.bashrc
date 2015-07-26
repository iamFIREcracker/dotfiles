# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=erasedups

# size of history
export HISTSIZE=10000

# merge / append histories
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# enable colors ..
colors=`which dircolors`
if [ -n "${colors}" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
else
    # .. on FreeBSD ..
    #LSCOLORS="ExGxFxdxCxDxDxhbadExEx"
    #export LSCOLORS
    alias ls='TERM=xterm-16color ls -G'
fi
alias l='tree -aC -L 1 -pughD'

# Term
TERM=screen-256color
export TERM

# Editor
EDITOR="vim"
export EDITOR

# Pager
PAGER="/usr/bin/less"
export PAGER

# Less default options
LESS="-FSRXKQ"
export LESS

# PYTHONPATH
PYTHONPATH="$HOME/lib/python:$PYTHONPATH"
export PYTHONPATH
PYTHONSTARTUP="$HOME/.pythonrc.py"
export PYTHONSTARTUP

# Better virtualenv
export VIRTUAL_ENV_DISABLE_PROMPT='1'

# NPM
PATH="${HOME}/npm/bin:$PATH"
export PATH

# JAVA_HOME
if [ -z "$JAVA_HOME" ]; then
    if [ -f /usr/libexec/java_home ]; then
        JAVA_HOME=$(/usr/libexec/java_home)
        export JAVA_HOME
    fi
fi

# hg-editor
HGEDITOR="~/bin/hgeditor"
export HGEDITOR

# git-editor
GIT_EDITOR="~/bin/giteditor"
export GIT_EDITOR

# CVS wrappers
alias h='hg'
alias g='git'

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
    if hg prompt >/dev/null 2>&1; then
        hg prompt "{on ${PINK}{branch}${D}}${GREEN}{status}${D}"
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


# ack
function a() { ack "$@"; }

# Virtualenv shortcuts
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
function de() { deactivate; }

# Make pip operation safe!
alias pip-sys="`which pip`"
pip() {
    if [ -n "$VIRTUAL_ENV" ]
    then `which pip` "$@"
    else echo "Not currently in a venv -- use pip-sys to work system-wide."
    fi
}
pipf() {
    pip freeze > requirements.txt
}


# BCVI stuff
alias bcvi='${HOME}/opt/bcvi/bin/bcvi'
alias ssh="bcvi --wrap-ssh --"


# Android
export ANDROIDSDK="${HOME}"/opt/android-sdk
alias android="${ANDROIDSDK}"/tools/android

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

# Titanium
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

# Emulators
ea4() { ${ANDROIDSDK}/tools/emulator -avd nexus4 -scale 0.40 & }
ea5() { ${ANDROIDSDK}/tools/emulator -avd nexus5 -scale 0.29 & }
ei4s() { open -a "iOS Simulator" --args -CurrentDeviceUDID $(_iei 'iPhone 4s'); }
ei5s() { open -a "iOS Simulator" --args -CurrentDeviceUDID $(_iei 'iPhone 5s'); }
ei6() { open -a "iOS Simulator" --args -CurrentDeviceUDID $(_iei 'iPhone 6'); }

# Runapp
ka() {
    kill `cat .bgrun.pid`
}
ra() {
    rk; bgrun "python run_app.py"
    watchmedo shell-command \
        --recursive \
        --wait \
        --patterns='*.py;*.html;*.js' \
        --command='echo "${watch_src_path}"; bash -c "kill `cat .bgrun.pid`; bgrun \"python run_app.py\""'
}

# Useful functions {{{

eb() { vim ~/.bashrc; }
eg() { vim ~/.gitconfig; }
eh() { vim ~/.hgrc; }
ep() { vim ~/.pentadactylrc; }
es() { vim ~/.slate; }
ev() { vim ~/.vimrc; }

function ..() {     cd ..; }
function ...() {    cd ../..; }
function ....() {   cd ../../..; }
function .....() {   cd ../../../..; }

edit-pasteboard() {
    cb | vipe | cb
}

function rldmyfuckinbashrc() { . ~/.bashrc; }

# }}}
# Vim {{{

# I give up
alias :q=exit
alias :qa=exit

bind -m vi-command '"H":beginning-of-line'
bind -m vi-command '"L":end-of-line'

set -o vi

# }}}
