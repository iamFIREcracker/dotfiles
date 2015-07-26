# Bash {{{

# merge / append histories
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

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
# Useful functions {{{

eb() { vim ~/.bashrc; }
ef() { vim ~/.config/fish/config.fish; }
eg() { vim ~/.gitconfig; }
eh() { vim ~/.hgrc; }
ep() { vim ~/.pentadactylrc; }
es() { vim ~/.slate; }
ev() { vim ~/.vimrc; }

function ..() {     cd ..; }
function ...() {    cd ../..; }
function ....() {   cd ../../..; }
function .....() {   cd ../../../..; }



function a() { ack "$@"; }
function bcvi() { ${HOME}/opt/bcvi/bin/bcvi "$@"; }
function collapse() { sed -e 's/  */ /g'; }
function cuts() { cut -d' ' "$@"; }
function de() { deactivate; }
function edit-pasteboard() { cb | vipe | cb; }
function g() { git "$@"; }
function h() { hg "$@"; }
function hl() { grep -E --color=always --line-buffered "$1|\$"; }
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
function md() { mkdir -p "$@"; }
function o() { open "$@"; }
function oo() { open .; }
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
function rldmyfuckinbashrc() { . ~/.bashrc; }
function serve-this() { python -m SimpleHTTPServer; }
function ssh() { bcvi --wrap-ssh --; }
function urldecode() { python -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])" "$@"; }
function urlencode() { python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);" "$@"; }
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

# }}}
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
# Z {{{

. ~/opt/z/z.sh

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
