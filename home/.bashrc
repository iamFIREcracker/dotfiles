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
PYTHONPATH="$PYTHONPATH:$HOME/lib/python"
export PYTHONPATH
PYTHONSTARTUP="$HOME/.pythonrc.py"
export PYTHONSTARTUP

# Better virtualenv
export VIRTUAL_ENV_DISABLE_PROMPT='1'

# NPM
PATH="$PATH:${HOME}/npm/bin"
export PATH

# move automatically inside last used directory
if ! shopt -q login_shell && [ -f ~/.lastdir ]; then
    cd "`cat ~/.lastdir`"
fi

# prompt_command callback: update directory counter, save the current directory
# and (optionally) list last edited files.
function productivity_update() {
    local current

    current="`pwd`"
    if [ "${current}" != "${PRODUCTIVITY_LASTDIR}" ]; then
        # update directory count
        ~/workspace/productivity/productivity.sh update "${current}"
        export PRODUCTIVITY_LASTDIR="${current}"
    fi

    # we need to update .lastdir because another shell could have modified it
    # but we are the most recent process active
    echo "${current}" > ~/.lastdir
}
export PROMPT_COMMAND='productivity_update'
export PRODUCTIVITY_LASTDIR="`pwd`"
alias c='. ~/workspace/productivity/productivity.sh prompt frequently'

function _productivity
{
    local cur opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    opts=$(bash ~/workspace/productivity/productivity.sh list frequently | sed 's/[0-9: \-]*//' | xargs -n1 basename )

    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
}

complete -F _productivity c

# hg-editor
HGEDITOR="~/bin/hgeditor"
export HGEDITOR

# git-editor
GIT_EDITOR="~/bin/giteditor"
export GIT_EDITOR

# svn-editor
SVN_EDITOR="~/workspace/svn-tools/svneditor"
export SVN_EDITOR

# CVS wrappers
alias s='bash "${HOME}/workspace/svn-tools/s.sh"'
alias h='hg'
alias g='git'

# Bash shell
D=$'\e[37;40m'
PINK=$'\e[35;40m'
GREEN=$'\e[32;40m'
ORANGE=$'\e[33;40m'
CYAN=$'\e[36;40m'

svn_ps1() {
    s prompt "on ${PINK}{branch}${D}${GREEN}{status}${D}" 2> /dev/null
}

hg_ps1() {
    h prompt "{on ${PINK}{branch}${D}}${GREEN}{status}${D}" 2> /dev/null
}

rcs_not_skipped() {
    test ! -e ~/.skipRcs
    return $?
}

rcs_ps1() {
    rcs_not_skipped && s prompt 2> /dev/null && svn_ps1 && return
    rcs_not_skipped && h prompt 2> /dev/null && hg_ps1 && return
    echo $SKIP_RCS
}

venv_ps1() {
    [ $VIRTUAL_ENV ] && echo "${ORANGE}>>"`basename $VIRTUAL_ENV`"<<${D}"
}

rcs_promp() {
    rcs_not_skipped && s prompt 2> /dev/null && echo 'ʂ' && return
    rcs_not_skipped && h prompt 2> /dev/null && echo '☿' && return
    echo '$'
}

# Inspired by: https://gist.github.com/3083586
PROMPT_COMMAND='EXITVAL=$?; '$PROMPT_COMMAND
get_exitval() {
    if [[ $EXITVAL != 0 ]]; then
        echo -n "$EXITVAL "
    fi
}

export PS1='\n${PINK}\u${D} at ${ORANGE}\h${D} in ${GREEN}\w${D} $(rcs_ps1) $(venv_ps1)\n$(get_exitval)$(rcs_promp) '

# Reload bashrc
alias rldmyfuckinbashrc='source ~/.bashrc'


# Bash completion
for bashcomp in /etc/bash_completion /usr/local/etc/bash_completion; do
    [ -f ${bashcomp} ] && source ${bashcomp} && break
done

# Ssh hosts completion
function _ssh_hosts
{
    local cur opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    opts=$(history | sed -e 's/  */ /g' | grep ' \+[0-9]\+ *ssh' | sed 's/ [0-9]* ssh //')

    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
}

# Complete ssh hosts
complete -F _ssh_hosts ssh


alias pypi='bash "${HOME}/workspace/hacks/pypi-deploy/pypi-deploy.sh"'


# go back!
alias cdd='cd -'

# Most used grep combo
alias grep='grep --color=auto'

# cd aliases
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."


function psg() {
    ps auxww | grep $* | grep -v grep | collapse | cuts -f 2,11-
}


# dd: go faster!!!!
alias ddd="dd bs=512K"


alias mutt='cd ~/Desktop; mutt'



# ack
function a() { ack "$@"; }

function curll() {
    local url=$1
    local method=${2:-GET}
    local data="$3"
    [ $# -lt 2 ] && {
        echo gimmeurjson URL METHOD
        return
    }


    [ ${method} = "GET" ] && url="${url}?${data}"

    curl -s "${url}" -X "${method}" -d "${data}"
}

function gimmeurjson() {
    local url=$1
    local method=$2
    local data="$3"

    [ $# -lt 2 ] && {
        echo gimmeurjson URL METHOD
        return
    }

    curll ${url} "${method}" "${data}" | jsonpp
}

function gimmeurxml() {
    local url=$1
    local method=$2
    local data="$3"

    curll ${url} "${method}" "${data}" | xmlpp
}

function hurl() {
    local url=$1
    local method=$2
    local data="$3"

    curll ${url} "${method}" "${data}" | htmlpp
}

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


# Fast sudo alias + redoer of the last command
# taken from: 
# http://alias.sh/do-sudo-command-or-do-sudo-last-typed-command-if-no-argument-given?destination=node/235
SUDO=`which sudo`
sd() {
    if [ $# == 0 ]; then
        ${SUDO} $(history -p '!!')
    else
        ${SUDO} "$@"
    fi
}
sudo() {
    echo "Use \`sd' instead!"
    sd "$@"
}

# BCVI stuff
alias bcvi='${HOME}/opt/bcvi/bin/bcvi'
alias ssh="bcvi --wrap-ssh --"
alias vi="bcvi"


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

# Titanium
ti() { myreattach-to-user-namespace `which ti` --no-color "$@"; }
tiba() { ti build --platform android "$@"; }
tibad() { ti build --platform android "$@" --target device; }
tiba4() { ti build --platform android "$@" --device-id nexus4; }
tiba5() { ti build --platform android "$@" --device-id nexus5; }
tibi() { ti build --platform ios "$@"; }
tibid() { tibi --target device; }
tibi4() { tibi -C 6901DD16-9C51-49BF-9A54-FF7088506377 "$@"; }
tibi5() { tibi -C 48B8AE98-32EE-4EA2-B933-2CF443ADEDB1 "$@"; }
tibi6() { tibi -C 1791BB08-34A2-493F-9A12-2D5A35FDF4C4 "$@"; }

# Redis

rs() {
    redis-cli "$@"
}

# Runapp
rk() {
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

# Print some fancy stuff!
#if ! shopt -q login_shell; then
    #fortune | cowsay -n | lolcat -f
#fi
