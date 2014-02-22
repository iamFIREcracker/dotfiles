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

# More colors
[ $(uname) = 'Linux' ] && TERM=gnome-256color || TERM=rxvt-256color
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

# Better virtualenv
export VIRTUAL_ENV_DISABLE_PROMPT='1'

# Scala Build Tool
PATH="$PATH:$HOME/opt/sbt/bin"
export PATH

# Scala
PATH="$PATH:/home/matteo/opt/scala-2.10.2/bin"
export PATH

# NPM
PATH="$PATH:/home/matteo/npm/bin"
export PATH

# Productivity settings
# > cat ~/.bash_history | frequency | sort -nr | head
alias frequency='sort | uniq -c | sort -gr'
# > cat .bash_history | sfrequency | sort -nr | head
alias sfrequency='awk "{print \$1}" | frequency'

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

        # display the content of the new dir
        ls -tAl | head -7 > ~/.postprompt
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
alias P='. ~/workspace/productivity/productivity.sh'


# hg-editor
HGEDITOR="~/bin/hgeditor"
export HGEDITOR

export SVN_EDITOR="~/workspace/svn-tools/svneditor"

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
    h prompt "{on ${PINK}{branch}${D}}{ at ${ORANGE}{bookmark}${D}}${GREEN}{status}${D}" 2> /dev/null
}

rcs_ps1() {
    s prompt 2> /dev/null && svn_ps1 && return
    h prompt 2> /dev/null && hg_ps1 && return
}

venv_ps1() {
    [ $VIRTUAL_ENV ] && echo "${ORANGE}>>"`basename $VIRTUAL_ENV`"<<${D}"
}

rcs_promp() {
    [ -f ~/.postprompt ] && cat ~/.postprompt && rm ~/.postprompt
    s prompt 2> /dev/null && echo 'ʂ' && return
    h prompt 2> /dev/null && echo '☿' && return
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

# text editing aliases
alias collapse="sed -e 's/  */ /g'"
alias cuts="cut -d' '"
# Count unique lines
alias count='sort | uniq -c | sort -n'


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

# Maven completion
_m2_make_goals()
{
  plugin=$1
  mojos=$2
  for mojo in $mojos
  do
    export goals="$goals $plugin:$mojo"
  done
}

_m2_complete()
{
  local cur goals

  COMPREPLY=()
  cur=${COMP_WORDS[COMP_CWORD]}
  goals='clean compile test install package deploy site'
  goals=$goals _m2_make_goals "eclipse" "eclipse"
  goals=$goals _m2_make_goals "idea" "idea"
  goals=$goals _m2_make_goals "assembly" "assembly"
  goals=$goals _m2_make_goals "plexus" "app bundle-application bundle-runtime descriptor runtime service"
  cur=`echo $cur | sed 's/\\\\//g'`
  COMPREPLY=($(compgen -W "${goals}" ${cur} | sed 's/\\\\//g') )
}

complete -F _m2_complete -o filenames mvn


alias pypi='bash "${HOME}/workspace/hacks/pypi-deploy/pypi-deploy.sh"'


# go back!
alias cdd='cd -'

# Most used grep combo
alias grep='grep --color=auto'
alias grepp='grep --recursive --line-number --with-filename --binary-files=without-match --exclude-dir=.svn'
alias greppi='grepp --ignore-case'

# cd aliases
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."

# pygmentize
alias hi="pygmentize"

function cdiff
{
    diff "$@" | pygmentize -l diff
}

# gvim shortcut
alias gv="gvim ."
alias tv="vim `mktemp`"

function m {
    bc -l <<EOF
define r(x, n) {
return(x/2^n)
}
define l(x, n) {
return(x*2^n)
}
$@
EOF
}

# change default modifier for dvtm
alias dvtm="dvtm -m "

# onchange shortcut
alias oc="bash ~/workspace/onchange/onchange.sh"

# Handy random function
alias rand='cat /dev/urandom | LC_ALL=C tr -dc _A-Z-a-z-0-9 | head -c '

function psg() {
    ps auxww | grep $* | grep -v grep | collapse | cuts -f 2,11-
}


# Beeeeeeeeeeep!
alias bp="echo -en '\a'"

# dd: go faster!!!!
alias ddd="dd bs=512K"


# Screenshot
alias wshot='import '
alias sshot='import -window root '

# include external libraries
alias p="bash ~/lib/bash/picobsd.sh"
alias n="bash ~/lib/bash/netmap.sh"
[ -f ~/lib/bash/pinganalysis.sh ] && source ~/lib/bash/pinganalysis.sh

alias say=espeak

alias jsonpp='python -mjson.tool | pygmentize -l javascript'
alias xmlpp='xmllint --format - | pygmentize -l xml'
alias htmlpp='pygmentize -l html'


alias mutt='cd ~/Desktop; mutt'

# ip
alias iip='ifconfig | grep "inet " | grep -v 127.0.0.1 | collapse | cuts -f 3 | cut -d":" -f 2'
alias eip='curl -s checkip.dyndns.org | grep -Eo "[0-9\.]+"'
alias ip='echo "`eip` (`iip`)"'


# logfilter
alias l='LFEDITOR="gvim FILE +ROW" logfilter -f 6'

# ack
alias a='ack-grep '

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

function xinject() {
    local appclass=$1
    local key=$2
    local active=$(xdotool getactivewindow)
    local options

    # Firefox does not listen to external events if not focused
    [[ ${appclass} =~ 'firefox' ]] && options='windowactivate --sync'
    xdotool search --class "${appclass}" ${options} key --clearmodifiers ${key}

    # We need to switch the focus back the the original window
    [[ ${appclass} =~ 'firefox' ]] && xdotool windowactivate ${active}
}

# Virtualenv `workon` wrapper which looks for `.venv` file containing the name
# of the virtual environment
function wo() {
    [ -n "$1" ] && workon "$1" || {
        local wd=`pwd`

        while [ $wd != '/' ]; do
            local venvfile=$wd/.venv

            if [ ! -e $venvfile ]; then
                wd=`dirname $wd`
            else
                workon `cat $venvfile`
                return
            fi
        done
    }
}

# Make pip operation safe!
alias pip-sys="`which pip`"

pip() {
    if [ -n "$VIRTUAL_ENV" ]
    then `which pip` "$@"
    else echo "Not currently in a venv -- use pip-sys to work system-wide."
    fi
}


# Fast sudo alias + redoer of the last command
# taken from: 
# http://alias.sh/do-sudo-command-or-do-sudo-last-typed-command-if-no-argument-given?destination=node/235
sd() {
    if [ $# == 0 ]; then
        sudo $(history -p '!!')
    else
        sudo "$@"
    fi
}

# BCVI stuff
alias bcvi='${HOME}/workspace/bcvi/bin/bcvi'
alias ssh="bcvi --wrap-ssh --"
alias vi="bcvi"


# Scala w/ Maven
alias mvn-arch-gen-scala="mvn archetype:generate \
      -DarchetypeGroupId=org.scala-tools.archetypes \
      -DarchetypeGroupId=net.alchim31.maven \
      -DarchetypeArtifactId=scala-archetype-simple  \
      -DarchetypeVersion=1.5 \
      -DremoteRepositories=http://scala-tools.org/repo-releases"


# Print some fancy stuff!
#if ! shopt -q login_shell; then
    #fortune | cowsay -n | lolcat -f
#fi
