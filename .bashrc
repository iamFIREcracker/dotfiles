# Platform {{{

OS_WIN=$(uname -rs | grep -e CYGWIN -e Microsoft)

# }}}
# Bash {{{

if [[ $- == *i* ]]; then
    # Abort piped command ASAP
    set -o pipefail

    # Update window size after every command
    shopt -s checkwinsize

    # merge / append histories
    shopt -s histappend

    # Save multi-line commands as one command
    shopt -s cmdhist

    # Disable terminal scroll lock
    stty -ixon

    # Don't 'susp' with C-Z (default)
    stty susp undef
fi

# Avoid duplicate entries, and skip entries with a leading whitespace
export HISTCONTROL="erasedups:ignoreboth:ignorespace"

export HISTSIZE=10000

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
GREEN=$'\e[0;32m'
ORANGE=$'\e[0;33m'
BLUE=$'\e[0;34m'
PINK=$'\e[0;35m'
CYAN=$'\e[0;36m'
RED=$'\e[0;31m'
WHITE=$'\e[0;97m'

# }}}
# Vim mode {{{

# set -o vi

# I give up
alias :e=vim
alias :q=exit
alias :qa=exit

# }}}
# Environment variables {{{

# FZF {{{

export FZF_DEFAULT_COMMAND='ag --hidden --nocolor -g ""'

# }}}
# General {{{

export EDITOR="vim"
export PAGER="/usr/bin/less"
export HGEDITOR="~/bin/hgeditor"
export BROWSER=pn

# }}}
# Java et al. {{{

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

# }}}
# Nvm {{{

export NVM_DIR="$HOME/.nvm"

# MacOS
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# Every other OS I use
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Immediately load the right version
[ -f ".nvmrc" ] && nvm use

# }}}
# Ruby {{{

export GEM_HOME=~/rubygems

# }}}
# Python {{{

export PYTHONSTARTUP="~/.pythonrc.py"
export VIRTUAL_ENV_DISABLE_PROMPT=1

# }}}

if [ -f ~/.env.properties ]; then
    set -a # export all variables created next
    source ~/.env.properties
    set +a # stop exporting
fi

# }}}
# Extra {{{

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

# }}}
# Useful functions {{{

# Quick editing {{{

ea()  { vim ~/.config/alacritty/alacritty.yml; }
eS()  { vim ~/.ssh/config; }
eT()  { vim ~/.tmuxinator/$(tmux display-message -p '#S').yml; }
eb()  { vim ~/dotfiles/.bashrc; }
eb1() { vim ~/my-env/opt/bunny1/b1_custom.py; }
eg()  { vim ~/dotfiles/.gitconfig; }
ek()  {
  if [ -n "${OS_WIN}" ]; then
    vim ~/my-env/Windows/AutoHotkey/KeyMappings.ahk
  else
    vim ~/.config/karabiner/karabiner.json
  fi
}
em()  { vim ~/.muttrc; }
ep()  {
  if [ -n "$1" ]; then
    vim ~/plan/.$1.plan
  elif [ -n "${OS_WIN}" ]; then
    vim ~/plan/.work.plan
  else
    vim ~/plan/.plan
  fi
}
eP()  { vim ~/plan/; }
es()  { vim ~/dotfiles/.slate; }
et()  { vim ~/dotfiles/.tmux.conf; }
ev()  { vim ~/dotfiles/.vim/vimrc; }

function ew() { vim $(which "$1"); }
complete -c ew -w which
function cw() { cat $(which "$1"); }
complete -c cw -w which

elinks() { $EDITOR ~/Dropbox/links.txt; }
etodos() { $EDITOR ~/Dropbox/todos.txt; }
enburls() { $EDITOR ~/.newsboat/urls; }

# }}}

function ..() {    cd ../"$@"; }
function ...() {   cd ../../"$@"; }
function ....() {  cd ../../../"$@"; }
function .....() { cd ../../../../"$@"; }


echo_n_run() {
    echo "$@"
    "$@"
}

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
function a() { ag --hidden --smart-case "$@"; }
function b() { bower "$@"; }
function banner() { figlet -w9999 "$@" | cowsay -W 9999 -n -p | lolcat; }
function b1() { ~/opt/bunny1/venv/bin/python ~/opt/bunny1/b1_custom.py --test "$*"; }
function brewski() { brew update && brew upgrade && brew cleanup; brew doctor; }
# cat {{{

if hash gcat 2>/dev/null; then
    _cat=gcat
else
    _cat=$(which cat)
fi
function cat() {
    ${_cat} "$@"
}

# }}}
function cleancodes() { sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"; }
function collapse() { sed -e 's/  */ /g'; }
function cols() { collapse | cuts -f "$@"; }
function cuts() { cut -d' ' "$@"; }
# Dockerfile templates {{{

function Dockerfile-ng() {
    cat <<EOF >> Dockerfile
FROM node:8.11.3-alpine

WORKDIR /usr/src/app

COPY package.json package.json
RUN npm install
RUN npm install @angular-devkit/core

ADD . ./

EXPOSE 4200
CMD npm start
EOF
    echo 'created Dockerfile...'
    cat Dockerfile
    echo 'build with:'
    echo 'docker build -t web-exercise .'
    echo 'run with:'
    echo 'docker run -it --rm -p 4200:4200 --name web-exercise web-exercise'
}
function Dockerfile-tomcat() {
    cat <<EOF >> Dockerfile
FROM tomcat:8-jre8

# Copy to images tomcat path
ADD target/BE-ION.war /usr/local/tomcat/webapps/
EOF
    echo 'created Dockerfile...'
    cat Dockerfile
    echo 'build with:'
    echo 'docker build -t web-exercise-tomcat .'
    echo 'run with:'
    echo 'docker run -it --rm -p 8080:8080 --name web-exercise-tomcat web-exercise-tomcat'
}

# }}}
function de() { deactivate; }
function ungron() { gron --ungron "$@"; }
function uniqdiff() {
    local input=/tmp/uniqdiff_all.$$
    trap "kill -TERM $PID; rm '${input}'" TERM INT
    cat > ${input}
    </dev/tty vimdiff <(cat ${input}) <(cat ${input} | uniq "$@")
    rm "${input}"
}
function uniqdiff1() { uniqdiff --skip-fields 1; }
function from() { tac "$1" | sed "/$2/q" | tac; }
function fucking-kill-nfsd() {
    # https://github.com/hashicorp/vagrant/issues/8103
    sudo sh -c "> /etc/exports"
    sudo nfsd restart
}
# Git {{{

function g() { git "$@"; }
hash __git_complete 2>/dev/null && __git_complete g __git_main

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
        "${@:4}" \
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

# }}}
function histgrep() { history | grep "$@" | tac; }
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
    tr '' '\n' | tr -s '\n' '	'
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
# math {{{

function math() {
    bc -l -q <(echo "$@"; echo "quit")
}

# }}}
# maven {{{


function mvn() {
    if [ -n "${OS_WIN}" ]; then
        winpty "$(cygpath -u $M2_HOME/bin)" "$@"
    else
        $(which mvn) "$@"
    fi
}

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

# }}}
# mutt {{{

function muttw() {
    (cd ~/Downloads && "$(which mutt)" "$@")
}
function mutt()      { muttw -F ~/Dropbox/mutt/matteo-matteolandi.net.muttrc; }
function mutt-work() { muttw -F ~/Dropbox/mutt/matteo.landi-iongroup.com.muttrc; }
function mutt-pec()  { muttw -F ~/Dropbox/mutt/landimatte-pec.it.muttrc; }

# }}}
# Node.js/NPM {{{

if hash winpty 2>/dev/null; then
    _node='winpty node'
else
    _node=$(which node)
fi
node() { ${_node} "$@"; }

if hash winpty 2>/dev/null; then
    _npm='winpty npm.cmd'
else
    _npm=$(which npm)
fi
npm() { ${_npm} "$@"; }

n() { npm "$@"; }
hash _npm_completion 2>/dev/null && complete -o default -F _npm_completion n

# }}}
function median() { percentile 50; }
function o() { open "$@"; }
function oo() { open .; }
function password() {
  cat /dev/urandom | LC_ALL=C tr -dc _A-Z-a-z-0-9 | head -c${1:-32}
  echo # new lines are good!
}
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
function ports { sudo lsof -iTCP -sTCP:LISTEN -P -n | gc "${1-.}"; }
function psa { ps aux | grep '${@}'; }
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
# SSH monochrome {{{

function ssh-mono() {
    TERM=vt220 ssh "$@" -t "clear;bash"
    unfuck && clear
}
function ssh-red() {
    printf "\x1b[41m"
    ssh-mono "$@"
}
function ssh-green() {
    printf "\x1b[42m"
    ssh-mono "$@"
}
function ssh-orange() {
    printf "\x1b[43m"
    ssh-mono "$@"
}
function ssh-blue() {
    printf "\x1b[44m"
    ssh-mono "$@"
}
function ssh-purple() {
    printf "\x1b[45m"
    ssh-mono "$@"
}

# }}}
function sb() { . ~/.bashrc; }
# Sed cross-OS wrapper {{{

if hash gsed 2>/dev/null; then
    _sed=gsed
else
    _sed=$(which sed)
fi
function sed() {
    ${_sed} "$@"
}

#}}}
function serve-this() { python -m SimpleHTTPServer "$@"; }
function sleeplees() {
    pmset -g assertions | egrep '(PreventUserIdleSystemSleep|PreventUserIdleDisplaySleep)'
}
function sum() { awk '{s+=$1}END{print s}'; }
# tac {{{

if hash gtac 2>/dev/null; then
    _tac=gtac
else
    _tac=$(which tac)
fi
function tac() {
    ${_tac} "$@"
}

# }}}
function to() { sed "/$1/q"; }
function tf() { tail -f "$@"; }
function tunnel() {
    local _server
    local _local_port
    local _service_host
    local _service_port
    read -p "server: " _server
    read -p "local port: " _local_port
    read -p "service host: " _service_host
    read -p "service port: " _service_port

    echo_n_run ssh ${_server} -L ${_local_port}:${_service_host}:${_service_port} -N
}
function unfuck() { echo "${D}"; }
function urldecode() { python -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])" "$@"; }
function urlencode() { python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);" "$@"; }
# Vagrant {{{

if hash winpty 2>/dev/null; then
    _vagrant='winpty vagrant'
else
    _vagrant=$(which vagrant)
fi
vagrant() { ${_vagrant} "$@"; }
v() { vagrant "$@"; }

# }}}
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
# Hosts {{{

function matteolandi {
    echo_n_run ssh-red matteo@matteolandi.net
}

# }}}
# Prompt {{{

git_ps1() {
    local branch=$(git currentbranch)
    local status=$(git_prompt_status)
    echo "on ${PINK}${branch}${D}${GREEN}${status}${D}"
}

hg_ps1() {
    local branch=$(hg branch)
    local status=$(hg_prompt_status)
    echo "on ${PINK}${branch}${D}${GREEN}${status}${D}"
}

rcs_ps1() {
    if [ -n "$PROMPT_NO_RCS" ]; then
        echo
    else
        if git root >/dev/null 2>&1; then
            git_ps1
        elif hg st >/dev/null 2>&1; then
            hg_ps1
        fi
    fi
}

venv_ps1() {
    [ $VIRTUAL_ENV ] && echo "${ORANGE}>>"`basename $VIRTUAL_ENV`"<<${D}"
}

actual_prompt() {
    local exit=$1

    if [[ $exit -eq 0 ]]; then
        echo -n "$ "
    else
        echo -n "$exit $ "
    fi
}

# Inspired by: https://gist.github.com/3083586
prompt_command() {
    local actual=$(actual_prompt $?)

    z --add `pwd`

    # Record each line as it gets issued
    history -a

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
    PS1="$PS1${WHITE}${USER}${D}"                 # username
    PS1="$PS1 at ${CYAN}${HOSTNAME}${D}"          # hostname
    PS1="$PS1 in ${UNDERLINE}${PWD}${D}"          # cwd
    PS1="$PS1 $(rcs_ps1)"                         # git/mercurial/svn
    PS1="$PS1 $(venv_ps1)"                        # python's virtualenv
    PS1="$PS1\n"                                  # new line
    PS1="$PS1${actual}"                           # the actual prompt
    export PS1
}


if [[ $- == *i* ]]; then
    clear-screen-and-position-cursor-at-the-bottom
    export PROMPT_COMMAND='prompt_command'
fi

# }}}
