# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

if [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
elif [ -f /usr/local/share/bash-completion/bash_completion ]; then
    # brew bash-completion@2
    source /usr/local/share/bash-completion/bash_completion
elif [ -f /usr/local/etc/bash_completion ]; then
    # brew bash-completion
    source /usr/local/etc/bash_completion
elif [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
fi

# the default umask is set in /etc/login.defs
umask 022

# include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

test -d /usr/local/opt/node@6/bin && export PATH="/usr/local/opt/node@6/bin:$PATH"
test -d /usr/local/sbin           && export PATH="/usr/local/sbin:$PATH"
test -d ~/bin                     && export PATH="~/bin:$PATH"
test -d ~/npm/bin                 && export PATH="~/npm/bin:$PATH"
test -d ~/opt/PathPicker          && export PATH="~/opt/PathPicker:$PATH"
test -d ~/opt/cb                  && export PATH="~/opt/cb:$PATH"
test -d ~/opt/tmux                && export PATH="~/opt/tmux:${PATH}"

# do the same with MANPATH
if [ -d ~/man ]; then
    MANPATH=~/man:"${MANPATH}"
    export MANPATH
fi


if [ -z "${LANG}" ]; then
    LANG=en_US.UTF-8
    export LANG
fi

if [ -z "${MM_CHARSET}" ]; then
    MM_CHARSET=UTF-8
    export MM_CHARSET
fi

if [ -z "${LC_ALL}" ]; then
    LC_ALL=$LANG
    export LC_ALL
fi

