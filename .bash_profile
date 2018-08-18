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

if [ -z "$JAVA_HOME" ]; then
    if [ -f /usr/libexec/java_home ]; then
        export JAVA_HOME=$(/usr/libexec/java_home)
    fi
fi

if [[ -z $TMUX ]]; then
    test -d /usr/sbin                      && export PATH="/usr/sbin:$PATH"
    test -d /usr/local/sbin                && export PATH="/usr/local/sbin:$PATH"
    test -d /usr/local/opt/node@8/bin      && export PATH="/usr/local/opt/node@8/bin:$PATH"
    test -d /usr/local/opt/python@2/bin    && export PATH="/usr/local/opt/python@2/bin:$PATH"
    test -n "$JAVA_HOME"                   && export PATH="$JAVA_HOME/bin:$PATH"
    test -n "$M2_HOME"                     && export PATH="$M2_HOME/bin:$PATH"
    test -d ~/bin                          && export PATH="$HOME/bin:$PATH"
    test -d ~/perl5/bin                    && export PATH="$HOME/perl5/bin$:$PATH"
    test -d ~/npm/bin                      && export PATH="$HOME/npm/bin:$PATH"
    test -d ~/rubygems/bin                 && export PATH="$HOME/rubygems/bin:$PATH"
    test -d ~/opt/tmux                     && export PATH="$HOME/opt/tmux:$PATH"
    test -d ~/opt/winpty/build/            && export PATH="$HOME/opt/winpty/build:$PATH"

    # do the same with MANPATH
    if [ -d ~/man ]; then
        MANPATH="$HOME/man:$MANPATH"
        export MANPATH
    fi

    test -d ~/lib/python              && export PYTHONPATH="$HOME/lib/python:$PYTHONPATH"
fi

if [ -z "${LANG}" ]; then
    LANG=en_US.UTF-8
    export LANG
fi

if [ -z "${LC_ALL}" ]; then
    LC_ALL=$LANG
    export LC_ALL
fi

if [ -z "${MM_CHARSET}" ]; then
    MM_CHARSET=UTF-8
    export MM_CHARSET
fi

# include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

