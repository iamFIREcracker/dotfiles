# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

if [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
elif [ -f /usr/local/etc/profile.d/bash_completion.sh ]; then
    # brew bash-completion@2
    source /usr/local/etc/profile.d/bash_completion.sh
elif [ -f /usr/local/etc/bash_completion ]; then
    # brew bash-completion
    source /usr/local/etc/bash_completion
elif [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
fi

# For God knows what reason, on Linux, `git` completion functions would not be
# loaded until you manually trigger them:
#
#   > git <tab>
#
# So here we manually source the file, if available
if [ -f /usr/share/bash-completion/completions/git ]; then
  source /usr/share/bash-completion/completions/git
fi

# Load NPM's completion
if hash npm 2>/dev/null; then
  eval "$(npm completion)"
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
    test -d /usr/local/opt/python@2/bin    && export PATH="/usr/local/opt/python@2/bin:$PATH"
    test -d /usr/local/opt/python@3/bin    && export PATH="/usr/local/opt/python@3/bin:$PATH"
    test -n "$JAVA_HOME"                   && export PATH="$JAVA_HOME/bin:$PATH"
    test -n "$M2_HOME"                     && export PATH="$M2_HOME/bin:$PATH"
    test -d ~/local/bin                    && export PATH="$HOME/local/bin:$PATH"
    test -d ~/perl5/bin                    && export PATH="$HOME/perl5/bin$:$PATH"
    test -d ~/npm/bin                      && export PATH="$HOME/npm/bin:$PATH"
    test -d ~/rubygems/bin                 && export PATH="$HOME/rubygems/bin:$PATH"
    test -d ~/bin                          && export PATH="$HOME/bin:$PATH"

    test -d ~/local/man/                   && export MANPATH="$HOME/local/man:$MANPATH"
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
