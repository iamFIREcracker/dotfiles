# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

if [[ $- == *i* ]]; then
    # Use full path here.. ~/bin will be added to PATH only later
    ~/bin/clear-screen-and-position-cursor-at-the-bottom
    ~/bin/motd
fi

envvarcontains() {
    eval "echo \$$1" | grep -Eq "(^|:)$2($|:)"
}

envvarmunge () {
    if ! envvarcontains $2 ; then
        if [ "$3" = "after" ] ; then
            eval $1=\$$1:$2
        else
            eval $1=$2:\$$1
        fi
    fi
}

envvarremovepart() {
    eval $1=:\$$1:
    if [ "$3" = "all" ]; then
        eval $1=\${$1//:$2:/:}
    else
        eval $1=\${$1/:$2:/:}
    fi
    eval $1=\${$1#:}
    eval $1=\${$1%:}
}

if [ -d $HOME/.nix-profile/share ]; then
    envvarmunge XDG_DATA_DIRS $HOME/.nix-profile/share
fi

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

# the default umask is set in /etc/login.defs
umask 022

if [ -z "$JAVA_HOME" ]; then
    if [ -f /usr/libexec/java_home ]; then
        export JAVA_HOME=$(/usr/libexec/java_home)
    fi
fi

envvarmunge MANPATH $HOME/local/man

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


pathremove() {
    PATH=:$PATH:
    if [ "$2" = "all" ]; then
        PATH=${PATH//:$1:/:}
    else
        PATH=${PATH/:$1:/:}
    fi
    PATH=${PATH#:}; PATH=${PATH%:}
}

envvarmunge PATH /usr/sbin
envvarmunge PATH /usr/local/sbin

# test -d /usr/local/opt/python@2/bin    && export PATH="/usr/local/opt/python@2/bin:$PATH"
# test -d /usr/local/opt/python@3.9/bin  && export PATH="/usr/local/opt/python@3.9/bin:$PATH"
# test -d /usr/local/opt/ruby/bin        && export PATH="/usr/local/opt/ruby/bin:$PATH"
# test -n "$JAVA_HOME"                   && export PATH="$JAVA_HOME/bin:$PATH"
# test -n "$M2_HOME"                     && export PATH="$M2_HOME/bin:$PATH"

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . /home/ubuntu/.nix-profile/etc/profile.d/nix.sh; fi
# - Opening a new window with `tmux` will create a new `bash` login shell,
#   which amongst other things will source this file.
# - Sourcing nix.sh will **always** prepend $HOME/.nix-profile/bin to $PATH.
# - However, there is a reason why we are loading nix here and not, say, at the
#   bottom of this file: we want to be able to use `nvm`, and we want those
#   binaries to take precendence over nix ones.
# - So what do we do? We source nix.sh, and immediately remove
#   $HOME/.nix-profile/bin from path; then we check if $HOME/.nix-profile/bin
#   is still contained inside $PATH, and if it is (i.e. nested login shell),
#   then we move on and rely on the fact that nix was loaded at the right time;
#   otherwise, we manually add $HOME/.nix-proifile/bin to $PATH
envvarremovepart PATH $HOME/.nix-profile/bin
if ! envvarcontains PATH $HOME/.nix-profile/bin; then 
    envvarmunge PATH $HOME/.nix-profile/bin
fi

envvarmunge PATH $HOME/local/bin
# test -d $HOME/perl5/bin                    && export PATH="$HOME/perl5/bin$:$PATH"
# test -d $HOME/.rvm/bin                     && export PATH="$HOME/.rvm/bin:$PATH"
envvarmunge PATH $HOME/bin

if [ -e $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh ]; then . $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh; fi
export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}

# include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi
