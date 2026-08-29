# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

OS_MAC=
case $HOSTNAME in
    beast.local) OS_MAC=true;;
    skinny.local) OS_MAC=true;;
esac

if [ -z "${LANG}" -o -n "$OS_MAC" ]; then
    LANG=en_US.UTF-8
    export LANG
fi

if [ -z "${LC_ALL}" -o -n "$OS_MAC" ]; then
    LC_ALL=$LANG
    export LC_ALL
fi

if [ -z "${MM_CHARSET}" -o -n "$OS_MAC" ]; then
    MM_CHARSET=UTF-8
    export MM_CHARSET
fi

if [ -z "${TMP}" ]; then
    if [ -n "${TMPDIR}" ]; then
        TMP=${TMPDIR}
        export TMP
    elif [ -d /tmp ]; then
        TMP=/tmp
        export TMP
    fi
fi


# MacOS has the habit, every once in a while, to _break_ Nix (i.e. stop
# loading it up).  Rather than messing around with the global bashrc file (e.g.
# /etc/bashrc, or /etc/zshrc), I decided to suck it up, and add the
# initialization script to this file instead
#
# Note: it should be safe to load the script over and over again, as the first
# thing it does, is to check and set the initialization variable:
# __ETC_PROFILE_NIX_SOURCED
# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
    . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

envvarcontains() {
    eval "echo \$$1" | grep -Eq "(^|:)$2($|:)"
}

envvarmunge () {
    if ! envvarcontains $1 $2 ; then
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

XDG_DATA_DIRS=${XDG_DATA_DIRS:-/usr/local/share:/usr/share}
if [ -d $HOME/.nix-profile/share ]; then
    envvarmunge XDG_DATA_DIRS $HOME/.nix-profile/share
fi

# the default umask is set in /etc/login.defs
umask 022

if [ -z "$JAVA_HOME" ]; then
    if [ -f /usr/libexec/java_home ]; then
        export JAVA_HOME=$(/usr/libexec/java_home)
    fi
fi

# Not needed anymore?  So long as we have $HOME/local/bin
# inside $PATH, `man` should be able to guess a man folder from there (i.e.
# $HOME/local/bin/../man)
# envvarmunge MANPATH $HOME/local/man
# Also...https://github.com/NixOS/nix/pull/1782#issuecomment-356967799
mkdir -p /tmp/nix-man-hack/bin
if [ ! -h /tmp/nix-man-hack/man ]; then
    ln -sf /Users/matteolandi/.nix-profile/share/man /tmp/nix-man-hack/man
fi
envvarmunge PATH /tmp/nix-man-hack/bin
envvarmunge PATH /usr/sbin
envvarmunge PATH /usr/local/sbin
envvarmunge PATH /mnt/c/Windows/System32/WindowsPowerShell/v1.0
envvarmunge PATH /mnt/c/Windows/System32/

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    . $HOME/.nix-profile/etc/profile.d/nix.sh
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
fi

envvarmunge PATH $HOME/.local/bin
envvarmunge PATH $HOME/local/bin
# test -d $HOME/perl5/bin                    && export PATH="$HOME/perl5/bin$:$PATH"
# test -d $HOME/.rvm/bin                     && export PATH="$HOME/.rvm/bin:$PATH"
envvarmunge PATH $HOME/.docker/bin
envvarmunge PATH $HOME/.roswell/bin
envvarmunge PATH $HOME/.cargo/bin
envvarmunge PATH $HOME/.lmstudio/bin

case $PATH in
    *brew/bin* ) ;;
    *)
        if [ -f /home/linuxbrew/.linuxbrew/bin/brew ]; then
            eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
        elif [ -f /opt/homebrew/bin/brew ]; then
            eval "$(/opt/homebrew/bin/brew shellenv)"
        fi
        ;;
esac
envvarmunge PATH ${HOMEBREW_PREFIX}/opt/curl/bin
envvarmunge PATH ${HOMEBREW_PREFIX}/opt/bc/bin
envvarmunge PATH ${HOMEBREW_PREFIX}/opt/rustup/bin
envvarmunge PATH ${HOMEBREW_PREFIX}/opt/mysql-client/bin
# local
envvarmunge PATH $HOME/bin

if [ -e $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh ]; then . $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh; fi
export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}

# include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

# always force a tmux session
# if [ -z "$TMUX_CLIENT" ]; then
#     ~/bin/ta .
# fi
