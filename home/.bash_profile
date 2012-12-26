# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/login.defs
umask 022

# include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# the rest of this file is commented out.

# set PATH so it includes user's private bin if it exists
if [ -d ~/bin ] ; then
    PATH=~/bin:"${PATH}"
    export PATH
fi

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
