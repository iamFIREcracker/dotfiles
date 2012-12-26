#!/usr/bin/env bash

# Utility script which googles for the data contained inside positional
# arguments or for everything stored inside the WM clipboard.
#
# If the content of either positional argumets or clipboard starts with
# http,ftp,ecc (which means it is already a valid url), then skip the
# url-stuffing bit.
# 
# Depends on xclip and xdg-open

[ $# != 0 ] && data="$@" || data=$(xclip -o)

if [[ "${data}" =~ ^(http|ftp|telnet) ]] ; then
    # do nothing, data is already an url resource
    url=${data}
else
    url="http://www.google.com/search?&q=${data}"
fi

if pgrep firefox; then
    PROG=`which firefox`
elif pgrep chromium-browser; then
    PROG=`which chromium-browser`
else
    PROG=xdg-open
fi

echo ${PROG} "${url}"
