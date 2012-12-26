#!/usr/bin/env bash

# Utility script which lookups for the definition of the word (or words)
# contained inside positional arguments or for everything stored inside the WM
# clipboard.
#
# Depends on xclip and xdg-open

[ $# != 0 ] && data="$@" || data=$(xclip -o)

gnome-dictionary "${data}"
