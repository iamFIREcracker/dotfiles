#!/usr/bin/env bash

# Utility script which creates and display a QR code with the content of
# positional arguments or with everything stored within the WM clipboard.
#
# Depends on xclip, curl and xdg-open

set -e

TEMP=""
cleanup_exit() {
    rm -rf "$TEMP"
}

# Remove temporary files even if we get interrupted
trap "cleanup_exit" 0
trap "exit 255" HUP INT QUIT ABRT TERM

TEMP=$(mktemp --suffix=.png)
[ x$TEMP != x ] || {
    echo "Could not create temporary file!  Exiting." 1>&2
    exit 1
}

[ $# != 0 ] && DATA="$@" || DATA=$(xclip -o)
URL="http://chart.apis.google.com/chart?cht=qr&chs=400x400&chl=${DATA}&chld=H|0"
echo $URL
curl -L $URL > $TEMP

xdg-open $TEMP

# Wait for the sub-process to successfully open the image
sleep 3
