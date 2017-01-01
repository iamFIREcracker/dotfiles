#!/bin/sh

pgrep mutt || exit 0

PID=`cat ~/.offlineimap/pid`
ps aux | grep "[ ]$PID" && kill $PID
OFFLINEIMAP=`which offlineimap`

function sync_normal {
    echo "NORMAL Sync"
    ${OFFLINEIMAP} -o -u quiet
}

function sync_quick {
    echo "QUICK Sync"
    ${OFFLINEIMAP} -o -q -u quiet
}

# This is silly.
#python -c'import sys, random; sys.exit(random.randint(0, 5))' && sync_normal || sync_quick
sync_normal

exit 0
