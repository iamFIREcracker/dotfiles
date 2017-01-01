#!/usr/bin/env bash

set -e

ARGS=
ARGS="$ARGS -1280x720"
ARGS="$ARGS --multi-sampling"
ARGS="$ARGS --seconds-per-day 60"
ARGS="$ARGS --auto-skip-seconds 1"  # skip inactivities after ...
ARGS="$ARGS --file-idle-time 0"     # expires files which haven't been touched by a user in ....
ARGS="$ARGS --stop-at-end"
ARGS="$ARGS --camera-mode track" # Follow active users
ARGS="$ARGS --key"                  # Show color-extension relation
ARGS="$ARGS --bloom-multiplier 2.0" # Radius size around commits
ARGS="$ARGS --bloom-intensity 1.0"  # Duration of the flash around commits
ARGS="$ARGS --highlight-users"
ARGS="$ARGS --hide mouse,progress,usernames"
ARGS="$ARGS --font-size 22"
ARGS="$ARGS --background-colour 222222"
ARGS="$ARGS --hash-seed 4815162342"
#ARGS="$ARGS --output-ppm-stream -"
ARGS="$ARGS --output-framerate 30"

gource $ARGS "$@"
