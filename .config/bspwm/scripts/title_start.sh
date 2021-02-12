#!/bin/bash

trap 'trap - TERM; kill 0' INT TERM QUIT EXIT

TITLE_FIFO="/tmp/title_fifo_polybar_"$1""
[ -e "$TITLE_FIFO" ] && rm "$TITLE_FIFO"
mkfifo "$TITLE_FIFO"

xtitle -sf 'title%s\n' -t 50 > "$TITLE_FIFO" &

$HOME/.config/bspwm/scripts/title_format "$2" < "$TITLE_FIFO" &

wait
