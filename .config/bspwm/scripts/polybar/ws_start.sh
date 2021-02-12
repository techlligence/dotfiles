#!/bin/bash


trap 'trap - TERM; kill 0' INT TERM QUIT EXIT

PANEL_FIFO="/tmp/panel_fifo_polybar_"$1""
[ -e "$PANEL_FIFO" ] && rm "$PANEL_FIFO"
mkfifo "$PANEL_FIFO"

bspc subscribe report > "$PANEL_FIFO" &

$HOME/polybar_project/ws_format "$2" < "$PANEL_FIFO" &

wait
