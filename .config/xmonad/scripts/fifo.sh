#!/bin/bash

PANEL_FIFO="/tmp/tester"
[ -e "$PANEL_FIFO" ] && rm "$PANEL_FIFO"
mkfifo "$PANEL_FIFO"

while read -r line; do
  echo "$line" > "$PANEL_FIFO"
done
