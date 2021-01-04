#!/usr/bin/bash

direction=$1
monitor_in_direction=$(bspc query -M -m $direction --names)

desktop_in_direction=$(bspc query -D focused -d "$monitor_in_direction":focused --names)

bspc desktop -f "$desktop_in_direction.local" || bspc desktop -s "$desktop_in_direction"
