#!/usr/bin/env sh

index=0
monitors=$(xrandr | grep " connected " | awk '{ print$1 }')
monitor_count=$(xrandr | grep " connected " | awk '{ print$1 }' | wc -l)

for monitor in $monitors
do
	index=$((index+1))
	panel $index $monitor &
done
