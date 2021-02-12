#!/usr/bin/env sh

index=0
monitors=$(xrandr | grep " connected " | awk '{ print$1 }')
monitor_count=$(xrandr | grep " connected " | awk '{ print$1 }' | wc -l)

for monitor in $monitors
do
	sed -e 's/set_monitor_sed/'$monitor'/g' -e 's/set_monitor_number_sed/'$index'/g' $HOME/.config/bspwm/scripts/config > $HOME/.config/bspwm/scripts/config_"$index"
	polybar -c $HOME/.config/bspwm/scripts/config_"$index" example &
	index=$((index+1))
done
