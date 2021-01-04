#!/usr/bin/bash

direction=$1

focused_desktops=()

index=0
monitors=$(xrandr | grep " connected " | awk '{ print$1 }')
monitor_count=$(xrandr | grep " connected " | awk '{ print$1 }' | wc -l)
for monitor in $monitors
do
	#echo $monitor
	focused_desktops[$index]=$(bspc query -D focused -d "$monitor":focused --names)
	#echo $(bspc query -D focused -d "$monitor":focused --names)

	index=$((index+1))
done



focused_desktop=$(bspc query -D -d focused --names)

if [ $direction = "forward" ]
then
	for ((tag=focused_desktop;tag<=9;tag++))
	do
		next_tag=$(( tag + 1 ))

		tag_in_focused=0
		for i in ${focused_desktops[@]}
		do
			if [ $i -eq $next_tag ] 
			then
				tag_in_focused=1
			fi
		done
	
		if [ $tag_in_focused -eq 1 ]
		then
			continue
		else
			bspc desktop -f "$next_tag.local" || bspc desktop -s "$next_tag"
			break
		fi
	
	done
fi

if [ $direction = "backward" ]
then
	for ((tag=focused_desktop;tag>=1;tag--))
	do
		next_tag=$(( tag - 1 ))

		tag_in_focused=0
		for i in ${focused_desktops[@]}
		do
			if [ $i -eq $next_tag ] 
			then
				tag_in_focused=1
			fi
		done
	
		if [ $tag_in_focused -eq 1 ]
		then
			continue
		else
			bspc desktop -f "$next_tag.local" || bspc desktop -s "$next_tag"
			break
		fi
	
	done
fi
