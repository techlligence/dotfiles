#!/bin/bash

ppDesktop(){
	IFS=':' read -ra splitArray <<< "$1"

	local wn
	local workspaceNumber
	local -a desktops
	local hasFocus
	local monitor
	local monitor_bar=$2 
	
	hasFocus=0

	for item in "${splitArray[@]}"
	do
		#echo $item
		case $item in
			[M]* )
				hasFocus=1
				monitor="${item:1}"
				echo $monitor
				;;
			[m]* )
				hasFocus=0
				monitor="${item:1}"
				echo $monitor
				;;

			[FO]* )
				if [ $monitor = $monitor_bar ]
				then
					workspaceNumber=${item:1}
					wn="[${item:1}]"
					desktops[$workspaceNumber]=$wn
				else
					workspaceNumber=${item:1}
					wn="'${item:1}'"
					desktops[$workspaceNumber]=$wn
				fi
				#echo $wn
				;;
			[fo]* )
				workspaceNumber=${item:1}
				wn="*${item:1}*"
				desktops[$workspaceNumber]=$wn
				#echo $wn
				;;

			* )
				continue
		esac
	done

	echo "wp"${desktops[@]}""
}

getDesktops(){
	getWMState=$(bspc wm -g)
	ppDesktops ${getWMState:1} $monitor_name
}

getDesktops "DP-1"
