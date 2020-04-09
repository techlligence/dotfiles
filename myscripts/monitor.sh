#!/bin/bash

focused_monitor=$(bspc query -M -m focused --names)

if [ "${focused_monitor}" == "DVI-D-0" ]; then
    echo "Secondary"
fi

if [ "${focused_monitor}" == "DP-3" ]; then
    echo "Primary"
fi

