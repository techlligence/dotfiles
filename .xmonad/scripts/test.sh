#!/bin/bash

bspc subscribe node_add | while read line
do
	bspc node $(echo "$line" | awk '{print $5}') -F horizontal 
done
