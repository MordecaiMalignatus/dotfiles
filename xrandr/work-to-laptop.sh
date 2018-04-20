#!/usr/bin/env bash
set -e 

xrandr --output DP-2-1 --off \
       --output DP-2-2 --off
sleep 1s
xrandr --out eDP-1 --primary --panning 0x0
i3-msg restart
