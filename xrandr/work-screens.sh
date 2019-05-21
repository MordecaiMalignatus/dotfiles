#!/bin/sh
xrandr --output VIRTUAL1 --off
xrandr --output eDP1 --off
xrandr --output DP1 --off
xrandr --output DP2-2 --mode 2560x1440 --pos 0x0 --primary
xrandr --output DP1 --off
xrandr --output DP2-3 --off
xrandr --output HDMI2 --off
xrandr --output HDMI1 --off
xrandr --output DP2 --off

# sh i3-arrangement.sh
