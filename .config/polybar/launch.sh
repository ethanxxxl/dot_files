#!/usr/bin/env bash

#Terminate alreay running bar instances
killall -q polybar

#wait until the processes have been shutdown
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

#launch bar1 and bar2
polybar -c ~/.config/polybar/config.ini mybar &
