#!/bin/bash

rofi_command="rofi -columns 5 -lines 1 -theme themes/powermenu.rasi"

# Each of the icon is a selectable element
options=$'\n\n'

chosen="$(echo "$options" | $rofi_command -font "Font Awesome 5 Pro Solid 50" -dmenu)"
case $chosen in
    ) # Shutdown the computer
        systemctl poweroff
        ;;
    ) # Reboot the computer
        systemctl reboot
        ;;
    ) # Log out of the current session
		bspc quit
        ;;
esac

