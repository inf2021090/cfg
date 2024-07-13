#!/bin/bash
# Set the padding icon
padding=" "
# Count the number of icons in the system tray
icons=$(xprop -root _NET_SYSTEM_TRAY_S0 | cut -d "#" -f 2 | tr -d " ")
# Calculate the padding based on the number of icons
if [ "$icons" -gt 0 ]; then
    padding=$(printf "%*s" $icons)
fi
# Print the padding
echo "$padding"
