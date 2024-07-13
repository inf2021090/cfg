#!/bin/bash

# Function to check if a package is installed
package_installed() {
    dpkg -s "$1" >/dev/null 2>&1
}

# List of packages to install
packages=(
    "xmonad"
    "libghc-xmonad-contrib-dev"
    "alacritty"
    "brave-browser"
    "codium"
    "feh"
    "picom"
    "network-manager-gnome"
    "volumeicon-alsa"
    "blueman"
    "flameshot"
    "greenclip"
    "betterlockscreen"
    "conky"
    "trayer"
    "xmobar"
    "playerctl"
    "brightnessctl"
    "rofi"
    "libghc-x11-dev"
    "libghc-x11-xft-dev"
    "libghc-dbus-dev"
    # Add any other Haskell libraries you need
)

# Loop through each package and install if not already installed
for package in "${packages[@]}"; do
    if package_installed "$package"; then
        echo "$package is already installed. Skipping."
    else
        sudo apt-get install -y "$package"
    fi
done

# Additional configuration steps if needed
# E.g., downloading specific configuration files, setting up directories, etc.

echo "Installation completed."

