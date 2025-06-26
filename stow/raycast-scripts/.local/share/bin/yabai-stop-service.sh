#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Stop Yabai Service
# @raycast.mode compact

# Optional parameters:
# @raycast.icon ⏹️
# @raycast.description Stop the yabai window manager service
# @raycast.packageName Yabai

# Documentation:
# @raycast.author oscarvarto
# @raycast.authorURL https://github.com/oscarvarto

launchctl stop gui/$(id -u)/org.nixos.yabai
echo "Yabai service stopped"
