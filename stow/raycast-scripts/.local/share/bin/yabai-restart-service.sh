#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Restart Yabai Service
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 🔄
# @raycast.description Restart the yabai window manager service
# @raycast.packageName Yabai

# Documentation:
# @raycast.author oscarvarto
# @raycast.authorURL https://github.com/oscarvarto

launchctl kickstart -k gui/$(id -u)/org.nixos.yabai
echo "Yabai service restarted"
