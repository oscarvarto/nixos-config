#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Start Yabai Service
# @raycast.mode compact

# Optional parameters:
# @raycast.icon ▶️
# @raycast.description Start the yabai window manager service
# @raycast.packageName Yabai

# Documentation:
# @raycast.author oscarvarto
# @raycast.authorURL https://github.com/oscarvarto

launchctl start gui/$(id -u)/org.nixos.yabai
echo "Yabai service started"
