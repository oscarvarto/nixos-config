#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Toggle Native Fullscreen
# @raycast.mode compact

# Optional parameters:
# @raycast.icon ⛶
# @raycast.description Toggle native fullscreen for current window
# @raycast.packageName Yabai

# Documentation:
# @raycast.author oscarvarto
# @raycast.authorURL https://github.com/oscarvarto

yabai -m window --toggle native-fullscreen
