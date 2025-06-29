#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Move Window to Space 10
# @raycast.mode compact

# Optional parameters:
# @raycast.icon ➡️🔟
# @raycast.description Move current window to space 10 and follow
# @raycast.packageName Yabai

# Documentation:
# @raycast.author oscarvarto
# @raycast.authorURL https://github.com/oscarvarto

/run/current-system/sw/bin/yabai -m window --space 10
/run/current-system/sw/bin/yabai -m space --focus 10
