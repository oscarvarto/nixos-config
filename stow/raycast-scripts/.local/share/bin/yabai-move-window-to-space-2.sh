#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Move Window to Space 2
# @raycast.mode compact

# Optional parameters:
# @raycast.icon ➡️2️⃣
# @raycast.description Move current window to space 2 and follow
# @raycast.packageName Yabai

# Documentation:
# @raycast.author oscarvarto
# @raycast.authorURL https://github.com/oscarvarto

/run/current-system/sw/bin/yabai -m window --space 2
/run/current-system/sw/bin/yabai -m space --focus 2
