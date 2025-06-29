#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Move Window to Space 5
# @raycast.mode compact

# Optional parameters:
# @raycast.icon ➡️5️⃣
# @raycast.description Move current window to space 5 and follow
# @raycast.packageName Yabai

# Documentation:
# @raycast.author oscarvarto
# @raycast.authorURL https://github.com/oscarvarto

/run/current-system/sw/bin/yabai -m window --space 5
/run/current-system/sw/bin/yabai -m space --focus 5
