#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Move Window to Space 4
# @raycast.mode compact

# Optional parameters:
# @raycast.icon ➡️4️⃣
# @raycast.description Move current window to space 4 and follow
# @raycast.packageName Yabai

# Documentation:
# @raycast.author oscarvarto
# @raycast.authorURL https://github.com/oscarvarto

/run/current-system/sw/bin/yabai -m window --space 4
/run/current-system/sw/bin/yabai -m space --focus 4
