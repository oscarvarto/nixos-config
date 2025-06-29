#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Move Window to Space 9
# @raycast.mode compact

# Optional parameters:
# @raycast.icon ➡️9️⃣
# @raycast.description Move current window to space 9 and follow
# @raycast.packageName Yabai

# Documentation:
# @raycast.author oscarvarto
# @raycast.authorURL https://github.com/oscarvarto

/run/current-system/sw/bin/yabai -m window --space 9
/run/current-system/sw/bin/yabai -m space --focus 9
