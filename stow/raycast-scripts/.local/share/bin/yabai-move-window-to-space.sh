#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Move Window to Space
# @raycast.mode compact

# Optional parameters:
# @raycast.icon ➡️🔢
# @raycast.description Move current window to space and follow (1-10)
# @raycast.packageName Yabai
# @raycast.argument1 { "type": "text", "placeholder": "Space number (1-10)", "percentEncoded": false }

# Documentation:
# @raycast.author oscarvarto
# @raycast.authorURL https://github.com/oscarvarto

if [[ -z "$1" ]]; then
  echo "Please provide a space number (1-10)"
  exit 1
fi

space_number="$1"

# Validate input
if ! [[ "$space_number" =~ ^[1-9]$|^10$ ]]; then
  echo "Invalid space number. Please use 1-10"
  exit 1
fi

yabai -m window --space "$space_number"; yabai -m space --focus "$space_number"
