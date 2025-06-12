{ user, config, pkgs, ... }:

let
  xdg_configHome = "${config.users.users.${user}.home}/.config";
  xdg_dataHome   = "${config.users.users.${user}.home}/.local/share";
  xdg_stateHome  = "${config.users.users.${user}.home}/.local/state";
  hammerspoonDir = "${config.users.users.${user}.home}/.hammerspoon";
in
{
  # yabai toggle split
  # Toggles between vertical and horizontal split layouts
  "${xdg_dataHome}/bin/yabai_toggle_split.sh" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      #
      # Required parameters:
      # @raycast.schemaVersion 1
      # @raycast.title Yabai Toggle Split
      # @raycast.mode silent

      /run/current-system/sw/bin/yabai -m window --toggle split
    '';
  };

  # yabai toggle float
  # Toggle float
  "${xdg_dataHome}/bin/yabai_toggle_float.sh" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      #
      # Required parameters:
      # @raycast.schemaVersion 1
      # @raycast.title Yabai Toggle Float
      # @raycast.mode silent

      /run/current-system/sw/bin/yabai -m window --toggle float
    '';
  };

  # Delete clipboard
  "${xdg_dataHome}/bin/delete_clipboard.sh" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      #
      # Required parameters:
      # @raycast.schemaVersion 1
      # @raycast.title Delete macOS clipboard
      # @raycast.mode silent

      pbcopy </dev/null
   '';
  };

  # Emacs everywhere
  "${xdg_dataHome}/bin/emacs_everywhere.sh" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      #
      # Required parameters:
      # @raycast.schemaVersion 1
      # @raycast.title Emacs Everywhere
      # @raycast.mode silent

      /Users/${user}/.emacs.d/bin/doom +everywhere; \
      sleep 2;
      /run/current-system/sw/bin/yabai -m window --focus west; \
      /run/current-system/sw/bin/yabai -m window --toggle float; \
      /run/current-system/sw/bin/yabai -m window --grid 4:4:1:1:2:2
    '';
  };

  # Emacs daemon Raycast script so that "Run Emacs" is available and uses Emacs daemon
  "${xdg_dataHome}/bin/emacsclient" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      #
      # Required parameters:
      # @raycast.schemaVersion 1
      # @raycast.title Run Emacs
      # @raycast.mode silent
      #
      # Optional parameters:
      # @raycast.packageName Emacs
      # @raycast.icon ${xdg_dataHome}/img/icons/Emacs.icns
      # @raycast.iconDark ${xdg_dataHome}/img/icons/Emacs.icns

      /opt/homebrew/bin/emacsclient -nc -s /var/folders/yh/5_g54kd572gd9vr8tbc4m6gh0000gn/T/emacs501/doom "$@"
    '';
  };

  # Create a template config file with just the biometric settings
  "${xdg_configHome}/op/biometric-config.json" = {
    text = ''
      {
        "app_start": {
          "biometric_unlock": true,
          "biometric_unlock_timeout": 86400
        },
        "account": {
          "biometric_unlock": true
        }
      }
    '';
 
    # Use onChange to merge the configurations
    onChange = ''
      if [ -f "$HOME/.config/op/config" ]; then
        # Create a temporary file for the merged config
        TEMP_FILE=$(mktemp)
 
        # Use jq to merge the existing config with the biometric settings
        jq -s '.[0] * .[1]' "$HOME/.config/op/config" "$HOME/.config/op/biometric-config.json" > "$TEMP_FILE"
 
        # Replace the config file with the merged version
        mv "$TEMP_FILE" "$HOME/.config/op/config"
      else
        # If no config exists yet, just copy the biometric config
        cp "$HOME/.config/op/biometric-config.json" "$HOME/.config/op/config"
      fi
    '';
  };

  # NordVPN auto-connect script - ONE SHOT WITH RETRY LOGIC
  "${config.users.users.${user}.home}/nordvpn-autoconnect.sh" = {
    executable = true;
    text = ''
      #!/bin/bash

      # NordVPN Auto Connect Script - ONE SHOT EXECUTION
      # This script connects to NordVPN using a dedicated IP retrieved from 1Password
      # It includes proper error handling, retry logic, and ensures network connectivity

      set -euo pipefail

      # Configuration
      SKIP_AUTOCONNECT="''${SKIP_VPN_AUTOCONNECT:-false}"
      GLOBALPROTECT_ACTIVE="''${GLOBALPROTECT_ACTIVE:-false}"
      LOG_FILE="$HOME/.nordvpn-autoconnect.log"
      MAX_RETRIES=3
      RETRY_DELAY=10
      OP_ITEM_NAME="NordVPN Dedicated IP"
      OP_VAULT="Personal"
      INTERNET_CHECK_TIMEOUT=5
      ONEPASSWORD_WAIT_TIMEOUT=30
      NORDVPN_APP="/Applications/NordVPN.app"

      # Will be set after 1Password authentication
      DEDICATED_IP=""

      # Logging function
      log_message() {
          echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
      }

      # Wait until internet connectivity is established
      wait_for_internet() {
          log_message "Waiting for internet connectivity..."
          local attempt=0
          while ! ping -c 1 -W $INTERNET_CHECK_TIMEOUT google.com >/dev/null 2>&1; do
              if (( attempt >= MAX_RETRIES )); then
                  log_message "ERROR: No internet connection after $MAX_RETRIES attempts"
                  exit 1
              fi
              log_message "No internet connection (attempt $((attempt + 1))/$MAX_RETRIES). Retrying..."
              ((attempt++))
              sleep $RETRY_DELAY
          done
          log_message "Internet connection established"
      }

      # Wait until 1Password is available
      wait_for_1password() {
          log_message "Waiting for 1Password CLI availability..."
          local attempt=0
          while ! op account list >/dev/null 2>&1; do
              if (( attempt >= MAX_RETRIES )); then
                  log_message "ERROR: 1Password CLI not available after $MAX_RETRIES attempts"
                  exit 1
              fi
              log_message "1Password CLI not available (attempt $((attempt + 1))/$MAX_RETRIES). Retrying..."
              ((attempt++))
              sleep $ONEPASSWORD_WAIT_TIMEOUT
          done
          log_message "1Password CLI is available"
      }

      # Retrieve dedicated IP from 1Password
      get_dedicated_ip_from_1password() {
          log_message "Retrieving dedicated IP from 1Password..."
          
          local ip
          if ip=$(op item get "$OP_ITEM_NAME" --vault="$OP_VAULT" --field password --reveal 2>/dev/null); then
              if [[ -n "$ip" && "$ip" =~ ^[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}$ ]]; then
                  DEDICATED_IP="$ip"
                  log_message "Successfully retrieved dedicated IP from 1Password"
                  return 0
              else
                  log_message "ERROR: Retrieved value is not a valid IP address: $ip"
                  return 1
              fi
          else
              log_message "ERROR: Failed to retrieve dedicated IP from 1Password item '$OP_ITEM_NAME' in vault '$OP_VAULT'"
              return 1
          fi
      }

      # Check if we should skip VPN connection
      should_skip_vpn() {
          # Check if a "no VPN" flag file exists
          NO_VPN_FLAG="$HOME/.config/no_vpn"
          if [ -f "$NO_VPN_FLAG" ]; then
              log_message "No VPN mode is enabled. Skipping NordVPN auto-connect."
              return 0
          fi

          # Skip if explicitly disabled
          if [[ "$SKIP_AUTOCONNECT" == "true" ]]; then
              log_message "VPN autoconnect disabled via SKIP_VPN_AUTOCONNECT"
              return 0
          fi
          
          # Skip if GlobalProtect is explicitly marked as active
          if [[ "$GLOBALPROTECT_ACTIVE" == "true" ]]; then
              log_message "GlobalProtect is marked as active, skipping NordVPN connection"
              return 0
          fi
          
          # Check if other VPNs are active - specifically GlobalProtect
          if pgrep -q "GlobalProtect"; then
              log_message "GlobalProtect appears to be running. Skipping NordVPN auto-connect."
              return 0
          fi
          
          return 1
      }

      # Check if NordVPN is installed
      check_nordvpn_installation() {
          if [ ! -d "$NORDVPN_APP" ]; then
              log_message "ERROR: NordVPN is not installed at $NORDVPN_APP"
              exit 1
          fi
          log_message "NordVPN application found at $NORDVPN_APP"
      }

      # Function to check if NordVPN is connected (without bringing window to front)
      check_connection() {
          CONNECTION_STATUS=$(osascript -e 'tell application "System Events"' \
                                       -e 'tell application process "NordVPN"' \
                                       -e 'if exists (button "Disconnect" of window 1) then' \
                                       -e 'return "connected"' \
                                       -e 'else' \
                                       -e 'return "disconnected"' \
                                       -e 'end if' \
                                       -e 'end tell' \
                                       -e 'end tell' 2>/dev/null || echo "error")
          echo "$CONNECTION_STATUS"
      }

      # Connect to VPN using AppleScript without showing the UI
      connect_to_vpn() {
          log_message "Attempting to connect to NordVPN with dedicated IP: $DEDICATED_IP"
          
          # Start NordVPN in background (already running apps won't show window)
          open -g "$NORDVPN_APP"
          sleep 3
          
          # Perform connection operations without bringing window to front
          osascript -e 'tell application "System Events"' \
                    -e 'tell application process "NordVPN"' \
                    -e 'set visible to false' \
                    -e 'keystroke "c" using {command down}' \
                    -e 'delay 1' \
                    -e 'keystroke "'"$DEDICATED_IP"'"' \
                    -e 'delay 1' \
                    -e 'key code 36' \
                    -e 'delay 1' \
                    -e 'set visible to false' \
                    -e 'end tell' \
                    -e 'end tell' > /dev/null 2>&1
          
          # Ensure window stays hidden after connection attempt
          sleep 2
          osascript -e 'tell application "System Events" to tell application process "NordVPN" to set visible to false' > /dev/null 2>&1
          
          log_message "Connection attempt completed"
      }

      # Main execution with retry logic
      main() {
          log_message "Starting NordVPN auto-connect process - ONE SHOT EXECUTION"
          
          # Check if we should skip VPN connection first
          if should_skip_vpn; then
              log_message "Skipping VPN connection due to configuration or conflicts"
              exit 0
          fi
          
          # Check prerequisites
          check_nordvpn_installation
          
          # Wait for network and 1Password availability
          wait_for_internet
          wait_for_1password
          
          # Attempt to connect with retry logic
          local attempt=0
          while (( attempt < MAX_RETRIES )); do
              log_message "Connection attempt $((attempt + 1))/$MAX_RETRIES"
              
              # Retrieve dedicated IP from 1Password
              if get_dedicated_ip_from_1password; then
                  log_message "Successfully retrieved dedicated IP: $DEDICATED_IP"
                  
                  # Check current connection status
                  CONNECTION_STATUS=$(check_connection)
                  
                  if [ "$CONNECTION_STATUS" = "connected" ]; then
                      log_message "NordVPN is already connected"
                      exit 0
                  else
                      log_message "NordVPN is not connected. Attempting connection..."
                      connect_to_vpn
                      sleep 5
                      
                      # Verify connection
                      AFTER_CONNECTION=$(check_connection)
                      if [ "$AFTER_CONNECTION" = "connected" ]; then
                          log_message "Successfully connected to NordVPN"
                          exit 0
                      else
                          log_message "Connection attempt $((attempt + 1)) failed"
                      fi
                  fi
              else
                  log_message "Failed to retrieve dedicated IP from 1Password on attempt $((attempt + 1))"
              fi
              
              ((attempt++))
              if (( attempt < MAX_RETRIES )); then
                  log_message "Waiting $RETRY_DELAY seconds before retry..."
                  sleep $RETRY_DELAY
              fi
          done
          
          log_message "ERROR: Failed to connect to NordVPN after $MAX_RETRIES attempts"
          exit 1
      }

      # Run main function
      main
    '';
  };

  # NordVPN show window script (for manual interaction)
  "${config.users.users.${user}.home}/nordvpn-show.sh" = {
    executable = true;
    text = ''
      #!/bin/bash

      # NordVPN Show Window Script
      # This script brings the NordVPN window to the front for manual interaction

      # Path to the NordVPN application
      NORDVPN_APP="/Applications/NordVPN.app"

      # Check if NordVPN is installed
      if [ ! -d "$NORDVPN_APP" ]; then
          echo "NordVPN is not installed at $NORDVPN_APP"
          exit 1
      fi

      # Start NordVPN if not running
      if ! pgrep -q "NordVPN"; then
          echo "Starting NordVPN..."
          open "$NORDVPN_APP"
          sleep 2
      fi

      # Bring NordVPN window to front for manual interaction
      osascript -e 'tell application "NordVPN" to activate' \
                -e 'tell application "System Events"' \
                -e 'tell application process "NordVPN"' \
                -e 'set frontmost to true' \
                -e 'set visible to true' \
                -e 'end tell' \
                -e 'end tell' > /dev/null 2>&1

      echo "NordVPN window is now visible for manual interaction"
    '';
  };

  # NordVPN Mexico P2P fix script
  "${config.users.users.${user}.home}/nordvpn-mexico-fix.sh" = {
    executable = true;
    text = ''
      #!/bin/bash

      # NordVPN Mexico P2P Server Fix
      # This script provides a workaround for connectivity issues with Mexico P2P servers

      # Check if a "no VPN" flag file exists
      NO_VPN_FLAG="$HOME/.config/no_vpn"
      if [ -f "$NO_VPN_FLAG" ]; then
          echo "No VPN mode is enabled. Skipping NordVPN Mexico P2P check."
          exit 0
      fi

      # Check if GlobalProtect is running
      if pgrep -q "GlobalProtect"; then
          echo "GlobalProtect appears to be running. Skipping NordVPN Mexico P2P check."
          exit 0
      fi

      # Check if NordVPN is running
      if ! pgrep -q "NordVPN"; then
          echo "NordVPN is not running. Skipping Mexico P2P check."
          exit 0
      fi

      # Function to check if NordVPN is connected to Mexico server (without bringing window to front)
      check_mexico_connection() {
          SERVER_INFO=$(osascript -e 'tell application "System Events"' \
                                 -e 'tell application process "NordVPN"' \
                                 -e 'if exists (static text of window 1 whose name contains "Mexico") then' \
                                 -e 'return "mexico"' \
                                 -e 'else' \
                                 -e 'return "other"' \
                                 -e 'end if' \
                                 -e 'end tell' \
                                 -e 'end tell' 2>/dev/null || echo "error")
          echo "$SERVER_INFO"
      }

      # Function to check if the connection is P2P (without bringing window to front)
      check_if_p2p() {
          P2P_INFO=$(osascript -e 'tell application "System Events"' \
                             -e 'tell application process "NordVPN"' \
                             -e 'if exists (static text of window 1 whose name contains "P2P") then' \
                             -e 'return "p2p"' \
                             -e 'else' \
                             -e 'return "not_p2p"' \
                             -e 'end if' \
                             -e 'end tell' \
                             -e 'end tell' 2>/dev/null || echo "error")
          echo "$P2P_INFO"
      }

      # Function to disconnect from VPN if connected to Mexico P2P (briefly bring to front for button click)
      disconnect_from_vpn() {
          osascript -e 'tell application "System Events"' \
                    -e 'tell application process "NordVPN"' \
                    -e 'click button "Disconnect" of window 1' \
                    -e 'delay 1' \
                    -e 'set visible to false' \
                    -e 'end tell' \
                    -e 'end tell' > /dev/null 2>&1
          echo "Disconnected from problematic Mexico P2P server"
      }

      # Array of stable Mexico P2P servers (ordered by reliability)
      MEXICO_P2P_SERVERS=(
          "Mexico #15"
          "Mexico #12"
          "Mexico #8"
          "Mexico #5"
          "Mexico #18"
      )
      
      # Function to test server connectivity
      test_server_connectivity() {
          local server="$1"
          local timeout=10
          
          echo "Testing connectivity to $server..."
          
          # Connect to the server
          osascript -e 'tell application "System Events"' \
                    -e 'tell application process "NordVPN"' \
                    -e 'set visible to false' \
                    -e 'keystroke "c" using {command down}' \
                    -e 'delay 2' \
                    -e 'keystroke "'"$server"'"' \
                    -e 'delay 2' \
                    -e 'key code 36' \
                    -e 'delay 5' \
                    -e 'set visible to false' \
                    -e 'end tell' \
                    -e 'end tell' > /dev/null 2>&1
          
          # Wait for connection to establish
          sleep 8
          
          # Check if connected successfully
          local connection_status=$(check_connection_status)
          if [ "$connection_status" = "connected" ]; then
              local p2p_status=$(check_if_p2p)
              if [ "$p2p_status" = "p2p" ]; then
                  echo "✅ $server: Connected with P2P support"
                  return 0
              else
                  echo "⚠️  $server: Connected but no P2P support"
                  return 1
              fi
          else
              echo "❌ $server: Connection failed"
              return 1
          fi
      }
      
      # Function to check connection status
      check_connection_status() {
          STATUS=$(osascript -e 'tell application "System Events"' \
                           -e 'tell application process "NordVPN"' \
                           -e 'if exists (static text of window 1 whose name contains "Connected") then' \
                           -e 'return "connected"' \
                           -e 'else if exists (static text of window 1 whose name contains "Connecting") then' \
                           -e 'return "connecting"' \
                           -e 'else' \
                           -e 'return "disconnected"' \
                           -e 'end if' \
                           -e 'end tell' \
                           -e 'end tell' 2>/dev/null || echo "error")
          echo "$STATUS"
      }
      
      # Function to connect to a stable Mexico P2P server
      connect_to_stable_mexico_p2p() {
          echo "Searching for stable Mexico P2P server..."
          
          for server in "''${MEXICO_P2P_SERVERS[@]}"; do
              echo "Attempting to connect to $server"
              
              if test_server_connectivity "$server"; then
                  echo "✅ Successfully connected to stable Mexico P2P server: $server"
                  
                  # Log the successful server for future preference
                  echo "$(date): Successfully connected to $server" >> "$HOME/.nordvpn-mexico-p2p-success.log"
                  return 0
              else
                  echo "Failed to connect to $server, trying next..."
                  # Disconnect before trying next server
                  disconnect_from_vpn
                  sleep 3
              fi
          done
          
          echo "❌ Failed to connect to any stable Mexico P2P server"
          echo "Falling back to US server for reliability"
          connect_to_fallback_server
          return 1
      }
      
      # Function to connect to fallback server (US P2P)
      connect_to_fallback_server() {
          FALLBACK_SERVER="United States"
          osascript -e 'tell application "System Events"' \
                    -e 'tell application process "NordVPN"' \
                    -e 'set visible to false' \
                    -e 'keystroke "c" using {command down}' \
                    -e 'delay 2' \
                    -e 'keystroke "'"$FALLBACK_SERVER"'"' \
                    -e 'delay 2' \
                    -e 'key code 36' \
                    -e 'delay 5' \
                    -e 'set visible to false' \
                    -e 'end tell' \
                    -e 'end tell' > /dev/null 2>&1
          echo "Connected to fallback server: $FALLBACK_SERVER"
      }

      # Main execution
      echo "Checking for problematic Mexico P2P connection..."

      # Check for Mexico P2P connection
      SERVER_TYPE=$(check_mexico_connection)
      P2P_TYPE=$(check_if_p2p)

      if [ "$SERVER_TYPE" = "mexico" ] && [ "$P2P_TYPE" = "p2p" ]; then
          echo "Detected Mexico P2P connection. Checking stability..." 
          
          # Test current connection stability
          echo "Testing current Mexico P2P connection stability..."
          sleep 5
          
          # Re-check connection after brief wait
          current_status=$(check_connection_status)
          if [ "$current_status" != "connected" ]; then
              echo "Current Mexico P2P connection is unstable. Finding alternative..."
              disconnect_from_vpn
              sleep 3
              connect_to_stable_mexico_p2p
          else
              echo "Current Mexico P2P connection appears stable. Monitoring..."
              # Log successful connection for monitoring
              echo "$(date): Current Mexico P2P connection stable" >> "$HOME/.nordvpn-mexico-p2p-monitor.log"
          fi
      elif [ "$SERVER_TYPE" = "mexico" ] && [ "$P2P_TYPE" != "p2p" ]; then
          echo "Connected to Mexico server without P2P. Switching to Mexico P2P server..."
          disconnect_from_vpn
          sleep 3
          connect_to_stable_mexico_p2p
      else
          echo "No Mexico P2P connection detected. Current setup: $SERVER_TYPE, $P2P_TYPE"
      fi

      # Ensure window stays hidden
      osascript -e 'tell application "System Events" to tell application process "NordVPN" to set visible to false' > /dev/null 2>&1

      exit 0
    '';
  };

  # NordVPN toggle script
  "${config.users.users.${user}.home}/nordvpn-toggle.sh" = {
    executable = true;
    text = ''
      #!/bin/bash

      # NordVPN Automation Toggle Script
      NO_VPN_FLAG="$HOME/.config/no_vpn"

      if [ "$1" == "enable" ]; then
          echo "Enabling NordVPN auto-connection..."
          rm -f "$NO_VPN_FLAG"
          launchctl load ~/Library/LaunchAgents/com.oscarvarto.nordvpn-autoconnect.plist 2>/dev/null
          launchctl load ~/Library/LaunchAgents/com.oscarvarto.nordvpn-mexico-fix.plist 2>/dev/null
          echo "NordVPN automation is now ENABLED"
          exit 0
      elif [ "$1" == "disable" ]; then
          echo "Disabling NordVPN auto-connection..."
          launchctl unload ~/Library/LaunchAgents/com.oscarvarto.nordvpn-autoconnect.plist 2>/dev/null
          launchctl unload ~/Library/LaunchAgents/com.oscarvarto.nordvpn-mexico-fix.plist 2>/dev/null
          echo "NordVPN automation is now DISABLED"
          exit 0
      elif [ "$1" == "no-vpn" ]; then
          echo "Enabling NO VPN mode..."
          mkdir -p "$(dirname "$NO_VPN_FLAG")"
          touch "$NO_VPN_FLAG"
          if pgrep -q "NordVPN"; then
              echo "Disconnecting from NordVPN..."
              osascript -e 'tell application "System Events"' \
                        -e 'tell application process "NordVPN"' \
                        -e 'set frontmost to true' \
                        -e 'if exists (button "Disconnect" of window 1) then' \
                        -e 'click button "Disconnect" of window 1' \
                        -e 'end if' \
                        -e 'end tell' \
                        -e 'end tell' > /dev/null 2>&1
              sleep 3
          fi
          echo "NO VPN mode is now ENABLED"
          exit 0
      elif [ "$1" == "status" ]; then
          if [ -f "$NO_VPN_FLAG" ]; then
              echo "NO VPN mode is ENABLED - NordVPN automation is inactive"
          elif launchctl list | grep -q com.oscarvarto.nordvpn-autoconnect; then
              echo "NordVPN automation is ENABLED"
          else
              echo "NordVPN automation is DISABLED"
          fi
          exit 0
      else
          echo "Usage: $0 [enable|disable|no-vpn|status]"
          echo "  enable: Enable NordVPN auto-connection"
          echo "  disable: Disable NordVPN auto-connection"
          echo "  no-vpn: Enable NO VPN mode (prevents all auto-connections)"
          echo "  status: Check if NordVPN auto-connection is enabled"
          exit 1
      fi
    '';
  };

  # NordVPN auto-connect launch agent
  "${config.users.users.${user}.home}/Library/LaunchAgents/com.oscarvarto.nordvpn-autoconnect.plist" = {
    text = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
      <plist version="1.0">
      <dict>
          <key>Label</key>
          <string>com.oscarvarto.nordvpn-autoconnect</string>
          <key>ProgramArguments</key>
          <array>
              <string>${config.users.users.${user}.home}/nordvpn-autoconnect.sh</string>
          </array>
          <key>RunAtLoad</key>
          <true/>
          <!-- ONE SHOT EXECUTION - No StartInterval -->
          <key>StandardErrorPath</key>
          <string>${config.users.users.${user}.home}/Library/Logs/nordvpn-autoconnect.log</string>
          <key>StandardOutPath</key>
          <string>${config.users.users.${user}.home}/Library/Logs/nordvpn-autoconnect.log</string>
      </dict>
      </plist>
    '';
  };

  # NordVPN Mexico P2P fix launch agent
  "${config.users.users.${user}.home}/Library/LaunchAgents/com.oscarvarto.nordvpn-mexico-fix.plist" = {
    text = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
      <plist version="1.0">
      <dict>
          <key>Label</key>
          <string>com.oscarvarto.nordvpn-mexico-fix</string>
          <key>ProgramArguments</key>
          <array>
              <string>${config.users.users.${user}.home}/nordvpn-mexico-fix.sh</string>
          </array>
          <key>RunAtLoad</key>
          <true/>
          <key>StartInterval</key>
          <integer>300</integer>
          <key>StandardErrorPath</key>
          <string>${config.users.users.${user}.home}/Library/Logs/nordvpn-mexico-fix.log</string>
          <key>StandardOutPath</key>
          <string>${config.users.users.${user}.home}/Library/Logs/nordvpn-mexico-fix.log</string>
      </dict>
      </plist>
    '';
  };

  # Hammerspoon configuration
  "${hammerspoonDir}/init.lua" = {
    text = ''
      -- Configuration for window focus management
      -- When closing a window, auto-focus the most recently used window on the same screen

      -- Log function for debugging
      local function log(msg)
        print(msg)
      end

      -- Keep track of window focus history per display
      local windowHistory = {}
      local MAX_HISTORY_PER_DISPLAY = 10

      -- Initialize history tracking for each screen
      local function initializeHistoryForDisplays()
        for _, screen in pairs(hs.screen.allScreens()) do
          local screenID = screen:id()
          if not windowHistory[screenID] then
            windowHistory[screenID] = {}
          end
        end
      end

      -- Add window to history for its display
      local function addToHistory(win)
        if not win then return end
        
        local screen = win:screen()
        if not screen then return end
        
        local screenID = screen:id()
        if not windowHistory[screenID] then
          windowHistory[screenID] = {}
        end
        
        -- Get window ID
        local winID = win:id()
        
        -- Remove this window if it already exists in history
        for i = #windowHistory[screenID], 1, -1 do
          if windowHistory[screenID][i] == winID then
            table.remove(windowHistory[screenID], i)
            break
          end
        end
        
        -- Add to front of history
        table.insert(windowHistory[screenID], 1, winID)
        
        -- Trim history if needed
        if #windowHistory[screenID] > MAX_HISTORY_PER_DISPLAY then
          table.remove(windowHistory[screenID])
        end
      end

      -- Focus the most recently used window on the same screen
      local function focusPreviousWindowOnSameScreen()
        local currentScreen = hs.screen.mainScreen()
        local screenID = currentScreen:id()
        
        if not windowHistory[screenID] then return end
        
        -- Try to find a valid window in history
        for i, winID in ipairs(windowHistory[screenID]) do
          local win = hs.window.get(winID)
          if win and win:isStandard() and not win:isMinimized() then
            -- Skip the window if it's the one being closed
            local currentWindow = hs.window.focusedWindow()
            if not currentWindow or currentWindow:id() ~= winID then
              -- Remove this window from history since we're focusing it
              table.remove(windowHistory[screenID], i)
              
              -- Focus the window
              win:focus()
              return true
            end
          end
        end
        
        -- If no suitable window found in history, fallback to any window on screen
        local windows = hs.window.filter.new():setScreens({currentScreen:id()}):getWindows()
        for _, win in ipairs(windows) do
          if win:isStandard() and not win:isMinimized() then
            win:focus()
            return true
          end
        end
        
        -- If no window found on this screen, then fall back to any window on any screen
        local anyWindow = hs.window.filter.new():getWindows()[1]
        if anyWindow then
          anyWindow:focus()
          return true
        end
        
        return false
      end

      -- Window focus watcher
      local windowFocusWatcher = hs.window.filter.new()
      windowFocusWatcher:subscribe(hs.window.filter.windowFocused, function(win)
        if win and win:isStandard() then
          addToHistory(win)
        end
      end)

      -- Window closed watcher
      local windowClosedWatcher = hs.window.filter.new()
      windowClosedWatcher:subscribe(hs.window.filter.windowDestroyed, function(win, appName, event)
        -- Small delay to allow the window to close properly
        hs.timer.doAfter(0.1, function()
          focusPreviousWindowOnSameScreen()
        end)
      end)

      -- Initialize when Hammerspoon loads
      initializeHistoryForDisplays()

      -- Install screen watcher to handle new/removed displays
      local screenWatcher = hs.screen.watcher.new(function()
        initializeHistoryForDisplays()
      end)
      screenWatcher:start()

      -- Alert that config has loaded successfully
      hs.alert.show("Hammerspoon config loaded")
    '';
  };

  # One shot password strategy
  # Generates a unique password, saves to 1Password, copies to clipboard, then deletes
  "${xdg_dataHome}/bin/one_shot_password.sh" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      #
      # Required parameters:
      # @raycast.schemaVersion 1
      # @raycast.title One Shot Password
      # @raycast.mode silent
      # @raycast.description Generate a secure password, save to 1Password temporarily, copy to clipboard

      set -euo pipefail

      # Configuration
      VAULT="Private"  # Change this to your preferred vault
      TITLE_PREFIX="OneShot"
      PASSWORD_LENGTH=32

      # Generate timestamp for unique title
      TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
      TITLE="''${TITLE_PREFIX}_''${TIMESTAMP}"

      # Generate secure password
      PASSWORD=$(openssl rand -base64 ''${PASSWORD_LENGTH} | tr -d "/+=" | cut -c1-''${PASSWORD_LENGTH})

      # Save to 1Password
      echo "Saving password to 1Password..."
      ITEM_ID=$(op item create \\
        --category=password \\
        --vault="''${VAULT}" \\
        --title="''${TITLE}" \\
        password="''${PASSWORD}" \\
        --format=json | jq -r '.id')

      if [ -z "''${ITEM_ID}" ]; then
        echo "Error: Failed to create 1Password item"
        exit 1
      fi

      # Copy to clipboard
      echo "''${PASSWORD}" | pbcopy
      echo "Password copied to clipboard!"

      # Schedule deletion after 5 minutes using launchd
      PLIST_NAME="com.oneshot.password.''${TIMESTAMP}"
      PLIST_PATH="''${HOME}/Library/LaunchAgents/''${PLIST_NAME}.plist"

      # Create temporary plist for deletion
      cat > "''${PLIST_PATH}" << EOF
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
      <plist version="1.0">
      <dict>
        <key>Label</key>
        <string>''${PLIST_NAME}</string>
        <key>ProgramArguments</key>
        <array>
          <string>/bin/bash</string>
          <string>-c</string>
          <string>op item delete "''${ITEM_ID}" && rm "''${PLIST_PATH}"</string>
        </array>
        <key>StartCalendarInterval</key>
        <dict>
          <key>Minute</key>
          <integer>$(( $(date +%M) + 5 ))</integer>
          <key>Hour</key>
          <integer>$(date +%H)</integer>
        </dict>
        <key>RunAtLoad</key>
        <false/>
      </dict>
      </plist>
      EOF

      # Load the plist
      launchctl load "''${PLIST_PATH}"

      echo "Password will be automatically deleted from 1Password in 5 minutes."
      echo "Item ID: ''${ITEM_ID}"
    '';
  };
}
