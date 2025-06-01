{ user, config, pkgs, ... }:

let
  xdg_configHome = "${config.users.users.${user}.home}/.config";
  xdg_dataHome   = "${config.users.users.${user}.home}/.local/share";
  xdg_stateHome  = "${config.users.users.${user}.home}/.local/state"; 
in
{
  # Helper scripts for Emacs/Ghostty window management

  # Set Ghostty window title.
  "${xdg_configHome}/yabai/set_emacs_ghostty.sh" = {
    executable = true;
    text = ''
      #!/bin/bash
      # Set a unique title for the Ghostty window that pairs with Emacs
      osascript -e 'tell application "Ghostty" to set name of front window to "EmacsPaired-Ghostty"'
    '';
  };

  # Helper functions for window management
  "${xdg_configHome}/yabai/helpers.sh" = {
    executable = true;
    text = ''
      #!/bin/bash

      #### Helper function to notify user of layout changes ####
      notify() {
        osascript -e "display notification \"''$1\" with title \"Yabai\""
      }

      #### Function to get the Emacs-paired Ghostty window ID ####
      get_emacs_ghostty_id() {
        # Look for a Ghostty window with the special title
        GHOSTTY_ID=$(yabai -m query --windows | jq '.[] | select(.app=="Ghostty" and .title=="EmacsPaired-Ghostty").id')

        # If not found, try to use the first Ghostty window in the same space as Emacs
        if [ -z "''${GHOSTTY_ID}" ]; then
          EMACS_SPACE=$(yabai -m query --windows | jq '.[] | select(.app=="Emacs").space')
          if [ -n "''${EMACS_SPACE}" ]; then
            GHOSTTY_ID=$(yabai -m query --windows | jq ".[] | select(.app==\"Ghostty\" and .space=''${EMACS_SPACE}).id" | head -1)
          fi
        fi

        echo "''${GHOSTTY_ID}"
      }

      #### Function to check if both Emacs and its paired Ghostty are available ####
      check_windows() {
        EMACS_ID=$(yabai -m query --windows | jq '.[] | select(.app=="Emacs").id')
        GHOSTTY_ID=$(get_emacs_ghostty_id)

        if [ -z "''${EMACS_ID}" ] || [ -z "''${GHOSTTY_ID}" ]; then
          notify "Both Emacs and its paired Ghostty must be running"
          return 1
        fi
        return 0
      }
    '';
  };

  # Maintain layout
  # "${xdg_configHome}/config/yabai/maintain_layout.sh" = {
  #   executable = true;
  #   text = ''
  #     #!/bin/bash

  #     # Get the current space type
  #     CURRENT_LAYOUT=$(yabai -m query --spaces | jq '.[] | select(.focused==1).type')

  #     # Only proceed if we're in bsp mode
  #     if [ "$CURRENT_LAYOUT" = "\"bsp\"" ]; then
  #         # Check if both Emacs and Ghostty are present
  #         EMACS_ID=$(yabai -m query --windows | jq '.[] | select(.app=="Emacs").id')
  #         GHOSTTY_ID=$(yabai -m query --windows | jq '.[] | select(.app=="Ghostty").id')
  #
  #         if [ -n "$EMACS_ID" ] && [ -n "$GHOSTTY_ID" ]; then
  #             # Check window positions to determine layout
  #             EMACS_FRAME=$(yabai -m query --windows | jq '.[] | select(.app=="Emacs").frame')
  #             GHOSTTY_FRAME=$(yabai -m query --windows | jq '.[] | select(.app=="Ghostty").frame')
  #
  #             # Compare Y coordinates to determine vertical vs horizontal layout
  #             EMACS_Y=$(echo "$EMACS_FRAME" | jq '.y')
  #             GHOSTTY_Y=$(echo "$GHOSTTY_FRAME" | jq '.y')
  #
  #             if [ "$EMACS_Y" -lt "$GHOSTTY_Y" ]; then
  #                 # Vertical layout
  #                 yabai -m config split_type vertical
  #             else
  #                 # Horizontal layout
  #                 yabai -m config split_type horizontal
  #             fi
  #         fi
  #     fi
  #   '';
  # };

  # Maintain layout
  "${xdg_configHome}/config/yabai/maintain_layout.sh" = {
    executable = true;
    text = ''
      #!/bin/bash

      #### Source helper functions ####
      . ~/.config/yabai/helpers.sh

      # Get window IDs
      EMACS_ID=$(yabai -m query --windows | jq '.[] | select(.app=="Emacs").id')
      GHOSTTY_ID=$(get_emacs_ghostty_id)

      if [ -n "''${EMACS_ID}" ] && [ -n "''${GHOSTTY_ID}" ]; then
        # Check window positions to determine layout
        EMACS_FRAME=$(yabai -m query --windows | jq ".[] | select(.id=''${EMACS_ID}).frame")
        GHOSTTY_FRAME=$(yabai -m query --windows | jq ".[] | select(.id=''${GHOSTTY_ID}).frame")

        # Compare Y coordinates to determine vertical vs horizontal layout
        EMACS_Y=$(echo "''${EMACS_FRAME}" | jq '.y')
        GHOSTTY_Y=$(echo "''${GHOSTTY_FRAME}" | jq '.y')

        if [ "''${EMACS_Y}" -lt "''${GHOSTTY_Y}" ]; then
          # Vertical layout
          yabai -m config split_type vertical
        else
          # Horizontal layout
          yabai -m config split_type horizontal
        fi
      fi
     '';
  };

  # Reset layout
  "${xdg_configHome}/yabai/reset_layout.sh" = {
    executable = true;
    text = ''
      #!/bin/bash

      # Get the current space type
      CURRENT_LAYOUT=$(yabai -m query --spaces | jq '.[] | select(.focused==1).type')

      if [ "$CURRENT_LAYOUT" = "\"bsp\"" ]; then
          EMACS_ID=$(yabai -m query --windows | jq '.[] | select(.app=="Emacs").id')
          if [ -n "$EMACS_ID" ]; then
              yabai -m window $EMACS_ID --toggle zoom-parent
              sleep 0.1
              yabai -m window $EMACS_ID --toggle zoom-parent
          fi
      fi
    '';
  };

  # Layout-tracking script
  "${xdg_configHome}/yabai/track_layout.sh" = {
    executable = true;
    text = ''
      #!/bin/bash

      # Source helper functions
      . ~/.config/yabai/helpers.sh

      # Get the current space index
      SPACE_INDEX=$(yabai -m query --spaces --space | jq '.index')
      LAYOUT_FILE="/tmp/yabai-space-''${SPACE_INDEX}-layout"

      # Save current layout
      save_layout() {
          EMACS_ID=$(yabai -m query --windows | jq '.[] | select(.app=="Emacs").id')
          GHOSTTY_ID=$(get_emacs_ghostty_id)

          if [ -n "''${EMACS_ID}" ] && [ -n "''${GHOSTTY_ID}" ]; then
              EMACS_FRAME=$(yabai -m query --windows --window ''${EMACS_ID} | jq '.frame')
              GHOSTTY_FRAME=$(yabai -m query --windows --window ''${GHOSTTY_ID} | jq '.frame')
              echo "{\"emacs\":''${EMACS_FRAME},\"ghostty\":''${GHOSTTY_FRAME}}" > "''${LAYOUT_FILE}"
          fi
      }

      # Restore saved layout
      restore_layout() {
          if [ -f "''${LAYOUT_FILE}" ]; then
              EMACS_ID=$(yabai -m query --windows | jq '.[] | select(.app=="Emacs").id')
              GHOSTTY_ID=$(get_emacs_ghostty_id)

              if [ -n "''${EMACS_ID}" ] && [ -n "''${GHOSTTY_ID}" ]; then
                  SAVED_LAYOUT=$(cat "''${LAYOUT_FILE}")
                  EMACS_FRAME=$(echo "''${SAVED_LAYOUT}" | jq '.emacs')
                  GHOSTTY_FRAME=$(echo "''${SAVED_LAYOUT}" | jq '.ghostty')

                  # Restore frames
                  yabai -m window ''${EMACS_ID} --move abs:$(echo "''${EMACS_FRAME}" | jq '.x'):$(echo "''${EMACS_FRAME}" | jq '.y')
                  yabai -m window ''${EMACS_ID} --resize abs:$(echo "''${EMACS_FRAME}" | jq '.w'):$(echo "''${EMACS_FRAME}" | jq '.h')
                  yabai -m window ''${GHOSTTY_ID} --move abs:$(echo "''${GHOSTTY_FRAME}" | jq '.x'):$(echo "''${GHOSTTY_FRAME}" | jq '.y')
                  yabai -m window ''${GHOSTTY_ID} --resize abs:$(echo "''${GHOSTTY_FRAME}" | jq '.w'):$(echo "''${GHOSTTY_FRAME}" | jq '.h')
              fi
          fi
      }

      case "''$1" in
          save)
              save_layout
              ;;
          restore)
              restore_layout
              ;;
      esac
    '';
  };


  # Raycast script so that "Run Emacs" is available and uses Emacs daemon
  "${xdg_dataHome}/bin/emacsclient" = {
    executable = true;
    text = ''
      #!/bin/zsh
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

      /opt/homebrew/bin/emacsclient -c -n $@
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
}
