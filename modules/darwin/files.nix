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

      /Users/${user}/.local/share/bin/emacsclient -s /var/folders/f4/08zm_5ks36vc7tw43765qk580000gn/T/emacs501/doom --eval "(emacs-everywhere)"; \
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

      /Users/${user}/.local/share/bin/emacsclient -nc -s /var/folders/f4/08zm_5ks36vc7tw43765qk580000gn/T/emacs501/doom "$@"
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

  # Hammerspoon configuration
  "${hammerspoonDir}/init.lua" = {
    text = ''
    '';
  };

}
