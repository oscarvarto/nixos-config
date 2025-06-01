{ user, config, pkgs, ... }:

let
  xdg_configHome = "${config.users.users.${user}.home}/.config";
  xdg_dataHome   = "${config.users.users.${user}.home}/.local/share";
  xdg_stateHome  = "${config.users.users.${user}.home}/.local/state"; 
in
{
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
