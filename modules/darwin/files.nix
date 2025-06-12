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

}
