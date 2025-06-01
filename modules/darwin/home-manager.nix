{ config, pkgs, op-shell-plugins, lib, home-manager, user ? "oscarvarto", ... }:

let
  sharedFiles = import ../shared/files.nix { inherit config pkgs user; };
  additionalFiles = import ./files.nix { inherit user config pkgs; };
in
{

  imports = [
    ./dock
  ];

  # It me
  users.users.${user} = {
    name = "${user}";
    home = "/Users/${user}";
    isHidden = false;
    shell = pkgs.zsh;
  };

  environment.variables = {
    EDITOR = "nvim";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    BAT_THEME="Coldark-Dark";
  };

  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "none";
      extraFlags = [ "--verbose" ];
      upgrade = true;
    };

    taps = [
      "d12frosted/homebrew-emacs-plus"
    ];

    brews = [
      "autoconf"
      "autoconf-archive"
      "automake"
      "awscli"
      "bat"
      "bat-extras"
      "cargo-binstall"
      "ccache"
      "cmake"
      "difftastic"
      "dotnet"
      {
        name = "emacs-plus@31";
        args = [ "with-xwidgets" "with-imagemagick" "with-savchenkovaleriy-big-sur-curvy-3d-icon" "with-mailutils" ];
        link = true;
      }
      "borders"
      "fish"
      "fish-lsp"
      "gradle"
      "jq"
      "libedit"
      "libvterm"
      "libsql"
      "libtool"
      "llvm"
      "lua"
      "maven"
      "mise"
      "mu"
      "mysql@8.4"
      "nasm"
      "ncurses"
      "ninja"
      "nushell"
      "pandoc"
      "pass"
      "pkg-config"
      "pinentry-mac"
      "pixi"
      "swig"
      "trash-cli"
      "uv"
      "vivid"
      "vcpkg"
      "volta"
      "wmctrl"
      "xz" # lzma is part of xz
      "yq"
      "zig"
    ];

    casks = pkgs.callPackage ./casks.nix {};
    caskArgs.appdir = "/Applications";

    # onActivation.cleanup = "uninstall";
   
    # These app IDs are from using the mas CLI app
    # mas = mac app store
    # https://github.com/mas-cli/mas
    #
    # $ nix shell nixpkgs#mas
    # $ mas search <app name>
    #
    # If you have previously added these apps to your Mac App Store profile (but not installed them on this system),
    # you may receive an error message "Redownload Unavailable with This Apple ID".
    # This message is safe to ignore. (https://github.com/dustinlyons/nixos-config/issues/83)

    masApps = {
      # "1password" = 1333542190;
      # "wireguard" = 1451685025;
      "neptunes" = 1006739057;
    };
  };

  # Enable home-manager
  home-manager = {
    useGlobalPkgs = true;
    users.${user} = { pkgs, config, lib, ... }:{
      imports = [
        op-shell-plugins.hmModules.default
      ];

      home = {
        enableNixpkgsReleaseCheck = false;
        packages = pkgs.callPackage ./packages.nix {};
        file = lib.mkMerge [
          sharedFiles
          additionalFiles
        ];

        stateVersion = "23.11";
      };

      programs = {
        _1password-shell-plugins = {
          # enable 1Password shell plugins for bash, zsh, and fish shell
          enable = true;
          # the specified packages as well as 1Password CLI will be
          # automatically installed and configured to use shell plugins
          plugins = with pkgs; [ awscli2 cachix gh glab ];
        };
      } // import ../shared/home-manager.nix { inherit
          config
          pkgs
          lib
          # myEmacs
          ;
        };

      services = {
      };

      # Marked broken Oct 20, 2022 check later to remove this
      # https://github.com/nix-community/home-manager/issues/3344
      manual.manpages.enable = false;
    };
  };

  # Fully declarative dock using the latest from Nix Store
  local = {
    dock = {
      enable = true;
      username = user;
      entries = [
        { path = "/Applications/Emacs.app/"; }
        { path = "/Applications/Zed Preview.app/"; }
        { path = "/Applications/Ghostty.app/"; }
        { path = "/Applications/WarpPreview.app/"; }
        { path = "/Applications/Firefox Nightly.app/"; }
        { path = "/Applications/Microsoft Edge.app/"; }
        { path = "/Applications/Google Chrome.app/"; }
        { path = "/System/Volumes/Preboot/Cryptexes/App/System/Applications/Safari.app/"; }
        { path = "/Applications/Microsoft Teams.app/"; }
        { path = "/Applications/Microsoft Outlook.app/"; }
        { path = "/Applications/zoom.us.app/"; }
        { path = "/Applications/Parallels Desktop.app/"; }
        { path = "/Applications/Beekeeper Studio.app/"; }
        { path = "/System/Applications/Music.app/"; }
        { path = "/System/Applications/Calendar.app/"; }
        { path = "/System/Applications/System Settings.app/"; }
      ];
    };
  };

  # this can also be `programs.bash` or `programs.fish`
  programs.zsh = {
    enable = true;
    # the rest of your shell configuration here
    shellInit = ''
      PATH="/opt/homebrew/bin:$PATH"

      PATH="/opt/homebrew/opt/llvm/bin:$PATH"
      export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
      export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"

      export DOTNET_ROOT=/usr/local/share/dotnet
      PATH="$DOTNET_ROOT:$HOME/.dotnet/tools:$PATH"

      PATH="/opt/homebrew/opt/mysql@8.4/bin:$PATH"
      export PATH="/opt/homebrew/opt/gnu-tar/libexec/gnubin:$PATH"
      eval "$(mise activate zsh)"

      alias tg="$EDITOR $HOME/.config/ghostty/config"

      # >>> conda initialize >>>
      # !! Contents within this block are managed by 'conda init' !!
      __conda_setup="$('/opt/homebrew/Caskroom/miniforge/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
      if [ $? -eq 0 ]; then
          eval "$__conda_setup"
      else
          if [ -f "/opt/homebrew/Caskroom/miniforge/base/etc/profile.d/conda.sh" ]; then
              . "/opt/homebrew/Caskroom/miniforge/base/etc/profile.d/conda.sh"
          else
              export PATH="/opt/homebrew/Caskroom/miniforge/base/bin:$PATH"
          fi
      fi
      unset __conda_setup
      # <<< conda initialize <<<
    '';
  };

  services = {

    jankyborders = {
      enable = true;
      active_color = "0xff00ff00";
      inactive_color = "0xff494d64";
      width = 10.0;
    };

    yabai = {
      enable = true;
      enableScriptingAddition = true;
      config = {
        mouse_follows_focus = "on";
        focus_follows_mouse = "autofocus";
        display_arrangement_order = "horizontal";
        window_origin_display = "default";
        window_placement = "second_child";
        window_zoom_persist = "on";
        window_shadow = "on";
        window_animation_duration = "0.0";  # Faster window operations
        window_animation_easing = "ease_out_circ";
        window_opacity_duration = "0.2";
        active_window_opacity = "1.0";
        normal_window_opacity = "0.7";
        window_opacity = "on";
        insert_feedback_color = "0xffd75f5f";
        split_ratio = "0.50";
        split_type = "auto";
        auto_balance = "off";
        # Add window borders for easier identification
        window_border = "on";
        window_border_width = "4";
        active_window_border_color = "0xFF40FF00";
        normal_window_border_color = "0x00FFFFFF";
        # Adjust padding for different layouts
        external_bar = "all:0:0";
        top_padding = 20;
        bottom_padding = 20;
        left_padding = 20;
        right_padding = 20;
        window_gap = 20;
        layout = "bsp f";
        mouse_modifier = "fn";
        mouse_action1 = "move";
        mouse_action2 = "resize";
        mouse_drop_action = "swap";
      };
      extraConfig = ''
        yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
        sudo yabai --load-sa

        #### Ghostty ####
        yabai -m signal --add app='^Ghostty$' event=window_created action='yabai -m space --layout bsp'
        yabai -m signal --add app='^Ghostty$' event=window_destroyed action='yabai -m space --layout bsp'

        # yabai -m rule --add app="^Google Chrome$" space=^10
        # float system preferences
        yabai -m rule --add app="^System Settings$" manage=off
        yabai -m rule --add title="Zoom Workplace" manage=off
        yabai -m rule --add title="Zoom Meeting" manage=off

        # Emacs specific rules
        # yabai -m rule --add app="^Emacs$" manage=on space=^1

        # Add signal handlers for maintaining layout
        yabai -m signal --add event=window_created app="^Emacs$" action='~/.config/yabai/maintain_layout.sh'
        yabai -m signal --add event=window_moved app="^Emacs$" action='~/.config/yabai/maintain_layout.sh'
        yabai -m signal --add event=window_resized app="^Emacs$" action='~/.config/yabai/maintain_layout.sh'
        yabai -m signal --add event=window_created app="^Ghostty$" action='~/.config/yabai/maintain_layout.sh'
        yabai -m signal --add event=window_moved app="^Ghostty$" action='~/.config/yabai/maintain_layout.sh'
        yabai -m signal --add event=window_resized app="^Ghostty$" action='~/.config/yabai/maintain_layout.sh'
        yabai -m signal --add event=application_activated app="^Emacs$" action='~/.config/yabai/maintain_layout.sh'
        yabai -m signal --add event=application_activated app="^Ghostty$" action='~/.config/yabai/maintain_layout.sh'

        # Signal handlers for Emacs/Ghostty layout management
        yabai -m signal --add event=window_created app="^Emacs$" action='~/.config/yabai/maintain_layout.sh'
        yabai -m signal --add event=window_created app="^Ghostty$" action='~/.config/yabai/maintain_layout.sh'
        yabai -m signal --add event=window_destroyed app="^Emacs$" action='~/.config/yabai/maintain_layout.sh'
        yabai -m signal --add event=window_destroyed app="^Ghostty$" action='~/.config/yabai/maintain_layout.sh'

        # Better window restoration
        yabai -m signal --add event=application_activated app="^Emacs$" action='~/.config/yabai/maintain_layout.sh'
        yabai -m signal --add event=application_activated app="^Ghostty$" action='~/.config/yabai/maintain_layout.sh'

        # Track layout changes
        yabai -m signal --add event=window_resized action='~/.config/yabai/track_layout.sh save'
        yabai -m signal --add event=window_moved action='~/.config/yabai/track_layout.sh save'
        yabai -m signal --add event=space_changed action='~/.config/yabai/track_layout.sh restore'

        borders &
      '';
    };

    skhd = {
      enable = true;
      skhdConfig = ''
        # Example and documentation here: https://github.com/koekeishiya/yabai/blob/master/examples/skhdrc
        cmd + ctrl + shift - r : skhd -r
        cmd + alt - 1  : yabai -m space --focus 1
        cmd + alt - 2  : yabai -m space --focus 2
        cmd + alt - 3  : yabai -m space --focus 3
        cmd + alt - 4  : yabai -m space --focus 4
        cmd + alt - 5  : yabai -m space --focus 5
        cmd + alt - 6  : yabai -m space --focus 6
        cmd + alt - 7  : yabai -m space --focus 7
        cmd + alt - 8  : yabai -m space --focus 8
        cmd + alt - 9  : yabai -m space --focus 9
        cmd + alt - 0  : yabai -m space --focus 10

        # send window to desktop and follow focus
        shift + cmd + alt - 1  : yabai -m window --space  1; yabai -m space --focus  1
        shift + cmd + alt - 2  : yabai -m window --space  2; yabai -m space --focus  2
        shift + cmd + alt - 3  : yabai -m window --space  3; yabai -m space --focus  3
        shift + cmd + alt - 4  : yabai -m window --space  4; yabai -m space --focus  4
        shift + cmd + alt - 5  : yabai -m window --space  5; yabai -m space --focus  5
        shift + cmd + alt - 6  : yabai -m window --space  6; yabai -m space --focus  6
        shift + cmd + alt - 7  : yabai -m window --space  7; yabai -m space --focus  7
        shift + cmd + alt - 8  : yabai -m window --space  8; yabai -m space --focus  8
        shift + cmd + alt - 9  : yabai -m window --space  9; yabai -m space --focus  9
        shift + cmd + alt - 0  : yabai -m window --space 10; yabai -m space --focus 10

        # options: zoom-parent, zoom-fullscreen, native-fullscreen
        ctrl + alt + shift - f : yabai -m window --toggle native-fullscreen
        ctrl + alt + shift - r : yabai --restart-service
        ctrl + alt + shift - t : yabai --stop-service
        ctrl + alt + shift - s : yabai --start-service
        ctrl + alt + shift - 0 : yabai -m config window_opacity off
        ctrl + alt + shift - 1 : yabai -m config window_opacity on
        # cmd + alt + cmd - e : /Users/oscarvarto/.local/share/bin/emacsclient -s /Users/${user}/.local/run/emacs/server -e '(emacs-everywhere)'
        cmd + alt + cmd - e :  /opt/homebrew/bin/emacsclient -s /Users/${user}/.local/run/emacs/server -e '(emacs-everywhere)'
      '';
    };
  };

}
