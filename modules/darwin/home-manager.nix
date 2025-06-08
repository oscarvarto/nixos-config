{ config, pkgs, lib, nixCats, neovim-nightly-overlay, op-shell-plugins, user ? "oscarvarto", ... } @ inputs:

let
  sharedFiles = import ../shared/files.nix { inherit config pkgs user; };
  additionalFiles = import ./files.nix { inherit user config pkgs; };
  utils = inputs.nixCats.utils;  # Available when needed
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
    shell = "/opt/homebrew/bin/fish";
  };

  environment.variables = {
    EDITOR = "nvim";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    BAT_THEME="Coldark-Cold";
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
      "borders"
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
      "fish"
      "fish-lsp"
      "gradle"
      "gnuplot"
      "isync"
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
      "1Password for Safari" = 1569813296;
      "neptunes" = 1006739057;
      "rcmd" = 1596283165;
      "XCode" = 497799835;
    };
  };

  # Enable home-manager
  home-manager = {
    useGlobalPkgs = true;
    # extraSpecialArgs = { inherit op-shell-plugins neovim-nightly-overlay; };
    extraSpecialArgs = { inherit inputs; };
    users.${user} = { pkgs, config, lib, ... }: {
      imports = [
        ./fish-config.nix
        inputs.nixCats.homeModules.default
        op-shell-plugins.hmModules.default
      ];

      home = {
        enableNixpkgsReleaseCheck = false;
        packages = pkgs.callPackage ./packages.nix {};
        file = lib.mkMerge [
          sharedFiles
          additionalFiles
        ];

        stateVersion = "25.05";
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

      nixCats = {
        enable = true;
        # nixpkgs_version = inputs.nixpkgs;
        # this will add the overlays from ./overlays and also,
        # add any plugins in inputs named "plugins-pluginName" to pkgs.neovimPlugins
        # It will not apply to overall system, just nixCats.
        addOverlays = /* (import ./overlays inputs) ++ */ [
          (utils.standardPluginOverlay inputs)
        ];
        # see the packageDefinitions below.
        # This says which of those to install.
        packageNames = [ "myHomeModuleNvim" ];

        luaPath = ./nixCats;

        # the .replace vs .merge options are for modules based on existing configurations,
        # they refer to how multiple categoryDefinitions get merged together by the module.
        # for useage of this section, refer to :h nixCats.flake.outputs.categories
        categoryDefinitions.replace = ({ pkgs, settings, categories, extra, name, mkPlugin, ... } @packageDef: {
          # to define and use a new category, simply add a new list to a set here,
          # and later, you will include categoryname = true; in the set you
          # provide when you build the package using this builder function.
          # see :help nixCats.flake.outputs.packageDefinitions for info on that section.

          # lspsAndRuntimeDeps:
          # this section is for dependencies that should be available
          # at RUN TIME for plugins. Will be available to PATH within neovim terminal
          # this includes LSPs
          lspsAndRuntimeDeps = {
            general = with pkgs; [
              lazygit
            ];
            lua = with pkgs; [
              lua-language-server
              stylua
            ];
            nix = with pkgs; [
              nixd
              alejandra
            ];
            go = with pkgs; [
              gopls
              delve
              golint
              golangci-lint
              gotools
              go-tools
              go
            ];
          };

          # This is for plugins that will load at startup without using packadd:
          startupPlugins = {
            general = with pkgs.vimPlugins; [
              # lazy loading isnt required with a config this small
              # but as a demo, we do it anyway.
              lze
              lzextras
              snacks-nvim
              onedark-nvim
              vim-sleuth
            ];
          };

          # not loaded automatically at startup.
          # use with packadd and an autocommand in config to achieve lazy loading
          optionalPlugins = {
            go = with pkgs.vimPlugins; [
              nvim-dap-go
            ];
            lua = with pkgs.vimPlugins; [
              lazydev-nvim
            ];
            general = with pkgs.vimPlugins; [
              mini-nvim
              nvim-lspconfig
              vim-startuptime
              blink-cmp
              nvim-treesitter.withAllGrammars
              lualine-nvim
              lualine-lsp-progress
              gitsigns-nvim
              which-key-nvim
              nvim-lint
              conform-nvim
              nvim-dap
              nvim-dap-ui
              nvim-dap-virtual-text
            ];
          };

          # shared libraries to be added to LD_LIBRARY_PATH
          # variable available to nvim runtime
          sharedLibraries = {
            general = with pkgs; [ ];
          };

          # environmentVariables:
          # this section is for environmentVariables that should be available
          # at RUN TIME for plugins. Will be available to path within neovim terminal
          environmentVariables = {
            # test = {
            #   CATTESTVAR = "It worked!";
            # };
          };

          # categories of the function you would have passed to withPackages
          python3.libraries = {
            # test = [ (_:[]) ];
          };

          # If you know what these are, you can provide custom ones by category here.
          # If you dont, check this link out:
          # https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/setup-hooks/make-wrapper.sh
          extraWrapperArgs = {
            # test = [
            #   '' --set CATTESTVAR2 "It worked again!"''
            # ];
          };
        });

        # see :help nixCats.flake.outputs.packageDefinitions
        packageDefinitions.replace = {
          # These are the names of your packages
          # you can include as many as you wish.
          myHomeModuleNvim = {pkgs, name, ... }: {
            # they contain a settings set defined above
            # see :help nixCats.flake.outputs.settings
            settings = {
              suffix-path = true;
              suffix-LD = true;
              wrapRc = true;
              # unwrappedCfgPath = "/path/to/here";
              # IMPORTANT:
              # your alias may not conflict with your other packages.
              aliases = [ "nvim" "vim" "homeVim" ];
              neovim-unwrapped = neovim-nightly-overlay.packages.${pkgs.system}.neovim;
              hosts.python3.enable = true;
              hosts.node.enable = true;
            };
            # and a set of categories that you want
            # (and other information to pass to lua)
            # and a set of categories that you want
            categories = {
              general = true;
              lua = true;
              nix = true;
              go = false;
            };
            # anything else to pass and grab in lua with `nixCats.extra`
            extra = {
              nixdExtras.nixpkgs = ''import ${pkgs.path} {}'';
            };
          };
        };
      };

      # Marked broken Oct 20, 2022 check later to remove this
      # https://github.com/nix-community/home-manager/issues/3344
      manual.manpages.enable = false;

      services = {};
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
    enable = false;
    # the rest of your shell configuration here
    shellInit = ''
      PATH="/opt/homebrew/bin:$PATH"

      PATH="/opt/homebrew/opt/llvm/bin:$PATH"
      export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
      export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"

      export DOTNET_ROOT=/usr/local/share/dotnet
      PATH="$DOTNET_ROOT:$HOME/.dotnet/tools:$PATH"

      PATH="/opt/homebrew/opt/mysql@8.4/bin:$PATH"
      PATH="/opt/homebrew/opt/gnu-tar/libexec/gnubin:$PATH"
      PATH="/run/current-system/sw/bin:$PATH"

      eval "$(mise activate zsh)"

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

      export PATH
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
        window_placement = "first_child";
        window_zoom_persist = "on";
        window_shadow = "off";
        window_animation_duration = "0.0";  # Faster window operations
        window_animation_easing = "ease_out_circ";
        window_opacity_duration = "0.2";
        active_window_opacity = "1.0";
        normal_window_opacity = "0.70";
        window_opacity = "off";
        insert_feedback_color = "0xffd75f5f";
        split_ratio = "0.70";
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
        layout = "bsp float";
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

        # • Status: launchctl list | grep yabai

        # yabai --restart-service
        ctrl + alt + shift - r : launchctl kickstart -k gui/$(id -u)/org.nixos.yabai

        # yabai --start-service
        ctrl + alt + shift - s : launchctl start gui/$(id -u)/org.nixos.yabai

        # yabai --stop-service
        ctrl + alt + shift - t : launchctl stop gui/$(id -u)/org.nixos.yabai
     '';
    };
  };
}
