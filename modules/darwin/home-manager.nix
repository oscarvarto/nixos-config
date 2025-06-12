{ config, pkgs, lib, neovim-nightly-overlay, op-shell-plugins, user ? "oscarvarto", ... } @ inputs:

let
  sharedFiles = import ../shared/files.nix { inherit config pkgs user; };
  additionalFiles = import ./files.nix { inherit user config pkgs; };
  utils = inputs.nixCats.utils;
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
      "bfg"
      # "borders"
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
      "eza"
      "fish"
      "fish-lsp"
      "gradle"
      "git-filter-repo"
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
      "pueue"
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
      # "XCode" = 497799835;
    };
  };

  # Enable home-manager
  home-manager = {
    useGlobalPkgs = true;
    backupFileExtension = "backup";
    # extraSpecialArgs = { inherit op-shell-plugins neovim-nightly-overlay; };
    extraSpecialArgs = { inherit inputs; };
    users.${user} = { pkgs, config, lib, ... }: {
      imports = [
        ./fish-config.nix
        ../shared/git-security.nix
        inputs.nixCats.homeModules.default
        op-shell-plugins.hmModules.default
        ../shared/nushell
      ];

      # Enable nushell with Nix environment integration
      local.nushell = {
        enable = true;
        left_prompt_cmd = "hostname -s";
        history_file_format = "sqlite";
      };

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
      } // import ../shared/home-manager.nix { inherit config pkgs lib; /* myEmacs */ };

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
              modus-themes-nvim
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
              unwrappedCfgPath = ./nixCats;
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

      # macOS LaunchAgent for NordVPN autoconnect at login - ONE SHOT ONLY
      launchd.agents.nordvpn-autoconnect = {
        enable = true;
        config = {
          Label = "local.nordvpn-autoconnect";
          ProgramArguments = [
            "/bin/bash"
            "-c"
            # One-shot execution with built-in retry logic and connectivity checks
            "${config.home.homeDirectory}/nixos-config/nordvpn-autoconnect.sh"
          ];
          EnvironmentVariables = {
            # No hardcoded IP - script retrieves from 1Password
            # Uncomment to disable VPN autoconnect entirely
            # SKIP_VPN_AUTOCONNECT = "true";
            # Uncomment to skip when GlobalProtect is active
            # GLOBALPROTECT_ACTIVE = "true";
            PATH = "/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/Users/oscarvarto/.nix-profile/bin";
            # Ensure 1Password CLI has access to GUI for authentication if needed
            HOME = config.home.homeDirectory;
          };
          RunAtLoad = true;
          KeepAlive = false;
          StandardOutPath = "${config.home.homeDirectory}/.nordvpn-autoconnect-out.log";
          StandardErrorPath = "${config.home.homeDirectory}/.nordvpn-autoconnect-err.log";
          # ONE SHOT - No StartInterval, runs only once at login
        };
      };
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

  };
}
