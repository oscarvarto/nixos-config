{ config, pkgs, lib, catppuccin, neovim-nightly-overlay, op-shell-plugins , user ? "oscarvarto", ... } @ inputs:

let
  sharedFiles = import ../shared/files.nix { inherit config pkgs user; };
  additionalFiles = import ./files.nix { inherit user config pkgs; };
  utils = inputs.nixCats.utils;
  inherit (builtins) fromTOML;
in
{

  imports = [
    ./dock
    ./brews.nix
    ./window-manager.nix
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
    BAT_THEME="ansi";
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
        ../shared/nushell
        inputs.nixCats.homeModules.default
        op-shell-plugins.hmModules.default
        catppuccin.homeModules.catppuccin
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
          {
            # Copy the external Zellij configuration file
            ".config/zellij/config.kdl".source = pkgs.writeText "zellij-config.kdl" (builtins.readFile ./zellij-config.kdl);
          }
        ];

        stateVersion = "25.05";
      };

      catppuccin = {
        enable = true;
        fish.enable = true;
        flavor = "mocha";
        fzf.enable = true;
        helix = {
          enable =true;
          useItalics = true;
        };
        starship.enable = false;
        vscode.enable = true;
      };

      programs = {
        _1password-shell-plugins = {
          # enable 1Password shell plugins for bash, zsh, and fish shell
          enable = true;
          # the specified packages as well as 1Password CLI will be
          # automatically installed and configured to use shell plugins
          plugins = with pkgs; [ awscli2 cachix gh glab ];
        };

        atuin = {
          enable = true;
          daemon.enable = true;
          enableFishIntegration = true;
          enableNushellIntegration = true;
          enableZshIntegration = true;
        };

        helix.enable = true;

        jujutsu = {
          enable = true;
          settings = {
            ui.editor = "nvim";
            user = {
              email = "contact@oscarvarto.mx";
              name = "Oscar Vargas Torres";
            };
          };
        };

        mise = {
          enable = true;
          enableFishIntegration = true;
          enableNushellIntegration = true;
        };

        starship = {
          enable = true;
          enableZshIntegration = true;
          enableFishIntegration = true;
          enableNushellIntegration = true;
          settings = fromTOML(builtins.readFile ./starship.toml);
        };

        vscode = {
          enable = true;
          mutableExtensionsDir = true;
        };

        yazi = {
          enable = true;
          enableFishIntegration = true;
          enableNushellIntegration = true;
          enableZshIntegration = true;
        };

        zoxide = {
          enable = true;
          enableFishIntegration = true;
          enableNushellIntegration = true;
          enableZshIntegration = true;
        };

        # zellij is installed via homebrew and configured manually
        # We use external config file instead of home-manager settings
      } // import ../shared/home-manager.nix { inherit config pkgs lib; /* myEmacs */ };

      # Marked broken Oct 20, 2022 check later to remove this
      # https://github.com/nix-community/home-manager/issues/3344
      manual.manpages.enable = false;

    } // import ./nixcats.nix { inherit config pkgs lib inputs; };
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

}
