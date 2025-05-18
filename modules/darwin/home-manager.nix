{ config, pkgs, op-shell-plugins , lib, home-manager, ... }:

let
  user = "oscarvarto";
  sharedFiles = import ../shared/files.nix { inherit config pkgs user; };
  additionalFiles = import ./files.nix { inherit user config pkgs; };
in
{

  imports = [
    # ./dock
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
      "awscli"
      "bat"
      "bat-extras"
      "cmake"
      "difftastic"
      "dotnet"
      {
        name = "emacs-plus@31";
        args = [ "with-xwidgets" "with-imagemagick" "with-savchenkovaleriy-big-sur-curvy-3d-icon" "with-mailutils" ];
        link = true;
      }
      "gradle"
      "jq"
      "libedit"
      "libvterm"
      "libtool"
      "llvm"
      "lua"
      "maven"
      "mise"
      "mu"
      "ncurses"
      "ninja"
      "nushell"
      "pass"
      "pinentry-mac"
      "swig"
      "wmctrl"
      "xz" # lzma is part of xz
      "yq"
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
          plugins = with pkgs; [ gh awscli2 cachix ];
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

  # this can also be `programs.bash` or `programs.fish`
  programs.zsh = {
    enable = true;
    # the rest of your shell configuration here
    shellInit = ''
      export PATH=/opt/homebrew/bin:/opt/homebrew/opt/llvm/bin:$PATH
      export JAVA_HOME=$(/usr/libexec/java_home -v 24)
      export BAT_THEME=Coldark-Cold

      alias tg="$EDITOR $HOME/Library/Application\ Support/com.mitchellh.ghostty/config"
    '';
  };
}
