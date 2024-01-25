{ config, pkgs, lib, home-manager, ... }:

let
  user = "oscarvarto";
  # Define the content of your file as a derivation
  myEmacsLauncher = pkgs.writeScript "emacs-launcher.command" ''
    #!/bin/sh
    emacsclient -nc -a ''\'''\' &
  '';
  sharedFiles = import ../shared/files.nix { inherit config pkgs; };
  # additionalFiles = import ./files.nix { inherit user config pkgs; };
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

  homebrew = {
    enable = true;
    brews = [
      "btop"
      "editorconfig"
      {
        name = "d12frosted/emacs-plus/emacs-plus";
        args = [ "with-xwidgets" "with-native-comp" "with-poll" ];
        link = true;
      }
      "difftastic"
      "direnv"
      "gcc"
      "gpg"
      "gradle"
      "grip"
      "groovy"
      "kotlin"
      "kotlin-language-server"
      # "ktlint"
      "libtool"
      "libvterm"
      "maven"
      "micromamba"
      # "neovim"
      {
        name = "openjdk";
        link = true;
      }
    ];
    casks = pkgs.callPackage ./casks.nix {};

    onActivation = {
      extraFlags = [ "--verbose" ];
    };

    # These app IDs are from using the mas CLI app
    # mas = mac app store
    # https://github.com/mas-cli/mas
    #
    # $ nix shell nixpkgs#mas
    # $ mas search <app name>
    #
    masApps = {
      #"1password" = 1333542190;
      #"wireguard" = 1451685025;
      "Amazon Prime Video" = 545519333;
      "WhatsApp Messenger" = 310633997;
    };
  };

  # Enable home-manager
  home-manager = {
    useGlobalPkgs = true;
    users.${user} = { pkgs, config, lib, ... }:{
      home = {
        enableNixpkgsReleaseCheck = false;
        packages = pkgs.callPackage ./packages.nix {};
        file = lib.mkMerge [
          sharedFiles
          # additionalFiles
          { "emacs-launcher.command".source = myEmacsLauncher; }
        ];

        stateVersion = "23.11";
      };

      programs = { } // import ../shared/home-manager.nix { inherit config pkgs lib; };

      # Marked broken Oct 20, 2022 check later to remove this
      # https://github.com/nix-community/home-manager/issues/3344
      manual.manpages.enable = false;
    };
  };

  # Fully declarative dock using the latest from Nix Store
  local = { 
    dock = {
      enable = true;
      entries = [
        { path = "/Applications/Brave Browser.app/"; }
	{ path = "/Users/oscarvarto/Applications/Rider.app"; }
	{ path = "/Applications/Emacs.app/"; }
        { path = "/Applications/Zed.app/"; }
	{ path = "/Applications/Adobe Acrobat Reader.app"; }
	{ path = "/Applications/LibreOffice.app/"; }
        { path = "/Applications/kitty.app/"; }
        { path = "/Applications/WezTerm.app/"; }
        { path = "/Applications/HopToDesk.app/"; }
        # { path = "/System/Applications/Music.app/"; }
        { path = "/Applications/Spotify.app/"; }
        { path = "/System/Applications/TV.app/"; }
	{ path = "/Applications/Prime Video.app/"; }
        { path = "/Applications/Proton Pass.app/"; }
        { path = "/Applications/ProtonVPN.app/"; }
        { path = "/Applications/Betterbird.app/"; }
        { path = "/Applications/Telegram.app/"; }
	{ path = "/Applications/Slack.app"; }
        # {
        #   path = toString myEmacsLauncher;
        #   section = "others";
        # }
        # {
        #   path = "${config.users.users.${user}.home}/.local/share/";
        #   section = "others";
        #   options = "--sort name --view grid --display folder";
        # }
        {
          path = "${config.users.users.${user}.home}/Downloads";
          section = "others";
          options = "--sort name --view grid --display stack";
        }
      ];
    };
  };
}
