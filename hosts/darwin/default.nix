{ agenix, config, pkgs, nixCats, ... }:

let 
  user = "oscarvarto";
in

{
  imports = [
    ../../modules/darwin/secrets.nix
    ../../modules/darwin/home-manager.nix
    ../../modules/shared
    agenix.darwinModules.default
  ];

  # Setup user, packages, programs
  nix = {
    package = pkgs.nix;

    settings = {
      trusted-users = [ "@admin" "${user}" ];
      substituters = [ "https://nix-community.cachix.org" "https://cache.nixos.org"];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      ];

      warn-dirty = true;
      # produces linking issues when updating on macOS
      # https://github.com/NixOS/nix/issues/7273
      auto-optimise-store = false;
    };

    gc = {
      automatic = true;
      interval = { Weekday = 0; Hour = 2; Minute = 0; };
      options = "--delete-older-than 10d";
    };

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  ids.gids.nixbld = 350;

  # Turn off NIX_PATH warnings now that we're using flakes
  system.checks.verifyNixPath = false;

  # Load configuration that is shared across systems
  environment.systemPackages = with pkgs; [
    agenix.packages."${pkgs.system}".default
  ] ++ (import ../../modules/shared/packages.nix { inherit pkgs; });

  # Add fish to available shells
  environment.shells = [ "/opt/homebrew/bin/fish" ];

  security.pam.services.sudo_local.touchIdAuth = true;

  # Enable fish shell system-wide
  # programs.zsh.enable = true;
  programs.fish.enable = true;

    # Set global environment variables for GUI applications launched via launchd
    # This ensures that applications like Ghostty launched via hotkeys have access
    # to the same PATH as terminal applications, including Nix store paths
    launchd.envVariables = {
      PATH = [
        # User script directories
        "/Users/oscarvarto/.local/bin"
        "/Users/oscarvarto/.local/share/bin"
        "/Users/oscarvarto/.cargo/bin"
        "/Users/oscarvarto/.emacs.d/bin"
        "/Users/oscarvarto/.volta/bin"
        "/Users/oscarvarto/Library/Application Support/Coursier/bin"

        # Nix paths (essential for Nix-managed tools)
        "/Users/oscarvarto/.nix-profile/bin"
        "/run/current-system/sw/bin"
        "/nix/var/nix/profiles/default/bin"

        # Homebrew paths
        "/opt/homebrew/bin"
        "/opt/homebrew/opt/mise/bin"
        "/opt/homebrew/opt/llvm/bin"
        "/opt/homebrew/opt/mysql@8.4/bin"
        "/opt/homebrew/opt/gnu-tar/libexec/gnubin"

        # System paths
        "/usr/local/bin"
        "/usr/bin"
        "/bin"
        "/usr/sbin"
        "/sbin"
        "/Library/Apple/usr/bin"
        "/Library/TeX/texbin"
      ];
    };

  system = {
    stateVersion = 4;
    primaryUser = "oscarvarto";

    defaults = {
      NSGlobalDomain = {
        AppleShowAllExtensions = true;
        ApplePressAndHoldEnabled = false;

        # 120, 90, 60, 30, 12, 6, 2
        KeyRepeat = 2;

        # 120, 94, 68, 35, 25, 15
        InitialKeyRepeat = 15;

        "com.apple.mouse.tapBehavior" = 1;
        "com.apple.sound.beep.volume" = 0.0;
        "com.apple.sound.beep.feedback" = 0;
      };

      dock = {
        autohide = true;
        show-recents = false;
        launchanim = false;
        orientation = "bottom";
        tilesize = 50;
      };

      finder = {
        _FXShowPosixPathInTitle = false;
      };

      trackpad = {
        Clicking = true;
        TrackpadThreeFingerDrag = false;
      };
    };
  };
}
