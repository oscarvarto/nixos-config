{ config, pkgs, lib, ... }:

let 
  name = "Oscar Vargas Torres";
  user = "oscarvarto";
  email = "contact@oscarvarto.mx";
  shellConfig = import ./shell-config.nix { inherit config pkgs lib; };
  gitIgnores = import ./git-ignores.nix { inherit config pkgs lib; };
in
{
  # Merge shell configurations directly (flatten)
  programs = shellConfig.programs // {
    git = {
    enable = true;
    ignores = gitIgnores.git.ignores;
    userName = name;
    # userEmail is handled by conditional includes based on directory
    lfs = {
      enable = true;
    };
    extraConfig = {
      init.defaultBranch = "main";
      core = {
        editor = "nvim";
        autocrlf = false;  # Better for macOS/Linux - preserves line endings as-is
        eol = "lf";        # Use LF line endings on macOS/Linux
        ignorecase = false; # Case-sensitive file names (better for cross-platform)
        # hooksPath removed - now configured conditionally
      };
      commit.gpgsign = false;
      diff.colorMoved = "zebra"; # https://spin.atomicobject.com/git-configurations-default/
      fetch.prune = true;
      pull.rebase = true;
      push.autoSetupRemote = true;
      rebase.autoStash = true;
      safe.directory = [
        "*"  # Trust all directories (most comprehensive)
        "/Users/${user}/nixos-config"
        "/nix/store/*"
        "/opt/homebrew/*"
      ];
      # Conditional includes for work directory
      includeIf."gitdir:/Users/${user}/ir/**".path = "/Users/${user}/.local/share/git/config-work";
      # Conditional include for Doom Emacs directory (with hooks)
      includeIf."gitdir:/Users/${user}/.emacs.d/".path = "/Users/${user}/.config/git/config-doom";
      # Include personal config as fallback for all other directories
      include.path = "/Users/${user}/.config/git/config-personal";
    };

    ssh = {
      enable = true;
      includes = [
        (lib.mkIf pkgs.stdenv.hostPlatform.isLinux
          "/home/${user}/.ssh/config_external"
        )
        (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin
          "/Users/${user}/.ssh/config_external"
        )
      ];
      matchBlocks = {
        "github.com" = {
          identitiesOnly = true;
          identityFile = [
            (lib.mkIf pkgs.stdenv.hostPlatform.isLinux
              "/home/${user}/.ssh/id_ed25519"
            )
            (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin
              "/Users/${user}/.ssh/id_ed25519"
            )
          ];
        };
      };
    };
  };
}
