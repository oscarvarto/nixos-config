{ config, pkgs, ... }:

let
  neovimOverlaySha256 = "1nvdv0rmpqmzgjjs5jhp4v22lxr9gillb8wgz05di7gvbjfzxlmn";
in
{
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowInsecure = false;
      allowUnsupportedSystem = true;
    };

    overlays =
      # Apply each overlay found in the /overlays directory
      let path = ../../overlays; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)))
      ++ [(import (builtins.fetchTarball {
            url = "https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz";
            sha256 = neovimOverlaySha256;
           }))];

      # ++ [(import (builtins.fetchTarball {
      #          url = "https://github.com/dustinlyons/emacs-overlay/archive/refs/heads/master.tar.gz";
      #          sha256 = emacsOverlaySha256;
      #      }))];
  };
}
