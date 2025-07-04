{ pkgs }:

with pkgs;
let shared-packages = import ../shared/packages.nix { inherit pkgs; }; in
shared-packages ++ [
  # Minimal darwin-specific packages only
  dockutil
  mas
  netcoredbg
]
