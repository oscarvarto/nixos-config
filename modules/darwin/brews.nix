{ config, pkgs, lib, ... }:

{
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
      # Essential brews
      "7-zip"
      "atuin"
      "autoconf"
      "autoconf-archive"
      "automake"
      "awscli"
      "bat"
      "bat-extras"
      "bfg"
      "carapace"
      "cargo-binstall"
      "ccache"
      "cmake"
      "coursier"
      "difftastic"
      {
        name = "emacs-plus@31";
        args = [ "with-xwidgets" "with-imagemagick" "with-savchenkovaleriy-big-sur-curvy-3d-icon" "with-mailutils" ];
        link = true;
      }
      "eza"
      "fish-lsp"
      "ffmpeg"
      "gradle"
      "git-filter-repo"
      "gnuplot"
      "go"
      "helix"
      "hugo"
      "imagemagick"
      "isync"
      "jq"
      "libedit"
      "libvterm"
      "libsql"
      "libtool"
      "llvm"
      "lua"
      "markdown-oxide"
      "marksman"
      "maven"
      "mise"
      "mosh"
      "mu"
      "mysql@8.4"
      "nasm"
      "ncurses"
      "ninja"
      "pandoc"
      "pueue"
      "pass"
      "pkg-config"
      "pinentry-mac"
      "pixi"
      "poppler"
      "resvg"
      "stow"
      "swig"
      "trash-cli"
      "uv"
      "vivid"
      "vcpkg"
      "volta"
      "wmctrl"
      "xz" # lzma is part of xz
      "yq"
      "zellij"
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
      "1Password for Safari" = 1569813296;  # Temporarily disabled
      "neptunes" = 1006739057;
      "Okta Verify" = 490179405;
      "rcmd" = 1596283165;
      "XCode" = 497799835;
    };
  };
}
