{ config, pkgs, lib, ... }:

let 
  user = "oscarvarto";
in
{
  programs = {
    # Shared shell utilities
    broot = {
      enable = true;
      enableFishIntegration = true;
      enableNushellIntegration = true;
      enableZshIntegration = true;
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    fzf = {
      enable = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
    };

    # Consolidated zsh configuration for both Darwin and shared use
    zsh = {
      enable = true;
      autocd = false;
      cdpath = [ "~/.local/share/src" ];
      plugins = [ ];
      
      # Base configuration that works everywhere
      initContent = lib.mkAfter ''
        # Nix daemon setup
        if [[ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]]; then
          . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
          . /nix/var/nix/profiles/default/etc/profile.d/nix.sh
        fi

        export LC_ALL="en_US.UTF-8"
        bindkey "^[[3~" delete-char

        # Define variables for directories
        export EMACSDIR=$HOME/.emacs.d
        export DOOMDIR=$HOME/.doom.d
        export DOOMLOCALDIR=$HOME/.emacs.d/.local
        export PATH=$HOME/.pnpm-packages/bin:$HOME/.pnpm-packages:$PATH
        export PATH=$HOME/.npm-packages/bin:$HOME/bin:$PATH
        export PATH=$HOME/.local/share/bin:$PATH
        export PATH=$HOME/.local/bin:$PATH
        export PATH=$HOME/.cargo/bin:$PATH
        export PATH=$EMACSDIR/bin:$PATH
        export PATH=$HOME/bin:$PATH
        export PATH="$HOME/Library/Application Support/Coursier/bin:$PATH"
        export PATH="$HOME/.volta/bin:$PATH"

        # Remove history data we don't want to see
        export HISTIGNORE="pwd:ls:cd"

        # Ripgrep alias
        alias search=rg -p --glob '!node_modules/*' $@

        export ALTERNATE_EDITOR=""
        export EDITOR="nvim"
        export VISUAL="/opt/homebrew/bin/emacsclient -nc -s /var/folders/yh/5_g54kd572gd9vr8tbc4m6gh0000gn/T/emacs501/doom"

        t() {
           # nvim "$@"
           /opt/homebrew/bin/emacsclient -nw -s /var/folders/yh/5_g54kd572gd9vr8tbc4m6gh0000gn/T/emacs501/doom "$@"
        }

        ec() {
           /opt/homebrew/bin/emacsclient -nc -s /var/folders/yh/5_g54kd572gd9vr8tbc4m6gh0000gn/T/emacs501/doom "$@"
        }

        e() {
           emacs & disown
        }

        # nix shortcuts
        shell() {
            nix-shell '<nixpkgs>' -A "$1"
        }

        # Always color ls and group directories
        alias ls='ls --color=auto'
 
        alias tg="$EDITOR $HOME/.config/ghostty/config"
        alias edd="emacs --daemon=doom"
 
        alias pke="pkill -9 Emacs"
        alias nz="nvim ~/.zshrc"
        alias gd="ghostty +show-config --default --docs"
        alias gp="git fetch --all -p; git pull; git submodule update --recursive"
        alias ds="doom sync --aot --gc -j \\$(nproc)"
        alias dup="doom sync -u --aot --gc -j \\$(nproc)"
        alias diff="difft"
        alias nb="pushd \\$HOME/nixos-config > /dev/null; nix run .#build; popd > /dev/null"
        alias ns="pushd \\$HOME/nixos-config > /dev/null; nix run .#build-switch; popd > /dev/null"
      '';
    };
  };
}
