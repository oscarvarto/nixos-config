{ ... }:

{
  programs.fish = {
    enable = true;
    generateCompletions = false;
    shellInit = ''
      # Nix daemon initialization
      if test -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
        set -gx NIX_SSL_CERT_FILE /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt
        set -gx NIX_PROFILES "/nix/var/nix/profiles/default $HOME/.nix-profile"
        set -gx NIX_PATH "/nix/var/nix/profiles/per-user/root/channels"
        fish_add_path /nix/var/nix/profiles/default/bin
      end

      # PATH configuration
      fish_add_path /opt/homebrew/bin
      fish_add_path /opt/homebrew/opt/llvm/bin
      fish_add_path /opt/homebrew/opt/mysql@8.4/bin
      fish_add_path /opt/homebrew/opt/gnu-tar/libexec/gnubin
      fish_add_path /run/current-system/sw/bin

      # Environment variables for compilation
      set -gx LDFLAGS "-L/opt/homebrew/opt/llvm/lib"
      set -gx CPPFLAGS "-I/opt/homebrew/opt/llvm/include"

      # .NET configuration
      set -gx DOTNET_ROOT /usr/local/share/dotnet
      fish_add_path $DOTNET_ROOT
      fish_add_path $HOME/.dotnet/tools

      # Mise activation (if available)
      if command -v mise > /dev/null 2>&1
        mise activate fish | source
      end
    '';

    interactiveShellInit = ''
      # Define variables for directories
      set -gx EMACSDIR $HOME/.emacs.d
      set -gx DOOMDIR $HOME/.doom.d
      set -gx DOOMLOCALDIR $HOME/.emacs.d/.local

      # PATH configuration
      fish_add_path $HOME/.pnpm-packages/bin
      fish_add_path $HOME/.pnpm-packages
      fish_add_path $HOME/.npm-packages/bin
      fish_add_path $HOME/bin
      fish_add_path $HOME/.local/share/bin
      fish_add_path $HOME/.local/bin
      fish_add_path $HOME/.cargo/bin
      fish_add_path $EMACSDIR/bin
      fish_add_path "$HOME/Library/Application Support/Coursier/bin"
      fish_add_path "$HOME/.volta/bin"

      # Environment variables
      set -gx LC_ALL "en_US.UTF-8"
      set -gx ALTERNATE_EDITOR ""
      set -gx EDITOR nvim
      set -gx VISUAL "/opt/homebrew/bin/emacsclient -nc -s /var/folders/yh/5_g54kd572gd9vr8tbc4m6gh0000gn/T/emacs501/doom"

      # >>> conda initialize >>>
      # !! Contents within this block are managed by 'conda init' !!
      if test -f /opt/homebrew/Caskroom/miniforge/base/bin/conda
          eval /opt/homebrew/Caskroom/miniforge/base/bin/conda "shell.fish" "hook" | source
      else
          if test -f "/opt/homebrew/Caskroom/miniforge/base/etc/fish/conf.d/conda.fish"
              . "/opt/homebrew/Caskroom/miniforge/base/etc/fish/conf.d/conda.fish"
          else
              set -x PATH "/opt/homebrew/Caskroom/miniforge/base/bin" $PATH
          end
      end
      # <<< conda initialize <<<
    '';

    functions = {
      # Terminal Emacs function
      t = {
        body = ''/opt/homebrew/bin/emacsclient -nw -s /var/folders/yh/5_g54kd572gd9vr8tbc4m6gh0000gn/T/emacs501/doom $argv'';
        description = "Open file in terminal Emacs";
      };

      # GUI Emacs client function
      e = {
        body = ''/opt/homebrew/bin/emacsclient -nc -s /var/folders/yh/5_g54kd572gd9vr8tbc4m6gh0000gn/T/emacs501/doom $argv'';
        description = "Open file in GUI Emacs client";
      };

      # Start Emacs in background
      ee = {
        body = ''emacs & disown'';
        description = "Start Emacs in background";
      };

      # Nix shell shortcut
      shell = {
        body = ''nix-shell '<nixpkgs>' -A "$argv[1]"'';
        description = "Enter nix-shell for package";
      };
    };

    shellAbbrs = {
      # 1Password plugin aliases
      aws = "op plugin run -- aws";
      cachix = "op plugin run -- cachix";
      # gh = "op plugin run -- gh";
      # glab = "op plugin run -- glab";

      # Utility aliases
      search = "rg -p --glob '!node_modules/*'";
      diff = "difft";

      # Terminal and editor shortcuts
      tg = "$EDITOR $HOME/.config/ghostty/config";
      edd = "emacs --daemon=doom";
      pke = "pkill -9 Emacs";
      nf = "nvim ~/.config/fish/config.fish";
      gd = "ghostty +show-config --default --docs";

      # Git shortcuts
      gp = "git fetch --all -p; git pull; git submodule update --recursive";

      # Doom Emacs shortcuts
      ds = "doom sync --aot --gc -j (nproc)";
      dup = "doom sync -u --aot --gc -j (nproc)";

      # Nix shortcuts
      nb = "pushd $HOME/nixos-config >/dev/null; nix run .#build; popd >/dev/null";
      ns = "pushd $HOME/nixos-config >/dev/null; nix run .#build-switch; popd >/dev/null";
    };
  };
}
