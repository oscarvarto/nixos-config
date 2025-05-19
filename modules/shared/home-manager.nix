{ config, pkgs, lib, ... }:

let name = "Oscar Vargas Torres";
    user = "oscarvarto";
    email = "contact@oscarvarto.mx";
in
{
  # Shared shell configuration
  zsh = {
    enable = true;
    autocd = false;
    cdpath = [ "~/.local/share/src" ];
    plugins = [
    ];
    initContent = lib.mkBefore ''
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
      alias pke="pkill -9 Emacs"
      alias search=rg -p --glob '!node_modules/*' $@

      alias nz="nvim ~/.zshrc"
      alias gd="ghostty +show-config --default --docs"

      nb() {
        pushd $HOME/nixos-config >/dev/null
        nix run .#build
        popd >/dev/null
      }

      ns() {
        pushd $HOME/nixos-config >/dev/null
        nix run .#build-switch
        popd >/dev/null
      }

      export ALTERNATE_EDITOR=""
      export EDITOR="nvim"
      export VISUAL="/opt/homebrew/bin/emacsclient -nc"

      t() {
         # nvim "$@"
         /opt/homebrew/bin/emacsclient -nw "$@"
      }

      ec() {
         /opt/homebrew/bin/emacsclient -nc "$@"
      }

      e() {
         emacs & disown
      }

      # nix shortcuts
      shell() {
          nix-shell '<nixpkgs>' -A "$1"
      }

      # common git alias
      alias gp="git fetch --all -p; git pull; git submodule update --recursive"

      # Doom update alias
      alias ds="doom sync --aot --gc -j $(nproc)"
      alias dup="doom upgrade; doom sync --aot --gc -j $(nproc)"

      # pnpm is a javascript package manager
      alias pn=pnpm
      alias px=pnpx

      # Use difftastic, syntax-aware diffing
      alias diff=difft

      # Always color ls and group directories
      alias ls='ls --color=auto'

      # Parinfer CLI functions for S-expression handling
      
      # Check if parinfer-cli is available
      if ! command -v parinfer-cli >/dev/null 2>&1; then
        echo "Warning: parinfer-cli not found. Please install it with 'volta install parinfer-cli'" >&2
      fi

      # Function to check S-expression balance
      check-sexp() {
        local mode="paren"
        local file=""
        local silent=0

        # Parse options
        while [[ $# -gt 0 ]]; do
          case $1 in
            -m|--mode)
              mode="$2"
              shift 2
              ;;
            -s|--silent)
              silent=1
              shift
              ;;
            *)
              file="$1"
              shift
              ;;
          esac
        done

        # Validate mode
        if [[ "$mode" != "paren" && "$mode" != "indent" ]]; then
          echo "Error: Invalid mode. Use 'paren' or 'indent'" >&2
          return 1
        fi

        # Function to process input
        process_input() {
          if [[ $silent -eq 1 ]]; then
            parinfer-cli -m "$mode" > /dev/null 2>&1
          else
            parinfer-cli -m "$mode" 2>&1
          fi
          return $?
        }

        # Handle input
        if [[ -n "$file" ]]; then
          if [[ ! -f "$file" ]]; then
            echo "Error: File not found: $file" >&2
            return 1
          fi
          if [[ $silent -eq 1 ]]; then
            echo "Checking S-expression balance in $file..."
          fi
          cat "$file" | process_input
        else
          if [[ -t 0 ]]; then
            echo "Error: No input provided. Either provide a file or pipe content" >&2
            echo "Usage: check-sexp [-m|--mode <paren|indent>] [-s|--silent] [file]" >&2
            return 1
          fi
          process_input
        fi
      }

      # Function to format S-expressions
      format-sexp() {
        local mode="paren"
        local infile=""
        local outfile=""

        # Parse options
        while [[ $# -gt 0 ]]; do
          case $1 in
            -m|--mode)
              mode="$2"
              shift 2
              ;;
            -o|--output)
              outfile="$2"
              shift 2
              ;;
            *)
              infile="$1"
              shift
              ;;
          esac
        done

        # Validate mode
        if [[ "$mode" != "paren" && "$mode" != "indent" ]]; then
          echo "Error: Invalid mode. Use 'paren' or 'indent'" >&2
          return 1
        fi

        # Function to process and output
        process_and_output() {
          if [[ -n "$outfile" ]]; then
            parinfer-cli -m "$mode" > "$outfile"
          else
            parinfer-cli -m "$mode"
          fi
        }

        # Handle input
        if [[ -n "$infile" ]]; then
          if [[ ! -f "$infile" ]]; then
            echo "Error: File not found: $infile" >&2
            return 1
          fi
          cat "$infile" | process_and_output
        else
          if [[ -t 0 ]]; then
            echo "Error: No input provided. Either provide a file or pipe content" >&2
            echo "Usage: format-sexp [-m|--mode <paren|indent>] [-o|--output <file>] [input-file]" >&2
            return 1
          fi
          process_and_output
        fi
      }

      # Function specifically for elisp files
      format-elisp() {
        local file="$1"
        local outfile="$2"

        if [[ -n "$file" ]]; then
          if [[ ! -f "$file" ]]; then
            echo "Error: File not found: $file" >&2
            return 1
          fi
          if [[ -n "$outfile" ]]; then
            format-sexp -m paren -o "$outfile" "$file"
          else
            format-sexp -m paren "$file"
          fi
        else
          if [[ -t 0 ]]; then
            echo "Error: No input provided. Either provide a file or pipe content" >&2
            echo "Usage: format-elisp <input-file> [output-file]" >&2
            return 1
          fi
          format-sexp -m paren
        fi
      }

      # Helpful aliases
      alias check-elisp='check-sexp -m paren'
      alias validate-sexp='check-sexp -s'
      alias pf='format-sexp'        # Quick format with default settings
      alias pfe='format-elisp'      # Quick format specifically for elisp
    '';
  };

  broot = {
    enable = true;
    enableNushellIntegration = true;
    enableZshIntegration = true;
  };

  direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
  };

  fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  git = {
    enable = true;
    ignores = [ "*.swp" ];
    userName = name;
    userEmail = email;
    lfs = {
      enable = true;
    };
    extraConfig = {
      init.defaultBranch = "main";
      core = {
	    editor = "nvim";
        autocrlf = "input";
      };
      commit.gpgsign = false;
      pull.rebase = true;
      rebase.autoStash = true;
    };
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

  starship = {
    enable = true;
    enableZshIntegration = true;
    enableNushellIntegration = true;
    settings = fromTOML(builtins.readFile ./config/starship.toml); 
  };

  zoxide = {
    enable = true;
    enableZshIntegration = true;
  };
}
