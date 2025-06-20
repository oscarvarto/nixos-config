{ pkgs, config, ... }:

let
  # Get user from the nixos-config function to avoid absolute paths
  user = config.users.users.oscarvarto.name or "oscarvarto";
in
{
  # mbsync configuration for retrieving mail
  ".mbsyncrc" = {
    text = ''
      # Account Information for oscarvarto@fastmail.com
      IMAPAccount oscarvarto-fastmail
      # Address to connect to
      Host imap.fastmail.com
      User oscarvarto@fastmail.com
      PassCmd "pass oscarvarto@protonmail/fastmail-mbsync"
      AuthMechs LOGIN
      TLSType IMAPS
      # CertificateFile /etc/ssl/cert.pem
 
      # Local and remote storage
      IMAPStore oscarvarto-fastmail-remote
      Account oscarvarto-fastmail
 
      # Main local storage for account, used to query server for subfolders
      MaildirStore oscarvarto-fastmail-local
      Path ~/Maildir/oscarvarto-fastmail/
      Inbox ~/Maildir/oscarvarto-fastmail/INBOX
      Subfolders Verbatim
 
      # Specify connection
      Channel oscarvarto-fastmail-inbox
      Far :oscarvarto-fastmail-remote:
      Near :oscarvarto-fastmail-local:
      Patterns "INBOX"
      Create Both
      Expunge Both
      SyncState *
 
      # Channel for Sent folder
      Channel oscarvarto-fastmail-sent
      Far :oscarvarto-fastmail-remote:"Sent"
      Near :oscarvarto-fastmail-local:Sent
      Create Both
      Expunge Both
      SyncState *
 
      # Channel for Drafts folder
      Channel oscarvarto-fastmail-drafts
      Far :oscarvarto-fastmail-remote:"Drafts"
      Near :oscarvarto-fastmail-local:Drafts
      Create Both
      Expunge Both
      SyncState *
 
      # Channel for Trash folder
      Channel oscarvarto-fastmail-trash
      Far :oscarvarto-fastmail-remote:"Trash"
      Near :oscarvarto-fastmail-local:Trash
      Create Both
      Expunge Both
      SyncState *
 
      # Channel for Archive folder
      Channel oscarvarto-fastmail-archive
      Far :oscarvarto-fastmail-remote:"Archive"
      Near :oscarvarto-fastmail-local:Archive
      Create Both
      Expunge Both
      SyncState *
 
      # Group all channels for synchronization
      Group oscarvarto-fastmail
      Channel oscarvarto-fastmail-inbox
      Channel oscarvarto-fastmail-sent
      Channel oscarvarto-fastmail-drafts
      Channel oscarvarto-fastmail-trash
      Channel oscarvarto-fastmail-archive
      # End group oscarvarto@fastmail.com
    '';
  };

  # Nix generation cleanup helper
  ".local/bin/nix-cleanup" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      # Nix cleanup script for managing generations and disk usage
      set -e

      # Colors
      RED='\033[0;31m'
      GREEN='\033[0;32m'
      YELLOW='\033[1;33m'
      BLUE='\033[0;34m'
      NC='\033[0m'

      show_help() {
        echo -e "$BLUE\U0001F9F9 Nix Generation Cleanup$NC"
        echo ""
        echo "Usage: nix-cleanup [OPTIONS] [COMMAND]"
        echo ""
        echo "Commands:"
        echo "  status           Show current disk usage and generations"
        echo "  quick            Quick cleanup (keep last 5 generations)"
        echo "  aggressive       Aggressive cleanup (keep last 3 generations)"
        echo "  minimal          Minimal cleanup (keep last 10 generations)"
        echo "  gc-only          Only run garbage collection"
        echo "  custom <N>       Keep last N generations"
        echo ""
        echo "Options:"
        echo "  -h, --help       Show this help message"
        echo "  -n, --dry-run    Show what would be cleaned without doing it"
        echo "  -f, --force      Skip confirmation prompts"
        echo "  -v, --verbose    Show detailed output"
        echo ""
        echo "Examples:"
        echo "  nix-cleanup status"
        echo "  nix-cleanup quick"
        echo "  nix-cleanup --dry-run aggressive"
        echo "  nix-cleanup custom 7"
      }

      show_status() {
        echo -e "$BLUE\U0001F4CA Nix Store Status$NC"
        echo -e "$YELLOW├─ Store size:$NC $(du -sh /nix/store 2>/dev/null | cut -f1 || echo 'Unknown')"
        
        echo -e "$YELLOW├─ User generations:$NC"
        nix-env --list-generations | tail -10 | while read line; do
          echo -e "$YELLOW│  $line$NC"
        done
        
        # Try to show system generations (may fail on some systems)
        if [ -r "/nix/var/nix/profiles/system" ]; then
          echo -e "$YELLOW├─ System generations (last 5):$NC"
          nix-env --list-generations --profile /nix/var/nix/profiles/system 2>/dev/null | tail -5 | while read line; do
            echo -e "$YELLOW│  $line$NC"
          done || echo -e "$YELLOW│  Cannot access system generations$NC"
        fi
        
        echo -e "$YELLOW└─ Garbage collection preview:$NC"
        local gc_info=$(nix store gc --dry-run 2>/dev/null | wc -l || echo "0")
        echo -e "$YELLOW   $gc_info store paths would be deleted$NC"
      }

      cleanup_generations() {
        local keep_count=''${1}
        local dry_run=''${2}
        local force=''${3}
        
        echo -e "$BLUE\U0001F9F9 Cleaning up generations (keeping last $keep_count)$NC"
        
        # Get current generation count
        local total_gens=$(nix-env --list-generations | wc -l)
        local to_delete=$((total_gens - keep_count))
        
        if [ $to_delete -le 0 ]; then
          echo -e "$GREEN\U2705 No generations to clean (have $total_gens, keeping $keep_count)$NC"
          return 0
        fi
        
        echo -e "$YELLOW\U1F4CB Will delete $to_delete old generations$NC"
        
        if [ "$dry_run" = true ]; then
          echo -e "$BLUE\U1F4AD DRY RUN: Would delete the oldest $to_delete generations$NC"
          nix-env --list-generations | head -n $to_delete | while read line; do
            echo -e "$RED│  Would delete: $line$NC"
          done
          return 0
        fi
        
        if [ "$force" != true ]; then
          echo -e "$YELLOW\U26A0 This will delete $to_delete old generations$NC"
          read -p "Continue? (y/N): " -n 1 -r
          echo ""
          if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo -e "$YELLOW\U1F6AB Operation cancelled$NC"
            return 0
          fi
        fi
        
        # Delete old generations
        local gens_to_delete=$(nix-env --list-generations | head -n $to_delete | awk '{print $1}' | tr '\n' ' ')
        if [ -n "$gens_to_delete" ]; then
          echo -e "$BLUE\U1F5D1 Deleting generations: $gens_to_delete$NC"
          nix-env --delete-generations $gens_to_delete
          echo -e "$GREEN\U2705 Deleted $to_delete generations$NC"
        fi
      }

      run_gc() {
        local dry_run=''${1}
        local verbose=''${2}
        
        echo -e "$BLUE\U1F5D1 Running garbage collection$NC"
        
        local before_size=$(du -sb /nix/store 2>/dev/null | cut -f1 || echo "0")
        
        if [ "$dry_run" = true ]; then
          echo -e "$BLUE\U1F4AD DRY RUN: Garbage collection preview$NC"
          nix store gc --dry-run
        else
          if [ "$verbose" = true ]; then
            nix store gc
          else
            nix store gc > /dev/null 2>&1
          fi
          
          local after_size=$(du -sb /nix/store 2>/dev/null | cut -f1 || echo "0")
          local freed=$((before_size - after_size))
          local freed_mb=$((freed / 1024 / 1024))
          
          echo -e "$GREEN\U2705 Garbage collection complete$NC"
          echo -e "$GREEN\U1F4BE Freed: ''${freed_mb}MB$NC"
        fi
      }

      # Parse arguments
      DRY_RUN=false
      FORCE=false
      VERBOSE=false
      COMMAND=""
      CUSTOM_COUNT=""

      while [[ $# -gt 0 ]]; do
        case $1 in
          -h|--help)
            show_help
            exit 0
            ;;
          -n|--dry-run)
            DRY_RUN=true
            shift
            ;;
          -f|--force)
            FORCE=true
            shift
            ;;
          -v|--verbose)
            VERBOSE=true
            shift
            ;;
          status|quick|aggressive|minimal|gc-only)
            COMMAND="$1"
            shift
            ;;
          custom)
            COMMAND="custom"
            shift
            if [[ $# -gt 0 && $1 =~ ^[0-9]+$ ]]; then
              CUSTOM_COUNT="$1"
              shift
            else
              echo -e "$RED\U274C custom command requires a number$NC" >&2
              show_help
              exit 1
            fi
            ;;
          -*)
            echo -e "$RED\U274C Unknown option: $1$NC" >&2
            show_help
            exit 1
            ;;
          *)
            echo -e "$RED\U274C Unknown command: $1$NC" >&2
            show_help
            exit 1
            ;;
        esac
      done

      # Default to status if no command given
      if [ -z "$COMMAND" ]; then
        COMMAND="status"
      fi

      case "$COMMAND" in
        status)
          show_status
          ;;
        quick)
          cleanup_generations 5 "$DRY_RUN" "$FORCE"
          [ "$DRY_RUN" != true ] && run_gc "$DRY_RUN" "$VERBOSE"
          ;;
        aggressive)
          cleanup_generations 3 "$DRY_RUN" "$FORCE"
          [ "$DRY_RUN" != true ] && run_gc "$DRY_RUN" "$VERBOSE"
          ;;
        minimal)
          cleanup_generations 10 "$DRY_RUN" "$FORCE"
          [ "$DRY_RUN" != true ] && run_gc "$DRY_RUN" "$VERBOSE"
          ;;
        gc-only)
          run_gc "$DRY_RUN" "$VERBOSE"
          ;;
        custom)
          cleanup_generations "$CUSTOM_COUNT" "$DRY_RUN" "$FORCE"
          [ "$DRY_RUN" != true ] && run_gc "$DRY_RUN" "$VERBOSE"
          ;;
      esac
    '';
  };

  # Quick aliases for common cleanup operations
  ".local/bin/nix-quick-clean" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      # Quick cleanup: keep last 5 generations + GC
      nix-cleanup --force quick
    '';
  };

  # Auto-cleanup script that can be run periodically
  ".local/bin/nix-auto-cleanup" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      # Automatic cleanup for regular maintenance
      # Designed to be safe for automated execution

      # Colors
      GREEN='\033[0;32m'
      YELLOW='\033[1;33m'
      BLUE='\033[0;34m'
      NC='\033[0m'

      echo -e "$BLUE\U1F916 Nix Auto-Cleanup$NC"

      # Check if store is getting large (>10GB)
      STORE_SIZE_GB=$(du -sb /nix/store 2>/dev/null | awk '{print int($1/1024/1024/1024)}' || echo "0")

      if [ "$STORE_SIZE_GB" -gt 10 ]; then
        echo -e "$YELLOW\U26A0 Store size is ''${STORE_SIZE_GB}GB, running cleanup$NC"
        
        # Conservative cleanup: keep last 7 generations
        GEN_COUNT=$(nix-env --list-generations | wc -l)
        if [ "$GEN_COUNT" -gt 7 ]; then
          echo -e "$BLUE\U1F9F9 Cleaning old generations (keeping last 7)$NC"
          OLD_GENS=$(nix-env --list-generations | head -n $((GEN_COUNT - 7)) | awk '{print $1}' | tr '\n' ' ')
          [ -n "$OLD_GENS" ] && nix-env --delete-generations $OLD_GENS
        fi
        
        # Run garbage collection
        echo -e "$BLUE\U1F5D1 Running garbage collection$NC"
        nix store gc > /dev/null 2>&1
        
        NEW_SIZE_GB=$(du -sb /nix/store 2>/dev/null | awk '{print int($1/1024/1024/1024)}' || echo "0")
        FREED_GB=$((STORE_SIZE_GB - NEW_SIZE_GB))
        echo -e "$GREEN\U2705 Cleanup complete. Freed ''${FREED_GB}GB (''${NEW_SIZE_GB}GB remaining)$NC"
      else
        echo -e "$GREEN\U2705 Store size is ''${STORE_SIZE_GB}GB, no cleanup needed$NC"
      fi
    '';
  };

  # Ghostty base configuration (managed by Nix)
  ".config/ghostty/config" = {
    text = ''
      # Ghostty Configuration - Base settings managed by Nix
      # Override settings in ~/.config/ghostty/overrides.conf for quick changes
 
      # Shell configuration (default fish, can be overridden)
      # Note: Default shell is set below, override with ghostty-config shell <shell-name>
      shell-integration = fish
      shell-integration-features = no-cursor,sudo,title

      # Default font (can be overridden)
      # font-family = PragmataPro Mono Liga
      font-family = MonoLisaVariable Nerd Font
      font-size = 18
 
      # Default theme (can be overridden)
      theme = dracula

      # Default shell (can be overridden)
      command = /opt/homebrew/bin/fish -i -l
      initial-command = /opt/homebrew/bin/fish -i -l

      # Window and appearance settings
      split-divider-color = green
      window-save-state = always
      cursor-style = block
      cursor-color = "#D9905A"
      auto-update-channel = tip
      quit-after-last-window-closed = true

      # macOS specific settings
      macos-option-as-alt = left

      # Key bindings
      keybind = global:super+ctrl+grave_accent=toggle_quick_terminal

      # Include user overrides LAST so they take precedence
      config-file = ~/.config/ghostty/overrides.conf
    '';
  };

  # Note: overrides.conf is NOT managed by Nix - it's created and managed by the helper scripts
  # This allows users to edit it directly without rebuilding the Nix configuration

  # Ghostty configuration helper scripts
  ".local/bin/ghostty-config" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      # Ghostty configuration helper for quick changes
      set -e

      # Colors
      RED='\e[0;31m'
      GREEN='\e[0;32m'
      YELLOW='\e[1;33m'
      BLUE='\e[0;34m'
      NC='\e[0m'

      OVERRIDES_FILE="''${HOME}/.config/ghostty/overrides.conf"

      show_help() {
        echo -e "''${BLUE}\U1F47B Ghostty Configuration Helper''${NC}"
        echo ""
        echo "Usage: ghostty-config [COMMAND] [OPTIONS]"
        echo ""
        echo "Commands:"
        echo "  font <font-name> [<size>]   Set font (and optional size)"
        echo "  theme <theme-name>         Set theme"
        echo "  shell <shell-name>         Set shell for new terminals"
        echo "  opacity <value>          Set background opacity (0.0-1.0)"
        echo "  reset                   Reset overrides to defaults"
        echo "  list                    Show available options"
        echo "  current                 Show current override settings"
        echo "  edit                    Open overrides file in editor"
        echo ""
        echo "Font Examples:"
        echo "  ghostty-config font 'MonoLisaVariable Nerd Font' 14"
        echo "  ghostty-config font 'PragmataPro Mono Liga' 18"
        echo "  ghostty-config font 'JetBrains Mono'"
        echo ""
        echo "Theme Examples:"
        echo "  ghostty-config theme dracula"
        echo "  ghostty-config theme BlulocoLight"
        echo "  ghostty-config theme nord"
        echo ""
        echo "Shell Examples:"
        echo "  ghostty-config shell fish"
        echo "  ghostty-config shell zsh"
        echo "  ghostty-config shell bash"
        echo "  ghostty-config shell nushell"
        echo "  ghostty-config shell pwsh"
        echo ""
        echo "Other Examples:"
        echo "  ghostty-config opacity 0.9"
        echo "  ghostty-config reset"
      }

      ensure_config_dir() {
        mkdir -p "$(dirname "''${OVERRIDES_FILE}")"
        if [ ! -f "''${OVERRIDES_FILE}" ]; then
          touch "''${OVERRIDES_FILE}"
        fi
      }

      update_setting() {
        local key="''${1}"
        local value="''${2}"

        ensure_config_dir

        # Remove existing setting if it exists
        sed -i.bak "/^''${key}[[:space:]]*=/d" "''${OVERRIDES_FILE}"

        # Add new setting
        echo "''${key} = ''${value}" >> "''${OVERRIDES_FILE}"

        # Clean up backup file
        rm -f "''${OVERRIDES_FILE}.bak"

        echo -e "''${GREEN}\U2705 Updated ''${key} = ''${value}''${NC}"
      }

      get_shell_path() {
        local shell_name="$1"
        case "$shell_name" in
          fish)
            echo "/opt/homebrew/bin/fish -i"
            ;;
          zsh)
            echo "/bin/zsh -i"
            ;;
          bash)
            echo "/bin/bash -i"
            ;;
          nushell|nu)
            echo "/Users/oscarvarto/.nix-profile/bin/nu -i"
            ;;
          pwsh|powershell)
            echo "/opt/homebrew/bin/pwsh -i"
            ;;
          *)
            echo ""
            ;;
        esac
      }

      set_shell() {
        local shell_name="''${1}"
        local shell_path=$(get_shell_path "''${shell_name}")

        if [ -z "''${shell_path}" ]; then
          echo -e "''${RED}\U274C Unknown shell: ''${shell_name}''${NC}" >&2
          echo -e "''${YELLOW}Available shells: fish, zsh, bash, nushell, pwsh''${NC}" >&2
          return 1
        fi

        # Check if shell exists
        local shell_binary=$(echo "''${shell_path}" | awk '{print $1}')
        if [ ! -x "''${shell_binary}" ]; then
          echo -e "''${RED}\U274C Shell not found: ''${shell_binary}''${NC}" >&2
          echo -e "''${YELLOW}Make sure ''${shell_name} is installed''${NC}" >&2
          return 1
        fi

        # Update both command and initial-command
        update_setting "command" "''${shell_path}"
        update_setting "initial-command" "''${shell_path}"

        echo -e "''${GREEN}\U1F41A Shell set to ''${shell_name}''${NC}"
      }

      remove_setting() {
        local key="''${1}"

        if [ -f "''${OVERRIDES_FILE}" ]; then
          sed -i.bak "/^''${key}[[:space:]]*=/d" "''${OVERRIDES_FILE}"
          rm -f "''${OVERRIDES_FILE}.bak"
          echo -e "''${GREEN}\U2705 Removed ''${key} override''${NC}"
        fi
      }

      remove_setting_old() {
        local key="''${1}"

        if [ -f "''${OVERRIDES_FILE}" ]; then
          sed -i.bak "/^''${key}[[:space:]]*=/d" "''${OVERRIDES_FILE}"
          rm -f "''${OVERRIDES_FILE}.bak"
          echo -e "''${GREEN}\U2705 Removed ''${key} override''${NC}"
        fi
      }

      restart_ghostty() {
        echo -e "''${YELLOW}\U1F504 Restarting Ghostty to apply changes...''${NC}"
        # Kill existing Ghostty processes
        pkill -f Ghostty || true
        sleep 1
        # Start Ghostty in background
        open -a Ghostty &> /dev/null &
        echo -e "''${GREEN}\U2705 Ghostty restarted''${NC}"
      }

      case "''${1:-help}" in
        font)
          if [ -z "''${2}" ]; then
            echo -e "''${RED}\U274C Font name required''${NC}" >&2
            show_help
            exit 1
          fi
          # Reset font family first, then set new one (two lines for proper reset)
          ensure_config_dir
          # Remove existing font-family lines
          sed -i.bak "/^font-family[[:space:]]*=/d" "''${OVERRIDES_FILE}"
          rm -f "''${OVERRIDES_FILE}.bak"
          # Add reset line first (empty quoted string as per Ghostty docs)
          echo 'font-family = ""' >> "''${OVERRIDES_FILE}"
          echo -e "''${GREEN}\U2705 Added font-family reset''${NC}"
          # Then add new font setting
          echo "font-family = \"''${2}\"" >> "''${OVERRIDES_FILE}"
          echo -e "''${GREEN}\U2705 Updated font-family = ''${2}''${NC}"
          [ -n "''${3}" ] && update_setting "font-size" "''${3}"
          restart_ghostty
          ;;
        theme)
          if [ -z "''${2}" ]; then
            echo -e "''${RED}\U274C Theme name required''${NC}" >&2
            show_help
            exit 1
          fi
          update_setting "theme" "''${2}"
          restart_ghostty
          ;;
        opacity)
          if [ -z "''${2}" ]; then
            echo -e "''${RED}\U274C Opacity value required''${NC}" >&2
            show_help
            exit 1
          fi
          update_setting "background-opacity" "''${2}"
          restart_ghostty
          ;;
        shell)
          if [ -z "''${2}" ]; then
            echo -e "''${RED}\U274C Shell name required''${NC}" >&2
            show_help
            exit 1
          fi
          set_shell "''${2}"
          restart_ghostty
          ;;
        reset)
          echo -e "''${YELLOW}\U1F504 Resetting overrides...''${NC}"
          echo "# Ghostty Runtime Overrides" > "''${OVERRIDES_FILE}"
          echo "# Edit this file for quick changes without Nix rebuild" >> "''${OVERRIDES_FILE}"
          echo "# These settings override the base config" >> "''${OVERRIDES_FILE}"
          echo "" >> "''${OVERRIDES_FILE}"
          echo -e "''${GREEN}\U2705 Overrides reset to defaults''${NC}"
          restart_ghostty
          ;;
        list)
          echo -e "''${BLUE}\U1F4CB Available Options:''${NC}"
          echo ""
          echo -e "''${YELLOW}Fonts:''${NC}"
          echo "  - MonoLisaVariable Nerd Font"
          echo "  - PragmataPro Mono Liga"
          echo "  - JetBrains Mono"
          echo "  - SF Mono"
          echo "  - Iosevka"
          echo ""
          echo -e "''${YELLOW}Themes:''${NC}"
          echo "  - dracula"
          echo "  - BlulocoLight"
          echo "  - nord"
          echo "  - github_light"
          echo "  - tokyo-night"
          echo "  - onedark"
          echo "  - gruvbox"
          echo ""
          echo -e "''${YELLOW}Shells:''${NC}"
          echo "  - fish (default)"
          echo "  - zsh"
          echo "  - bash"
          echo "  - nushell (nu)"
          echo "  - pwsh (powershell)"
          echo ""
          echo -e "''${YELLOW}Font Sizes:''${NC}"
          echo "  - 12, 14, 16, 18, 20, 24"
          echo ""
          echo -e "''${YELLOW}Opacity:''${NC}"
          echo "  - 0.8 (very transparent)"
          echo "  - 0.9 (semi-transparent)"
          echo "  - 0.95 (slightly transparent)"
          echo "  - 1.0 (opaque)"
          ;;
        current)
          echo -e "''${BLUE}\U1F4CB Current Override Settings:''${NC}"
          if [ -f "''${OVERRIDES_FILE}" ] && [ -s "''${OVERRIDES_FILE}" ]; then
            grep -v '^#' "''${OVERRIDES_FILE}" | grep -v '^[[:space:]]*$' || echo "No active overrides"
          else
            echo "No overrides file or empty"
          fi
          ;;
        edit)
          ''${EDITOR:-nano} "''${OVERRIDES_FILE}"
          echo -e "''${YELLOW}\U1F504 Restart Ghostty to apply manual changes''${NC}"
          ;;
        help|--help|-h)
          show_help
          ;;
        *)
          echo -e "''${RED}\U274C Unknown command: ''${1}''${NC}" >&2
          show_help
          exit 1
          ;;
      esac
    '';
  };

  # Quick font switcher aliases
  ".local/bin/ghostty-font-monolisa" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      ghostty-config font "MonoLisaVariable Nerd Font" 14
    '';
  };

  ".local/bin/ghostty-font-pragmata" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      ghostty-config font "PragmataPro Mono Liga" 18
    '';
  };

  # Quick theme switcher aliases
  ".local/bin/ghostty-theme-dark" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      ghostty-config theme dracula
    '';
  };

  ".local/bin/ghostty-theme-light" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      ghostty-config theme BlulocoLight
    '';
  };

  # Quick shell switcher aliases
  ".local/bin/ghostty-shell-fish" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      ghostty-config shell fish
    '';
  };

  ".local/bin/ghostty-shell-zsh" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      ghostty-config shell zsh
    '';
  };

  ".local/bin/ghostty-shell-bash" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      ghostty-config shell bash
    '';
  };

  ".local/bin/ghostty-shell-nushell" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      ghostty-config shell nushell
    '';
  };

  ".local/bin/ghostty-shell-pwsh" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      ghostty-config shell pwsh
    '';
  };
}

