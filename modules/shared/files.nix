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
    text = ''#!/usr/bin/env bash
      # Nix cleanup script for managing generations and disk usage
      set -e
      
      # Colors
      RED='\033[0;31m'
      GREEN='\033[0;32m'
      YELLOW='\033[1;33m'
      BLUE='\033[0;34m'
      NC='\033[0m'
      
      show_help() {
        echo -e "''${BLUE}🧹 Nix Generation Cleanup''${NC}"
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
        echo -e "''${BLUE}📊 Nix Store Status''${NC}"
        echo -e "''${YELLOW}├─ Store size:''${NC} $(du -sh /nix/store 2>/dev/null | cut -f1 || echo 'Unknown')"
        
        echo -e "''${YELLOW}├─ User generations:''${NC}"
        nix-env --list-generations | tail -10 | while read line; do
          echo -e "''${YELLOW}│  ''${line}''${NC}"
        done
        
        # Try to show system generations (may fail on some systems)
        if [ -r "/nix/var/nix/profiles/system" ]; then
          echo -e "''${YELLOW}├─ System generations (last 5):''${NC}"
          nix-env --list-generations --profile /nix/var/nix/profiles/system 2>/dev/null | tail -5 | while read line; do
            echo -e "''${YELLOW}│  ''${line}''${NC}"
          done || echo -e "''${YELLOW}│  Cannot access system generations''${NC}"
        fi
        
        echo -e "''${YELLOW}└─ Garbage collection preview:''${NC}"
        local gc_info=$(nix store gc --dry-run 2>/dev/null | wc -l || echo "0")
        echo -e "''${YELLOW}   ''${gc_info} store paths would be deleted''${NC}"
      }
      
      cleanup_generations() {
        local keep_count=''${1}
        local dry_run=''${2}
        local force=''${3}
        
        echo -e "''${BLUE}🧹 Cleaning up generations (keeping last ''${keep_count})''${NC}"
        
        # Get current generation count
        local total_gens=$(nix-env --list-generations | wc -l)
        local to_delete=$((total_gens - keep_count))
        
        if [ ''${to_delete} -le 0 ]; then
          echo -e "''${GREEN}✅ No generations to clean (have ''${total_gens}, keeping ''${keep_count})''${NC}"
          return 0
        fi
        
        echo -e "''${YELLOW}📋 Will delete ''${to_delete} old generations''${NC}"
        
        if [ "''${dry_run}" = true ]; then
          echo -e "''${BLUE}💭 DRY RUN: Would delete the oldest ''${to_delete} generations''${NC}"
          nix-env --list-generations | head -n ''${to_delete} | while read line; do
            echo -e "''${RED}│  Would delete: ''${line}''${NC}"
          done
          return 0
        fi
        
        if [ "''${force}" != true ]; then
          echo -e "''${YELLOW}⚠️  This will delete ''${to_delete} old generations''${NC}"
          read -p "Continue? (y/N): " -n 1 -r
          echo ""
          if [[ ! ''${REPLY} =~ ^[Yy]$ ]]; then
            echo -e "''${YELLOW}🚫 Operation cancelled''${NC}"
            return 0
          fi
        fi
        
        # Delete old generations
        local gens_to_delete=$(nix-env --list-generations | head -n ''${to_delete} | awk '{print $1}' | tr '\n' ' ')
        if [ -n "''${gens_to_delete}" ]; then
          echo -e "''${BLUE}🗑️  Deleting generations: ''${gens_to_delete}''${NC}"
          nix-env --delete-generations ''${gens_to_delete}
          echo -e "''${GREEN}✅ Deleted ''${to_delete} generations''${NC}"
        fi
      }
      
      run_gc() {
        local dry_run=''${1}
        local verbose=''${2}
        
        echo -e "''${BLUE}🗑️  Running garbage collection''${NC}"
        
        local before_size=$(du -sb /nix/store 2>/dev/null | cut -f1 || echo "0")
        
        if [ "''${dry_run}" = true ]; then
          echo -e "''${BLUE}💭 DRY RUN: Garbage collection preview''${NC}"
          nix store gc --dry-run
        else
          if [ "''${verbose}" = true ]; then
            nix store gc
          else
            nix store gc > /dev/null 2>&1
          fi
          
          local after_size=$(du -sb /nix/store 2>/dev/null | cut -f1 || echo "0")
          local freed=$((before_size - after_size))
          local freed_mb=$((freed / 1024 / 1024))
          
          echo -e "''${GREEN}✅ Garbage collection complete''${NC}"
          echo -e "''${GREEN}💾 Freed: ''${freed_mb}MB''${NC}"
        fi
      }
      
      # Parse arguments
      DRY_RUN=false
      FORCE=false
      VERBOSE=false
      COMMAND=""
      CUSTOM_COUNT=""
      
      while [[ ''${#} -gt 0 ]]; do
        case ''${1} in
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
            COMMAND="''${1}"
            shift
            ;;
          custom)
            COMMAND="custom"
            shift
            if [[ ''${#} -gt 0 && ''${1} =~ ^[0-9]+$ ]]; then
              CUSTOM_COUNT="''${1}"
              shift
            else
              echo -e "''${RED}❌ custom command requires a number''${NC}" >&2
              show_help
              exit 1
            fi
            ;;
          -*)
            echo -e "''${RED}❌ Unknown option: ''${1}''${NC}" >&2
            show_help
            exit 1
            ;;
          *)
            echo -e "''${RED}❌ Unknown command: ''${1}''${NC}" >&2
            show_help
            exit 1
            ;;
        esac
      done
      
      # Default to status if no command given
      if [ -z "''${COMMAND}" ]; then
        COMMAND="status"
      fi
      
      case "''${COMMAND}" in
        status)
          show_status
          ;;
        quick)
          cleanup_generations 5 "''${DRY_RUN}" "''${FORCE}"
          [ "''${DRY_RUN}" != true ] && run_gc "''${DRY_RUN}" "''${VERBOSE}"
          ;;
        aggressive)
          cleanup_generations 3 "''${DRY_RUN}" "''${FORCE}"
          [ "''${DRY_RUN}" != true ] && run_gc "''${DRY_RUN}" "''${VERBOSE}"
          ;;
        minimal)
          cleanup_generations 10 "''${DRY_RUN}" "''${FORCE}"
          [ "''${DRY_RUN}" != true ] && run_gc "''${DRY_RUN}" "''${VERBOSE}"
          ;;
        gc-only)
          run_gc "''${DRY_RUN}" "''${VERBOSE}"
          ;;
        custom)
          cleanup_generations "''${CUSTOM_COUNT}" "''${DRY_RUN}" "''${FORCE}"
          [ "''${DRY_RUN}" != true ] && run_gc "''${DRY_RUN}" "''${VERBOSE}"
          ;;
      esac
    '';
  };

  # Quick aliases for common cleanup operations
  ".local/bin/nix-quick-clean" = {
    executable = true;
    text = ''#!/usr/bin/env bash
      # Quick cleanup: keep last 5 generations + GC
      nix-cleanup --force quick
    '';
  };

  # Auto-cleanup script that can be run periodically
  ".local/bin/nix-auto-cleanup" = {
    executable = true;
    text = ''#!/usr/bin/env bash
      # Automatic cleanup for regular maintenance
      # Designed to be safe for automated execution
      
      # Colors
      GREEN='\033[0;32m'
      YELLOW='\033[1;33m'
      BLUE='\033[0;34m'
      NC='\033[0m'
      
      echo -e "''${BLUE}🤖 Nix Auto-Cleanup''${NC}"
      
      # Check if store is getting large (>10GB)
      STORE_SIZE_GB=$(du -sb /nix/store 2>/dev/null | awk '{print int($1/1024/1024/1024)}' || echo "0")
      
      if [ "''${STORE_SIZE_GB}" -gt 10 ]; then
        echo -e "''${YELLOW}⚠️  Store size is ''${STORE_SIZE_GB}GB, running cleanup''${NC}"
        
        # Conservative cleanup: keep last 7 generations
        GEN_COUNT=$(nix-env --list-generations | wc -l)
        if [ "''${GEN_COUNT}" -gt 7 ]; then
          echo -e "''${BLUE}🧹 Cleaning old generations (keeping last 7)''${NC}"
          OLD_GENS=$(nix-env --list-generations | head -n $((GEN_COUNT - 7)) | awk '{print $1}' | tr '\n' ' ')
          [ -n "''${OLD_GENS}" ] && nix-env --delete-generations ''${OLD_GENS}
        fi
        
        # Run garbage collection
        echo -e "''${BLUE}🗑️  Running garbage collection''${NC}"
        nix store gc > /dev/null 2>&1
        
        NEW_SIZE_GB=$(du -sb /nix/store 2>/dev/null | awk '{print int($1/1024/1024/1024)}' || echo "0")
        FREED_GB=$((STORE_SIZE_GB - NEW_SIZE_GB))
        echo -e "''${GREEN}✅ Cleanup complete. Freed ''${FREED_GB}GB (''${NEW_SIZE_GB}GB remaining)''${NC}"
      else
        echo -e "''${GREEN}✅ Store size is ''${STORE_SIZE_GB}GB, no cleanup needed''${NC}"
      fi
    '';
  };

  # Ghostty base configuration (managed by Nix)
  ".config/ghostty/config" = {
    text = ''
      # Ghostty Configuration - Base settings managed by Nix
      # Override settings in ~/.config/ghostty/overrides.conf for quick changes
      
      # Shell configuration
      command = /opt/homebrew/bin/nu -i -l
      initial-command = /opt/homebrew/bin/nu -i -l
      shell-integration = fish
      shell-integration-features = no-cursor,sudo,title
      
      # Default font (can be overridden)
      font-family = PragmataPro Liga
      font-size = 18
      
      # Default theme (can be overridden)
      theme = dracula
      
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
      
      # Include user overrides (this file can be edited without Nix rebuild)
      config-file = ~/.config/ghostty/overrides.conf
    '';
  };

  # Note: overrides.conf is NOT managed by Nix - it's created and managed by the helper scripts
  # This allows users to edit it directly without rebuilding the Nix configuration

  # Ghostty configuration helper scripts
  ".local/bin/ghostty-config" = {
    executable = true;
    text = ''#!/usr/bin/env bash
      # Ghostty configuration helper for quick changes
      set -e
      
      # Colors
      RED='\033[0;31m'
      GREEN='\033[0;32m'
      YELLOW='\033[1;33m'
      BLUE='\033[0;34m'
      NC='\033[0m'
      
      OVERRIDES_FILE="''${HOME}/.config/ghostty/overrides.conf"
      
      show_help() {
        echo -e "''${BLUE}👻 Ghostty Configuration Helper''${NC}"
        echo ""
        echo "Usage: ghostty-config [COMMAND] [OPTIONS]"
        echo ""
        echo "Commands:"
        echo "  font <font-name> [<size>]   Set font (and optional size)"
        echo "  theme <theme-name>         Set theme"
        echo "  opacity <value>          Set background opacity (0.0-1.0)"
        echo "  reset                   Reset overrides to defaults"
        echo "  list                    Show available options"
        echo "  current                 Show current override settings"
        echo "  edit                    Open overrides file in editor"
        echo ""
        echo "Font Examples:"
        echo "  ghostty-config font 'MonoLisaVariable Nerd Font' 14"
        echo "  ghostty-config font 'PragmataPro Liga' 18"
        echo "  ghostty-config font 'JetBrains Mono'"
        echo ""
        echo "Theme Examples:"
        echo "  ghostty-config theme dracula"
        echo "  ghostty-config theme BlulocoLight"
        echo "  ghostty-config theme nord"
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
        
        echo -e "''${GREEN}✅ Updated ''${key} = ''${value}''${NC}"
      }
      
      remove_setting() {
        local key="''${1}"
        
        if [ -f "''${OVERRIDES_FILE}" ]; then
          sed -i.bak "/^''${key}[[:space:]]*=/d" "''${OVERRIDES_FILE}"
          rm -f "''${OVERRIDES_FILE}.bak"
          echo -e "''${GREEN}✅ Removed ''${key} override''${NC}"
        fi
      }
      
      restart_ghostty() {
        echo -e "''${YELLOW}🔄 Restarting Ghostty to apply changes...''${NC}"
        # Kill existing Ghostty processes
        pkill -f Ghostty || true
        sleep 1
        # Start Ghostty in background
        open -a Ghostty &> /dev/null &
        echo -e "''${GREEN}✅ Ghostty restarted''${NC}"
      }
      
      case "''${1:-help}" in
        font)
          if [ -z "''${2}" ]; then
            echo -e "''${RED}❌ Font name required''${NC}" >&2
            show_help
            exit 1
          fi
          update_setting "font-family" "''${2}"
          [ -n "''${3}" ] && update_setting "font-size" "''${3}"
          restart_ghostty
          ;;
        theme)
          if [ -z "''${2}" ]; then
            echo -e "''${RED}❌ Theme name required''${NC}" >&2
            show_help
            exit 1
          fi
          update_setting "theme" "''${2}"
          restart_ghostty
          ;;
        opacity)
          if [ -z "''${2}" ]; then
            echo -e "''${RED}❌ Opacity value required''${NC}" >&2
            show_help
            exit 1
          fi
          update_setting "background-opacity" "''${2}"
          restart_ghostty
          ;;
        reset)
          echo -e "''${YELLOW}🔄 Resetting overrides...''${NC}"
          echo "# Ghostty Runtime Overrides" > "''${OVERRIDES_FILE}"
          echo "# Edit this file for quick changes without Nix rebuild" >> "''${OVERRIDES_FILE}"
          echo "# These settings override the base config" >> "''${OVERRIDES_FILE}"
          echo "" >> "''${OVERRIDES_FILE}"
          echo -e "''${GREEN}✅ Overrides reset to defaults''${NC}"
          restart_ghostty
          ;;
        list)
          echo -e "''${BLUE}📋 Available Options:''${NC}"
          echo ""
          echo -e "''${YELLOW}Fonts:''${NC}"
          echo "  - MonoLisaVariable Nerd Font"
          echo "  - PragmataPro Liga"
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
          echo -e "''${BLUE}📋 Current Override Settings:''${NC}"
          if [ -f "''${OVERRIDES_FILE}" ] && [ -s "''${OVERRIDES_FILE}" ]; then
            grep -v '^#' "''${OVERRIDES_FILE}" | grep -v '^[[:space:]]*$' || echo "No active overrides"
          else
            echo "No overrides file or empty"
          fi
          ;;
        edit)
          ''${EDITOR:-nano} "''${OVERRIDES_FILE}"
          echo -e "''${YELLOW}🔄 Restart Ghostty to apply manual changes''${NC}"
          ;;
        help|--help|-h)
          show_help
          ;;
        *)
          echo -e "''${RED}❌ Unknown command: ''${1}''${NC}" >&2
          show_help
          exit 1
          ;;
      esac
    '';
  };

  # Quick font switcher aliases
  ".local/bin/ghostty-font-monolisa" = {
    executable = true;
    text = ''#!/usr/bin/env bash
      ghostty-config font "MonoLisaVariable Nerd Font" 14
    '';
  };

  ".local/bin/ghostty-font-pragmata" = {
    executable = true;
    text = ''#!/usr/bin/env bash
      ghostty-config font "PragmataPro Liga" 18
    '';
  };

  # Quick theme switcher aliases
  ".local/bin/ghostty-theme-dark" = {
    executable = true;
    text = ''#!/usr/bin/env bash
      ghostty-config theme dracula
    '';
  };

  ".local/bin/ghostty-theme-light" = {
    executable = true;
    text = ''#!/usr/bin/env bash
      ghostty-config theme BlulocoLight
    '';
  };
}

