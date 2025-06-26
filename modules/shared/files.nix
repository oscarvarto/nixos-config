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

  # NOTE: nix-cleanup script is now managed via stow (nix-scripts package)
  # Run: cd ~/nixos-config/stow && stow nix-scripts

  # NOTE: Nix cleanup scripts are now managed via stow (nix-scripts package)
  # Run: cd ~/nixos-config/stow && stow nix-scripts

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

  # NOTE: Ghostty configuration scripts are now managed via stow (nix-scripts package)
  # Run: cd ~/nixos-config/stow && stow nix-scripts
}

