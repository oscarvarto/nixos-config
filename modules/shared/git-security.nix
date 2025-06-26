{ config, pkgs, lib, user ? "oscarvarto", ... }:

{
  
  # Work-specific git configuration file (will be updated by script)
  home.file.".config/git/config-work" = {
    text = ''[user]
      name = Oscar Vargas Torres
      email = YOUR-WORK-EMAIL@company.com
    '';
  };
  
  # Default git configuration for personal projects
  home.file.".config/git/config-personal" = {
    text = ''[user]
      name = Oscar Vargas Torres
      email = contact@oscarvarto.mx
    '';
  };
  
  # Doom Emacs specific git configuration (with hooks)
  home.file.".config/git/config-doom" = {
    text = ''[user]
      name = Oscar Vargas Torres
      email = contact@oscarvarto.mx
[core]
      hooksPath = ~/.config/git/hooks
    '';
  };

  # Template files for secure development
  home.file.".config/git/templates/env.example" = {
    text = ''# Environment variables template
      # Copy this to .env and use 1Password CLI to populate secrets
      
      # Example API key - use: export API_KEY=$(op read "op://vault/item/field")
      API_KEY=op://vault/item/field
      
      # Example database URL - use: export DB_URL=$(op read "op://vault/db/url")
      DB_URL=op://vault/db/url
      
      # Example OAuth secret - use: export OAUTH_SECRET=$(op read "op://vault/oauth/secret")
      OAUTH_SECRET=op://vault/oauth/secret
      
      # Non-secret configuration
      NODE_ENV=development
      PORT=3000
    '';
  };
  
  home.file.".config/git/templates/README-secrets.md" = {
    text = ''# Secret Management with 1Password
      
      This project uses 1Password CLI for secure secret management across different shells.
      
      ## Setup
      
      1. Ensure 1Password CLI is installed (handled via Nix configuration)
      2. Sign in: `op signin`
      3. Verify access: `op vault list`
      
      ## Usage by Shell
      
      ### Bash/Zsh
      
      #### Environment Variables
      ```bash
      # Export secrets to environment
      export API_KEY=$(op read "op://vault/item/field")
      export DB_PASSWORD=$(op read "op://vault/database/password")
      export OAUTH_SECRET=$(op read "op://vault/oauth/secret")
      ```
      
      #### In Scripts
      ```bash
      #!/bin/bash
      # Get secret in script
      API_KEY=$(op read "op://vault/api/key")
      curl -H "Authorization: Bearer $API_KEY" https://api.example.com
      
      # Multiple secrets
      DB_USER=$(op read "op://vault/database/username")
      DB_PASS=$(op read "op://vault/database/password")
      psql "postgresql://$DB_USER:$DB_PASS@localhost/mydb"
      ```
      
      ### Fish Shell
      
      #### Environment Variables
      ```fish
      # Export secrets to environment
      set -gx API_KEY (op read "op://vault/item/field")
      set -gx DB_PASSWORD (op read "op://vault/database/password")
      set -gx OAUTH_SECRET (op read "op://vault/oauth/secret")
      ```
      
      #### In Scripts
      ```fish
      #!/usr/bin/env fish
      # Get secret in script
      set API_KEY (op read "op://vault/api/key")
      curl -H "Authorization: Bearer $API_KEY" https://api.example.com
      
      # Multiple secrets
      set DB_USER (op read "op://vault/database/username")
      set DB_PASS (op read "op://vault/database/password")
      psql "postgresql://$DB_USER:$DB_PASS@localhost/mydb"
      ```
      
      #### Fish Functions
      ```fish
      # Create a function for commonly used secrets
      function get_api_key
          op read "op://vault/api/key"
      end
      
      function setup_dev_env
          set -gx API_KEY (op read "op://vault/dev/api-key")
          set -gx DB_URL (op read "op://vault/dev/database-url")
          echo "Development environment configured"
      end
      ```
      
      ### Nushell
      
      #### Environment Variables
      ```nu
      # Export secrets to environment
      $env.API_KEY = (op read "op://vault/item/field")
      $env.DB_PASSWORD = (op read "op://vault/database/password")
      $env.OAUTH_SECRET = (op read "op://vault/oauth/secret")
      ```
      
      #### In Scripts
      ```nu
      #!/usr/bin/env nu
      # Get secret in script
      let api_key = (op read "op://vault/api/key")
      curl -H $"Authorization: Bearer ($api_key)" https://api.example.com
      
      # Multiple secrets with error handling
      try {
          let db_user = (op read "op://vault/database/username")
          let db_pass = (op read "op://vault/database/password")
          psql $"postgresql://($db_user):($db_pass)@localhost/mydb"
      } catch {
          echo "Failed to retrieve database credentials"
      }
      ```
      
      #### Nushell Functions
      ```nu
      # Create custom commands for secret management
      def get-api-key [] {
          op read "op://vault/api/key"
      }
      
      def setup-dev-env [] {
          $env.API_KEY = (op read "op://vault/dev/api-key")
          $env.DB_URL = (op read "op://vault/dev/database-url")
          print "Development environment configured"
      }
      
      # Load secrets into a record
      def load-secrets [] {
          {
              api_key: (op read "op://vault/api/key")
              db_password: (op read "op://vault/database/password")
              oauth_secret: (op read "op://vault/oauth/secret")
          }
      }
      ```
      
      ### Helper Script
      
      The provided `git-1p-helper` works with all shells:
      ```bash
      # Use the provided helper (works in any shell)
      git-1p-helper get vault/item/field
      git-1p-helper env API_KEY vault/item/field
      ```
      
      ## Best Practices
      
      - Never commit actual secrets to git
      - Use descriptive vault/item/field names
      - Keep .env files in .gitignore
      - Use environment-specific vaults (dev, staging, prod)
      - Rotate secrets regularly
      - Use shell-specific syntax for optimal integration
      
      ## Git Hooks
      
      This project includes git hooks that scan for:
      - API keys (various patterns)
      - Passwords in code
      - Common secret patterns
      - Hardcoded tokens
      
      If secrets are detected, the commit will be blocked.
      
      ### Bypass (use with caution)
      ```bash
      git commit --no-verify
      ```
      
      ## Work Email Management with 1Password
      
      Your work directory (`~/ir/`) is configured to use 1Password for secure email management.
      
      ### Setup Work Email in 1Password
      
      1. Store your work email in 1Password:
         ```bash
         # Create or update the work email in 1Password
         op item create --category login --title "CompanyName" --vault "Work" \
           email="your.name@company.com"
         ```
      
      2. Update git configuration to use the stored email:
         ```bash
         update-work-git-config
         ```
      
      ### Manual Setup (Alternative)
      
      If you prefer to set up the 1Password item manually:
      
      1. Open 1Password
      2. Create a new item in your "Work" vault
      3. Title: "CompanyName" (replace with your company)
      4. Add a field named "email" with your work email
      5. Run `update-work-git-config` to sync
      
      ### Verification
      
      Check that your work email is configured correctly:
      ```bash
      cd ~/ir/any-work-repo
      git config --get user.email  # Should show your work email
      
      cd ~/personal-project
      git config --get user.email  # Should show contact@oscarvarto.mx
      ```
      
      ### Automation
      
      You can add this to your shell profile to automatically update on login:
      ```bash
      # Add to ~/.config/fish/config.fish or ~/.zshrc
      update-work-git-config >/dev/null 2>&1
      ```
    '';
  };

}

