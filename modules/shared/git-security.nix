{ config, pkgs, lib, user ? "oscarvarto", ... }:

{
  # Git security: pre-commit hook to prevent secrets
  home.file.".config/git/hooks/pre-commit" = {
    executable = true;
    text = ''#!/usr/bin/env bash
      # Pre-commit hook to prevent committing secrets
      set -e

      # Colors
      RED='\033[0;31m'
      GREEN='\033[0;32m'
      YELLOW='\033[1;33m'
      NC='\033[0m'

      echo -e "$YELLOW\U1F50D Scanning for secrets...$NC"

      # Get staged files
      STAGED_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(js|ts|py|java|scala|clj|cs|json|yaml|yml|toml|env|sh|bash|fish|nix)$' || true)

      if [ -z "$STAGED_FILES" ]; then
        echo -e "$GREEN\U2705 No files to check$NC"
        exit 0
      fi

      FOUND_SECRETS=false

      # Simple patterns for common secrets
      PATTERNS=(
        'AKIA[0-9A-Z]{16}'                    # AWS Access Key
        'sk-[a-zA-Z0-9]{48}'                  # OpenAI API Key
        'gh[ps]_[A-Za-z0-9_]{36}'            # GitHub Token
        '[aA][pP][iI]_[kK][eE][yY].*[a-zA-Z0-9_-]{20,}'  # API_KEY assignments
        '[pP][aA][sS][sS][wW][oO][rR][dD].*[a-zA-Z0-9_-]{8,}'  # PASSWORD assignments
        '[sS][eE][cC][rR][eE][tT].*[a-zA-Z0-9_-]{16,}'    # SECRET assignments
      )

      for FILE in $STAGED_FILES; do
        if [ -f "$FILE" ]; then
          for PATTERN in "''${PATTERNS[@]}"; do
            if git show ":$FILE" | grep -qE "$PATTERN"; then
              echo -e "$RED\U274C Potential secret found in $FILE$NC"
              FOUND_SECRETS=true
            fi
          done
        fi
      done

      if [ "$FOUND_SECRETS" = true ]; then
        echo -e "$RED\n\U1F6AB COMMIT BLOCKED: Potential secrets detected!$NC"
        echo -e "$YELLOW\U1F4A1 Use 1Password CLI instead:$NC"
        echo -e "$YELLOW   export API_KEY=\$(op read 'op://vault/item/field')$NC"
        echo -e "$YELLOW\n\U1F527 To bypass (use with caution): git commit --no-verify$NC"
        exit 1
      fi

      echo -e "$GREEN\U2705 No secrets detected$NC"
    '';
  };

  # 1Password helper script
  home.file.".local/bin/git-1p-helper" = {
    executable = true;
    text = ''#!/usr/bin/env bash
      # 1Password CLI helper for secrets

      case "$1" in
        "get")
          if [ -z "$2" ]; then
            echo "Usage: git-1p-helper get <vault/item/field>"
            exit 1
          fi
          op read "op://$2" 2>/dev/null || {
            echo "\U274C Failed to read from 1Password: $2" >&2
            echo "\U1F4A1 Try: op signin" >&2
            exit 1
          }
          ;;
        "env")
          if [ -z "$2" ] || [ -z "$3" ]; then
            echo "Usage: git-1p-helper env <VAR_NAME> <vault/item/field>"
            exit 1
          fi
          export "$2"=$(op read "op://$3" 2>/dev/null)
          echo "\U2705 Exported $2 from 1Password"
          ;;
        *)
          echo "1Password Git Helper"
          echo "Usage:"
          echo "  git-1p-helper get <vault/item/field>"
          echo "  git-1p-helper env VAR_NAME <vault/item/field>"
          echo ""
          echo "Examples:"
          echo "  export API_KEY=\$(git-1p-helper get MyVault/API/key)"
          echo "  git-1p-helper env API_KEY MyVault/API/key"
          ;;
      esac
    '';
  };

  # Script to generate work-specific git configuration  
  home.file.".local/bin/update-work-git-config" = {
    executable = true;
    text = ''#!/usr/bin/env bash
      # Script to update work git config with email from 1Password
      set -e
      
      CONFIG_DIR="''${HOME}/.local/share/git"
      CONFIG_FILE="''${CONFIG_DIR}/config-work"
      
      # Ensure directory exists
      mkdir -p "''${CONFIG_DIR}"
      
      # Try to get work email from 1Password, fallback to placeholder
      if command -v op >/dev/null 2>&1 && op account get >/dev/null 2>&1; then
        WORK_EMAIL=$(op read "op://Work/CompanyName/email" 2>/dev/null || echo "YOUR-WORK-EMAIL@company.com")
      else
        WORK_EMAIL="YOUR-WORK-EMAIL@company.com"
        echo "\U26A0 1Password CLI not available or not signed in. Using placeholder email."
        echo "\U1F4A1 Sign in with: op signin"
      fi
      
      # Create/update the git config file
      cat > "''${CONFIG_FILE}" <<EOF
[user]
    name = Oscar Vargas Torres
    email = ''${WORK_EMAIL}
EOF
      
      echo "\U2705 Updated work git config with email: ''${WORK_EMAIL}"
    '';
  };
  
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

  # BFG helper script to clean git history from sensitive data file
  home.file.".local/bin/git-bfg-cleaner" = {
    executable = true;
    text = ''#!/usr/bin/env bash
      # BFG helper script to clean git history from sensitive data
      set -e
      
      # Colors
      RED='\033[0;31m'
      GREEN='\033[0;32m'
      YELLOW='\033[1;33m'
      BLUE='\033[0;34m'
      NC='\033[0m'
      
      show_help() {
        echo -e "$BLUE\U1F9F9 Git BFG History Cleaner$NC"
        echo ""
        echo "Usage: git-bfg-cleaner [OPTIONS] <sensitive-data-file>"
        echo ""
        echo "Arguments:"
        echo "  sensitive-data-file    Text file containing sensitive data to remove (one per line)"
        echo ""
        echo "Options:"
        echo "  -h, --help            Show this help message"
        echo "  -n, --dry-run         Show what would be cleaned without actually doing it"
        echo "  -f, --force           Skip confirmation prompts"
        echo "  --no-backup          Don't create backup before cleaning"
        echo ""
        echo "Examples:"
        echo "  git-bfg-cleaner secrets.txt"
        echo "  git-bfg-cleaner --dry-run sensitive-emails.txt"
        echo "  git-bfg-cleaner --force --no-backup secrets.txt"
        echo ""
        echo "File format (one sensitive string per line):"
        echo "  john.doe@company.com"
        echo "  sk-1234567890abcdef"
        echo "  CompanySecretName"
        echo ""
        echo "Prerequisites:"
        echo "  - bfg package must be installed (available via nixos-config)"
        echo "  - Repository must be clean (no uncommitted changes)"
        echo "  - Create backup branch before running"
      }
      
      # Parse arguments
      DRY_RUN=false
      FORCE=false
      NO_BACKUP=false
      SENSITIVE_FILE=""
      
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
          --no-backup)
            NO_BACKUP=true
            shift
            ;;
          -*)
            echo -e "$RED\U274C Unknown option: $1$NC" >&2
            show_help
            exit 1
            ;;
          *)
            if [ -z "$SENSITIVE_FILE" ]; then
              SENSITIVE_FILE="$1"
            else
              echo -e "$RED\U274C Too many arguments$NC" >&2
              show_help
              exit 1
            fi
            shift
            ;;
        esac
      done
      
      # Check if file argument is provided
      if [ -z "$SENSITIVE_FILE" ]; then
        echo -e "$RED\U274C Missing sensitive data file argument$NC" >&2
        show_help
        exit 1
      fi
      
      # Check if file exists
      if [ ! -f "$SENSITIVE_FILE" ]; then
        echo -e "$RED\U274C File not found: $SENSITIVE_FILE$NC" >&2
        exit 1
      fi
      
      # Check if we're in a git repository
      if ! git rev-parse --git-dir > /dev/null 2>&1; then
        echo -e "$RED\U274C Not in a git repository$NC" >&2
        exit 1
      fi
      
      # Check if bfg is available
      if ! command -v bfg > /dev/null 2>&1; then
        echo -e "$RED\U274C BFG not found. Install it via nixos-config.$NC" >&2
        echo -e "$YELLOW\U1F4A1 Add 'bfg-repo-cleaner' to your packages list$NC" >&2
        exit 1
      fi
      
      # Check for uncommitted changes
      if ! git diff-index --quiet HEAD --; then
        echo -e "$RED\U274C Repository has uncommitted changes$NC" >&2
        echo -e "$YELLOW\U1F4A1 Commit or stash your changes first$NC" >&2
        exit 1
      fi
      
      # Read and validate sensitive data file
      echo -e "$BLUE\U1F4CB Reading sensitive data from: $SENSITIVE_FILE$NC"
      LINES_COUNT=$(wc -l < "$SENSITIVE_FILE")
      echo -e "$BLUE\U1F4CA Found $LINES_COUNT items to clean$NC"
      
      if [ "$LINES_COUNT" -eq 0 ]; then
        echo -e "$YELLOW\U26A0 File is empty, nothing to clean$NC"
        exit 0
      fi
      
      # Show what will be cleaned
      echo -e "$YELLOW\U1F50D Items to be removed from git history:$NC"
      while IFS= read -r line; do
        [ -n "$line" ] && echo "  - $line"
      done < "$SENSITIVE_FILE"
      
      if [ "$DRY_RUN" = true ]; then
        echo -e "$BLUE\U1F4AD DRY RUN: Would clean the above items from git history$NC"
        echo -e "$BLUE\U1F4DD Run without --dry-run to actually perform the cleanup$NC"
        exit 0
      fi
      
      # Create backup branch if not skipped
      BACKUP_BRANCH="backup-before-bfg-$(date +%Y%m%d-%H%M%S)"
      if [ "$NO_BACKUP" != true ]; then
        echo -e "$YELLOW\U1F4BE Creating backup branch: $BACKUP_BRANCH$NC"
        git branch "$BACKUP_BRANCH"
        echo -e "$GREEN\U2705 Backup created: git checkout $BACKUP_BRANCH$NC"
      fi
      
      # Confirmation
      if [ "$FORCE" != true ]; then
        echo -e "$RED\U26A0 WARNING: This will permanently alter git history!$NC"
        echo -e "$YELLOW\U1F4CB This action will:$NC"
        echo -e "$YELLOW   1. Remove sensitive data from ALL commits$NC"
        echo -e "$YELLOW   2. Rewrite git history$NC"
        echo -e "$YELLOW   3. Change commit hashes$NC"
        echo -e "$YELLOW   4. Require force push if pushed to remote$NC"
        [ "$NO_BACKUP" != true ] && echo -e "$GREEN   \U2705 Backup branch created: $BACKUP_BRANCH$NC"
        echo ""
        read -p "Are you sure you want to continue? (y/N): " -n 1 -r
        echo ""
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
          echo -e "$YELLOW\U1F6AB Operation cancelled$NC"
          [ "$NO_BACKUP" != true ] && git branch -d "$BACKUP_BRANCH" 2>/dev/null || true
          exit 0
        fi
      fi
      
      echo -e "$BLUE\U1F9F9 Starting BFG cleanup...$NC"
      
      # Run BFG with the sensitive data file
      if bfg --replace-text="$SENSITIVE_FILE" --no-blob-protection; then
        echo -e "$GREEN\U2705 BFG cleanup completed successfully$NC"
        
        echo -e "$BLUE\U1F527 Running git reflog expire and gc...$NC"
        git reflog expire --expire=now --all
        git gc --prune=now --aggressive
        
        echo -e "$GREEN\U2705 Git repository optimized$NC"
        echo ""
        echo -e "$YELLOW\U1F4CB Next steps:$NC"
        echo -e "$YELLOW   1. Review the changes: git log --oneline$NC"
        echo -e "$YELLOW   2. Test your application thoroughly$NC"
        [ "$NO_BACKUP" != true ] && echo -e "$YELLOW   3. If satisfied, delete backup: git branch -d $BACKUP_BRANCH$NC"
        echo -e "$YELLOW   4. Force push to remote: git push --force-with-lease origin main$NC"
        echo -e "$RED\U26A0 WARNING: Force push will affect all collaborators!$NC"
      else
        echo -e "$RED\U274C BFG cleanup failed$NC" >&2
        [ "$NO_BACKUP" != true ] && echo -e "$GREEN\U1F504 Restore from backup: git checkout $BACKUP_BRANCH$NC"
        exit 1
      fi
    '';
  };
}

