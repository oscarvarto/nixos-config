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
      
      echo -e "$YELLOW🔍 Scanning for secrets..."$NC
      
      # Get staged files
      STAGED_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep -E '\.(js|ts|py|java|scala|clj|cs|json|yaml|yml|toml|env|sh|bash|fish|nix)$' || true)
      
      if [ -z "$STAGED_FILES" ]; then
        echo -e "$GREEN✅ No files to check"$NC
        exit 0
      fi
      
      FOUND_SECRETS=false
      
      # Simple patterns for common secrets
      PATTERNS=(
        'AKIA[0-9A-Z]{16}'                    # AWS Access Key
        'sk-[a-zA-Z0-9]{48}'                  # OpenAI API Key
        'gh[ps]_[A-Za-z0-9_]{36}'            # GitHub Token
        '[aA][pP][iI]_[kK][eE][yY].*["'"'"'][a-zA-Z0-9_-]{20,}["'"'"']'  # API_KEY assignments
        '[pP][aA][sS][sS][wW][oO][rR][dD].*["'"'"'][a-zA-Z0-9_-]{8,}["'"'"']'  # PASSWORD assignments
        '[sS][eE][cC][rR][eE][tT].*["'"'"'][a-zA-Z0-9_-]{16,}["'"'"']'    # SECRET assignments
      )
      
      for FILE in $STAGED_FILES; do
        if [ -f "$FILE" ]; then
          for PATTERN in "''${PATTERNS[@]}"; do
            if git show ":$FILE" | grep -qE "$PATTERN"; then
              echo -e "$RED❌ Potential secret found in $FILE"$NC
              FOUND_SECRETS=true
            fi
          done
        fi
      done
      
      if [ "$FOUND_SECRETS" = true ]; then
        echo -e "$RED\n🚫 COMMIT BLOCKED: Potential secrets detected!"$NC
        echo -e "$YELLOW💡 Use 1Password CLI instead:"$NC
        echo -e "$YELLOW   export API_KEY=\$(op read 'op://vault/item/field')"$NC
        echo -e "$YELLOW\n🔧 To bypass (use with caution): git commit --no-verify"$NC
        exit 1
      fi
      
      echo -e "$GREEN✅ No secrets detected"$NC
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
            echo "❌ Failed to read from 1Password: $2" >&2
            echo "💡 Try: op signin" >&2
            exit 1
          }
          ;;
        "env")
          if [ -z "$2" ] || [ -z "$3" ]; then
            echo "Usage: git-1p-helper env <VAR_NAME> <vault/item/field>"
            exit 1
          fi
          export "$2"=$(op read "op://$3" 2>/dev/null)
          echo "✅ Exported $2 from 1Password"
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
    '';
  };
}

