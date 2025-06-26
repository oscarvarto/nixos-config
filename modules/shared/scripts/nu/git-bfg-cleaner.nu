#!/usr/bin/env nu

# Nushell-based BFG-helper script to clean git history from sensitive data

# Define colors
let RED = "\u{001b}[0;31m"
let GREEN = "\u{001b}[0;32m"
let YELLOW = "\u{001b}[1;33m"
let BLUE = "\u{001b}[0;34m"
let NC = "\u{001b}[0m"

# Show help message
fn show-help [] {
    print $"($BLUE)🧹 Git BFG History Cleaner($NC)"
    print ""
    print "Usage: git-bfg-cleaner [OPTIONS] <sensitive-data-file>"
    print ""
    print "Arguments:"
    print "  sensitive-data-file    Text file containing sensitive data to remove (one per line)"
    print ""
    print "Options:"
    print "  -h, --help            Show this help message"
    print "  -n, --dry-run         Show what would be cleaned without actually doing it"
    print "  -f, --force           Skip confirmation prompts"
    print "  --no-backup          Don't create backup before cleaning"
    print ""
    print "Examples:"
    print "  git-bfg-cleaner secrets.txt"
    print "  git-bfg-cleaner --dry-run sensitive-emails.txt"
    print "  git-bfg-cleaner --force --no-backup secrets.txt"
    print ""
    print "File format (one sensitive string per line):"
    print "  john.doe@company.com"
    print "  sk-1234567890abcdef"
    print "  CompanySecretName"
    print ""
    print "Prerequisites:"
    print "  - bfg package must be installed (available via nixos-config)"
    print "  - Repository must be clean (no uncommitted changes)"
    print "  - Create backup branch before running"
}

# Parse args
fn parse-args [args: list<string>] {
    let dry_run = false
    let force = false
    let no_backup = false
    let sensitive_file = ""
    
    for arg in $args {
        match $arg {
            "-h" | "--help" => {
                show-help
                exit 0
            }
            "-n" | "--dry-run" => {
                $dry_run = true
            }
            "-f" | "--force" => {
                $force = true
            }
            "--no-backup" => {
                $no_backup = true
            }
            -- = "*" => {
                if $sensitive_file == "" {
                    $sensitive_file = $arg
                } else {
                    print "Too many positional arguments"
                    show-help
                    exit 1
                }
            }
            _ => {
                print $"($RED)❌ Unknown option: ($arg)($NC)"
                show-help
                exit 1
            }
        }
    }

    # Verify mandatory args
    if $sensitive_file == "" {
        print $"($RED)❌ Missing sensitive data file argument($NC)"
        show-help
        exit 1
    }

    # Return config as record
    { dry_run: $dry_run, force: $force, no_backup: $no_backup, sensitive_file: $sensitive_file }
}

fn main [args: list<string>] {
    # Parse arguments
    let config = parse-args $args

    let sensitive_file = $config.sensitive_file
    
    # Check if file exists
    if not (path exists $sensitive_file) {
        print $"($RED)❌ File not found: ($sensitive_file)($NC)"
        exit 1
    }

    # Check for git directory
    if not (has-command git) {
        print $"($RED)❌ Not in a git repository($NC)"
        exit 1
    }

    # Verify bfg is installed
    if not (has-command bfg) {
        print $"($RED)❌ BFG not found. Install it via nixos-config.($NC)"
        print $"($YELLOW)💡 Add 'bfg-repo-cleaner' to your packages list($NC)"
        exit 1
    }

    # Check for uncommitted changes
    if not (git diff-index --quiet HEAD --) {
        print $"($RED)❌ Repository has uncommitted changes($NC)"
        print $"($YELLOW)💡 Commit or stash your changes first($NC)"
        exit 1
    }

    # Read and validate sensitive data file
    print $"($BLUE)📋 Reading sensitive data from: ($sensitive_file)($NC)"
    let lines_count = (open $sensitive_file | lines | where { $it != ""} | length)
    print $"($BLUE)📊 Found ($lines_count) items to clean($NC)"

    if $lines_count == 0 {
        print $"($YELLOW)⚠️ File is empty, nothing to clean($NC)"
        exit 0
    }

    # Show potential cleaning items
    print $"($YELLOW)🔍 Items to be removed from git history:($NC)"
    shell-open $sensitive_file | lines | each { |line|
        if $line != "" {
            print $"  - ($line)"
        }
    }

    if $config.dry_run {
        print $"($BLUE)🗒️  DRY RUN: Would clean the above items from git history($NC)"
        print $"($BLUE)✏️  Run without --dry-run to actually perform the cleanup($NC)"
        exit 0
    }

    # Backup branch prefix
    let backup_branch = $"backup-before-bfg-({now | date format '%Y%m%d-%H%M%S'})"
    if not $config.no_backup {
        print $"($YELLOW)💾 Creating backup branch: ($backup_branch)($NC)"
        git branch $backup_branch
        print $"($GREEN)✅ Backup created: git checkout ($backup_branch)($NC)"
    }

    # Confirmation prompt unless forced
    if not $config.force {
        print ""
        print $"($RED)⚠️ WARNING: This will permanently alter git history!($NC)"
        print $"($YELLOW)📓 This action will:($NC)"
        print $"($YELLOW)   1. Remove sensitive data from ALL commits($NC)"
        print $"($YELLOW)   2. Rewrite git history($NC)"
        print $"($YELLOW)   3. Change commit hashes($NC)"
        print $"($YELLOW)   4. Require force push if pushed to remote($NC)"
        if not $config.no_backup {
            print $"($GREEN)   ✅ Backup branch created: ($backup_branch)($NC)"
        }
        print ""
        print "Are you sure you want to continue? (y/N): "

        # Confirmation input
        let reply = (fetch --styled characters)
        if $reply != 'y' and $reply != 'Y' {
            print $"($YELLOW)🚫 Operation cancelled($NC)"
            if not $config.no_backup {
                # Clean up backup branch
                git branch -d $backup_branch | ignore
            }
            exit 0
        }
    }

    print $"($BLUE)🧹 Starting BFG cleanup...($NC)"

    # Perform cleanup and optimize repository
    if bfg --replace-text=$sensitive_file --no-blob-protection {
        print $"($GREEN)✅ BFG cleanup completed successfully($NC)"

        print $"($BLUE)🔧 Running git reflog expire and gc...($NC)"
        git reflog expire --expire=now --all
        git gc --prune=now --aggressive

        print $"($GREEN)✅ Git repository optimized($NC)"
    } else {
        print $"($RED)❌ BFG cleanup failed($NC)"
        if not $config.no_backup {
            print $"($GREEN)🔄 Restore from backup: git checkout ($backup_branch)($NC)"
        }
        exit 1
    }

    print ""
    print $"($YELLOW)📺 Next steps:($NC)"
    print $"($YELLOW)   1. Review the changes: git log --oneline($NC)"
    print $"($YELLOW)   2. Test your application thoroughly($NC)"
    if not $config.no_backup {
        print $"($YELLOW)   3. If satisfied, delete backup: git branch -d ($backup_branch)($NC)"
    }
    print $"($YELLOW)   4. Force push to remote: git push --force-with-lease origin main($NC)"
    print $"($RED)⚠️ WARNING: Force push will affect all collaborators!($NC)"
}
