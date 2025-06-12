# Secure Workflow Setup Documentation

This document provides a comprehensive guide for setting up and maintaining the secure automated workflow configuration on macOS. This setup includes Fish shell automation, LaunchAgent services, Warp terminal integration, and proper macOS permissions management.

## Overview

The secure workflow consists of:
- **Fish Shell Integration**: Custom functions and snippets for secure command execution
- **LaunchAgent Service**: Background service for automated tasks
- **Warp Terminal Configuration**: Security-focused terminal settings
- **macOS Permissions**: Properly configured system permissions for secure operation

## Prerequisites

- macOS 26 Tahoe (or compatible version)
- Warp terminal installed and configured
- Fish shell as the default shell
- Nix package manager with home-manager
- Administrative access for system configuration

## Step-by-Step Setup Guide

### Step 1: Fish Shell Configuration

#### Location
- Fish configuration managed through Nix at: `~/nixos-config/modules/darwin/home-manager`
- Shared configuration at: `~/nixos-config/modules/shared/home-manager`

#### Fish Shell Snippet
```fish
# Secure workflow functions
function secure_execute
    # Validate input parameters
    if test (count $argv) -eq 0
        echo "Error: No command provided" >&2
        return 1
    end
    
    # Log the execution attempt
    echo "[$(date)] Executing secure command: $argv" >> ~/.local/var/log/secure_workflow.log
    
    # Execute with proper error handling
    eval $argv
    set exit_status $status
    
    if test $exit_status -ne 0
        echo "[$(date)] Command failed with status: $exit_status" >> ~/.local/var/log/secure_workflow.log
    end
    
    return $exit_status
end

# Environment validation
function validate_secure_env
    # Check required environment variables
    set required_vars HOME USER PATH
    
    for var in $required_vars
        if not set -q $var
            echo "Error: Required environment variable $var is not set" >&2
            return 1
        end
    end
    
    echo "Environment validation passed"
    return 0
end

# Secure path management
function secure_path_add
    if test -d $argv[1]
        if not contains $argv[1] $PATH
            set -gx PATH $argv[1] $PATH
            echo "Added $argv[1] to PATH"
        end
    else
        echo "Warning: Directory $argv[1] does not exist" >&2
    end
end
```

#### Custom Aliases
```fish
# Build aliases (as per user rules)
alias nb='nix build'
alias ns='nix build --switch'

# Secure workflow aliases
alias slog='tail -f ~/.local/var/log/secure_workflow.log'
alias scheck='validate_secure_env'
```

### Step 2: LaunchAgent Configuration

#### Location
- LaunchAgent plist created at: `~/Library/LaunchAgents/`
- Managed through Nix configuration

#### LaunchAgent Plist
```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>com.user.secure-workflow</string>
    
    <key>ProgramArguments</key>
    <array>
        <string>/Users/oscarvarto/.local/share/bin/secure-workflow-daemon</string>
    </array>
    
    <key>RunAtLoad</key>
    <true/>
    
    <key>KeepAlive</key>
    <dict>
        <key>SuccessfulExit</key>
        <false/>
    </dict>
    
    <key>StandardOutPath</key>
    <string>/Users/oscarvarto/.local/var/log/secure-workflow-daemon.log</string>
    
    <key>StandardErrorPath</key>
    <string>/Users/oscarvarto/.local/var/log/secure-workflow-daemon-error.log</string>
    
    <key>WorkingDirectory</key>
    <string>/Users/oscarvarto</string>
    
    <key>EnvironmentVariables</key>
    <dict>
        <key>PATH</key>
        <string>/usr/local/bin:/usr/bin:/bin:/Users/oscarvarto/.local/share/bin</string>
        <key>HOME</key>
        <string>/Users/oscarvarto</string>
    </dict>
    
    <key>ProcessType</key>
    <string>Background</string>
    
    <key>LowPriorityIO</key>
    <true/>
    
    <key>Nice</key>
    <integer>10</integer>
</dict>
</plist>
```

#### LaunchAgent Management Commands
```bash
# Load the LaunchAgent
launchctl load ~/Library/LaunchAgents/com.user.secure-workflow.plist

# Unload the LaunchAgent
launchctl unload ~/Library/LaunchAgents/com.user.secure-workflow.plist

# Check status
launchctl list | grep secure-workflow

# View logs
tail -f ~/.local/var/log/secure-workflow-daemon.log
```

### Step 3: Warp Terminal Settings

#### Security-Focused Configuration

1. **Privacy Settings**
   - Disable telemetry and analytics
   - Enable local-only command history
   - Disable cloud sync for sensitive data

2. **Terminal Security**
   - Enable session isolation
   - Configure secure environment variable handling
   - Set appropriate umask for file creation

3. **Warp Settings Adjustments**
   ```json
   {
     "privacy": {
       "disable_telemetry": true,
       "local_history_only": true,
       "cloud_sync_enabled": false
     },
     "security": {
       "session_isolation": true,
       "secure_env_vars": true,
       "default_umask": "0022"
     },
     "workflow": {
       "enable_secure_mode": true,
       "validate_commands": true,
       "log_execution": true
     }
   }
   ```

4. **Terminal Environment**
   ```fish
   # Set secure umask
   umask 0022
   
   # Configure secure environment
   set -gx SECURE_MODE 1
   set -gx LOG_COMMANDS 1
   ```

### Step 4: macOS Permissions Configuration

#### Required Permissions

The following macOS permissions must be granted for the secure workflow to function properly:

1. **Full Disk Access**
   - **Application**: Warp Terminal
   - **Reason**: Required for accessing configuration files and logs
   - **Location**: System Preferences → Security & Privacy → Privacy → Full Disk Access

2. **Accessibility**
   - **Application**: Warp Terminal
   - **Reason**: Required for secure automation features
   - **Location**: System Preferences → Security & Privacy → Privacy → Accessibility

3. **Developer Tools**
   - **Application**: Terminal applications (Warp, ghostty)
   - **Reason**: Required for running development tools and scripts
   - **Location**: System Preferences → Security & Privacy → Privacy → Developer Tools

4. **Files and Folders**
   - **Directories**: 
     - `~/nixos-config/`
     - `~/.local/share/bin/`
     - `~/.local/var/log/`
   - **Reason**: Required for configuration management and logging
   - **Location**: System Preferences → Security & Privacy → Privacy → Files and Folders

5. **Automation**
   - **Application**: Warp Terminal
   - **Target**: System Events, Finder
   - **Reason**: Required for automated workflow execution
   - **Location**: System Preferences → Security & Privacy → Privacy → Automation

#### Permission Verification
```fish
# Check current permissions
function check_permissions
    echo "Checking macOS permissions for secure workflow..."
    
    # Check if running with appropriate permissions
    if test -w ~/.local/var/log/
        echo "✓ Log directory is writable"
    else
        echo "✗ Log directory is not writable" >&2
    end
    
    if test -x ~/.local/share/bin/secure-workflow-daemon
        echo "✓ Daemon script is executable"
    else
        echo "✗ Daemon script is not executable" >&2
    end
    
    # Check LaunchAgent status
    if launchctl list | grep -q secure-workflow
        echo "✓ LaunchAgent is loaded"
    else
        echo "✗ LaunchAgent is not loaded" >&2
    end
end
```

## Directory Structure

```
~/nixos-config/
├── modules/
│   ├── darwin/
│   │   ├── home-manager/          # Main home-manager config
│   │   └── ...
│   └── shared/
│       ├── home-manager/          # Shared home-manager config
│       └── ...
├── SECURE_WORKFLOW_SETUP.md       # This documentation
└── ...

~/Library/LaunchAgents/
└── com.user.secure-workflow.plist  # LaunchAgent configuration

~/.local/
├── share/bin/                     # Executable scripts
│   └── secure-workflow-daemon
└── var/log/                       # Log files
    ├── secure_workflow.log
    ├── secure-workflow-daemon.log
    └── secure-workflow-daemon-error.log
```

## Maintenance and Troubleshooting

### Regular Maintenance

1. **Log Rotation**
   ```fish
   # Rotate logs weekly
   function rotate_secure_logs
       set log_dir ~/.local/var/log
       set timestamp (date +%Y%m%d_%H%M%S)
       
       for log in $log_dir/secure*.log
           if test -f $log
               mv $log $log.$timestamp
               touch $log
           end
       end
   end
   ```

2. **Configuration Validation**
   ```fish
   # Validate configuration integrity
   function validate_config
       echo "Validating secure workflow configuration..."
       
       # Check Fish configuration
       fish -n ~/.config/fish/config.fish
       if test $status -eq 0
           echo "✓ Fish configuration is valid"
       else
           echo "✗ Fish configuration has syntax errors" >&2
       end
       
       # Check LaunchAgent plist
       plutil -lint ~/Library/LaunchAgents/com.user.secure-workflow.plist
       if test $status -eq 0
           echo "✓ LaunchAgent plist is valid"
       else
           echo "✗ LaunchAgent plist has errors" >&2
       end
   end
   ```

### Troubleshooting Common Issues

1. **LaunchAgent Not Starting**
   - Check plist syntax: `plutil -lint ~/Library/LaunchAgents/com.user.secure-workflow.plist`
   - Verify file permissions: `ls -la ~/.local/share/bin/secure-workflow-daemon`
   - Check system logs: `log show --predicate 'subsystem == "com.apple.launchd"' --last 1h`

2. **Permission Denied Errors**
   - Verify macOS permissions in System Preferences
   - Check file ownership: `ls -la ~/.local/`
   - Ensure correct umask settings

3. **Fish Function Not Working**
   - Reload Fish configuration: `source ~/.config/fish/config.fish`
   - Check function syntax: `fish -n -c 'functions secure_execute'`
   - Verify PATH settings: `echo $PATH`

4. **Warp Integration Issues**
   - Reset Warp settings to defaults
   - Check Warp logs in Console.app
   - Verify terminal environment variables

### Security Best Practices

1. **Regular Security Audits**
   - Review log files for suspicious activity
   - Validate file permissions quarterly
   - Update configurations when system changes occur

2. **Backup and Recovery**
   - Include configuration files in regular backups
   - Test restoration procedures periodically
   - Document any custom modifications

3. **Access Control**
   - Limit file permissions to minimum required
   - Use secure file locations (`~/.local/` hierarchy)
   - Avoid storing secrets in configuration files

## Update Procedures

### Updating Fish Configuration
1. Modify configuration in `~/nixos-config/modules/darwin/home-manager`
2. Run `ns` to apply changes (nix build-switch)
3. Test configuration: `fish -n ~/.config/fish/config.fish`
4. Reload Fish: `source ~/.config/fish/config.fish`

### Updating LaunchAgent
1. Unload current agent: `launchctl unload ~/Library/LaunchAgents/com.user.secure-workflow.plist`
2. Update plist file through Nix configuration
3. Apply changes: `ns`
4. Load updated agent: `launchctl load ~/Library/LaunchAgents/com.user.secure-workflow.plist`

### Updating Warp Settings
1. Modify settings through Warp preferences UI
2. Document changes in this file
3. Export settings for backup if available

## Support and Documentation

- **Configuration Repository**: `~/nixos-config/`
- **Log Files**: `~/.local/var/log/secure*.log`
- **System Documentation**: This file (`SECURE_WORKFLOW_SETUP.md`)
- **Nix Documentation**: See individual module README files

## Version History

| Version | Date | Changes |
|---------|------|----------|
| 1.0 | Current | Initial secure workflow setup documentation |

---

**Note**: This configuration is designed for macOS 26 Tahoe and may require adjustments for other macOS versions. Always test changes in a non-production environment first.

**Security Warning**: Ensure all file permissions are properly set and regularly audit the configuration for security compliance.

