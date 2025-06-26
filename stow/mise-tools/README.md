# Mise Tools Management

This stow package provides version-controlled management of mise-managed runtimes (Java, Python, Node.js, etc.) with intelligent fallback handling for unavailable versions.

## How It Works

1. **Configuration Tracking**: `mise-tools.toml` tracks your desired runtime versions and global settings
2. **Management Script**: `manage-mise-tools` provides commands to install, update, and manage runtimes
3. **Version Control**: Your mise runtime configuration is tracked in git with your nixos-config
4. **System Restoration**: After a system wipe, easily reinstall your entire development environment
5. **Intelligent Fallbacks**: Automatically finds newer versions when exact versions are unavailable

## Files

- `mise-tools.toml` - Configuration file listing desired runtimes and versions
- `.local/share/bin/manage-mise-tools` - Management script (deployed via stow)

## Usage

### Deploy the management script:
```bash
cd ~/nixos-config/stow
stow mise-tools
```

### Install all configured runtimes:
```bash
manage-mise-tools install
```

### Check status of configured runtimes:
```bash
manage-mise-tools status
```

### List configured runtimes:
```bash
manage-mise-tools list
```

### Update mise and runtimes:
```bash
manage-mise-tools update
```

### Clean cache and old versions:
```bash
manage-mise-tools clean
```

### Run mise doctor for troubleshooting:
```bash
manage-mise-tools doctor
```

### Add a new runtime to configuration:
```bash
manage-mise-tools add
```

## Configuration Format

Edit `mise-tools.toml` to manage your runtimes:

```toml
[runtimes]
# Java/JDK versions
java = [
  { version = "corretto-21.0.7.6.1", description = "Amazon Corretto JDK 21 (LTS)" },
  { version = "oracle-graalvm-24.0.1", description = "Oracle GraalVM 24 (latest)", global = true },
]

# Python versions
python = [
  { version = "3.11.7", description = "Python 3.11 (stable)", global = true },
  { version = "3.12.1", description = "Python 3.12 (latest)" },
]

# Node.js versions (if not using volta)
nodejs = [
  { version = "20.10.0", description = "Node.js 20 LTS", global = true },
]

[global_tools]
# Reserved for future mise global tool support

[settings]
auto_update = true
use_fallback_newer = true
sync_global_config = true
cleanup_old_versions = false
install_timeout = 600
```

## Key Features

### Intelligent Version Fallbacks
- **Resilient Installation**: When an exact version isn't available, automatically finds newer versions
- **Provider-Aware**: Maintains provider consistency (e.g., if `corretto-21.0.7.6.1` fails, finds latest `corretto-*`)
- **Configurable**: Can disable fallbacks if you need exact versions only

### Runtime Management
- **Multiple Versions**: Install multiple versions of the same runtime
- **Global Defaults**: Set specific versions as global defaults
- **Cross-Runtime**: Manage Java, Python, Node.js, Ruby, Go, etc. in one place

### System Integration
- **Stow Integration**: Seamlessly deploys with your other managed scripts
- **Version Control**: Configuration tracked with your dotfiles
- **Restoration**: Easy recovery after system rebuilds

## Benefits

1. **Version Control**: Your runtime preferences are tracked in git
2. **Easy Restoration**: After system restore, one command reinstalls everything
3. **Intelligent Fallbacks**: Handles upstream version changes gracefully
4. **Documentation**: Each runtime has a description explaining its purpose
5. **Flexibility**: Supports all mise-supported runtimes
6. **Integration**: Works seamlessly with your existing setup

## System Restoration Workflow

After a system wipe:
1. Install mise (via homebrew/nix)
2. Clone your nixos-config
3. Run `cd ~/nixos-config/stow && stow mise-tools`
4. Run `manage-mise-tools install`

Your entire development runtime environment will be automatically restored!

## Error Resilience

### Version Fallback Logic
1. **Try Exact Version**: Attempts to install the exact configured version
2. **Provider Fallback**: If failed, finds latest version from same provider (e.g., `corretto-*`)
3. **General Fallback**: If no provider match, uses latest available version
4. **Graceful Failure**: Reports what happened and continues with other runtimes

### Configuration Options
- `use_fallback_newer = true`: Enable intelligent fallbacks (recommended)
- `auto_update = true`: Allow automatic updates during sync
- `cleanup_old_versions = false`: Keep old versions (safer default)

## Examples

### Managing Java Versions
```toml
java = [
  { version = "corretto-17.0.15.6.1", description = "Amazon Corretto JDK 17 (LTS)" },
  { version = "corretto-21.0.7.6.1", description = "Amazon Corretto JDK 21 (LTS)" },
  { version = "oracle-graalvm-24.0.1", description = "Oracle GraalVM 24 (latest)", global = true },
]
```

### Multi-Language Setup
```toml
[runtimes]
java = [{ version = "corretto-21.0.7.6.1", description = "Java 21 LTS", global = true }]
python = [{ version = "3.11.7", description = "Python 3.11", global = true }]
nodejs = [{ version = "20.10.0", description = "Node.js 20 LTS", global = true }]
ruby = [{ version = "3.3.0", description = "Ruby 3.3", global = true }]
go = [{ version = "1.21.5", description = "Go 1.21", global = true }]
```

This provides a robust, version-controlled development environment that adapts to upstream changes!
