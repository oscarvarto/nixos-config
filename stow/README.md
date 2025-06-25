# Stow-Managed Auxiliary Scripts

This directory contains auxiliary scripts and configurations managed by GNU Stow. These are scripts that need to avoid problematic parsing when embedded in Nix configuration files, but should still be version-controlled and restorable.

## Directory Structure

```
stow/
├── aux-scripts/          # Auxiliary scripts package
│   └── .local/
│       └── share/
│           └── bin/     # Scripts that go in ~/.local/share/bin
├── cargo-tools/          # Rust/Cargo tools management
│   ├── cargo-tools.toml # Configuration for cargo packages
│   └── .local/share/bin/# Management script
├── nodejs-tools/         # Node.js tools management
│   ├── nodejs-tools.toml# Configuration for Node.js packages
│   └── .local/share/bin/# Management script
└── README.md           # This file
```

## Usage

### Install/Deploy All Packages
To deploy all managed tools and scripts:
```bash
cd ~/nixos-config/stow
manage-aux-scripts deploy
```

Or deploy individual packages:
```bash
stow aux-scripts    # Deploy auxiliary scripts
stow cargo-tools    # Deploy cargo tools management
stow nodejs-tools   # Deploy Node.js tools management
```

This will create symlinks from `~/.local/share/bin/` to the scripts in `stow/aux-scripts/.local/share/bin/`.

### Remove All Packages
To remove all symlinks:
```bash
cd ~/nixos-config/stow
manage-aux-scripts remove
```

### Add New Scripts
1. Place new scripts in the appropriate subdirectory under `stow/aux-scripts/`
2. Ensure they have the correct executable permissions
3. Run `stow aux-scripts` to deploy the new scripts

### System Restoration
After a system wipe:
1. Install GNU Stow via Nix (already configured in home-manager.nix)
2. Clone this repository
3. Run `cd ~/nixos-config/stow && stow aux-scripts`

## Managed Packages

### Auxiliary Scripts (`aux-scripts`)
- `cleanup-intellij` - Comprehensive IntelliJ IDEA cache and project cleanup utility
- `install-metals-emacs.sh` - Installs Metals language server for Scala development in Emacs
- `lunar` - CLI wrapper for Lunar display control app
- `manage-aux-scripts` - Management utility for this stow-based script system
- `restore-jj-repo` - Restores a Jujutsu colocated repository after .jj directory deletion
- `test-maven-classpath.sh` - Maven classpath testing utility
- `testng-debug` - TestNG debugging wrapper with proper classpath setup
- `testng-debug-zsh` - Zsh version of TestNG debugging wrapper
- `testng-debug.nu` - Nushell version of TestNG debugging wrapper

### Cargo Tools (`cargo-tools`)
- Configuration-based management of Rust/Cargo installed tools
- `manage-cargo-tools` - Install, update, and manage cargo packages
- `cargo-tools.toml` - Declarative configuration for desired packages

### Node.js Tools (`nodejs-tools`)
- Configuration-based management of Node.js toolchain and global packages
- `manage-nodejs-tools` - Install, update, and manage Node.js environment
- `nodejs-tools.toml` - Declarative configuration for toolchain and packages
- Integrates with Volta for Node.js version management

## Benefits

1. **Avoid Nix Parsing Issues**: Scripts remain as separate files, avoiding complex escaping
2. **Version Control**: All scripts are tracked in your nixos-config repository
3. **Easy Deployment**: Single command deploys all scripts
4. **System Restoration**: Easily restore all scripts after system rebuilds
5. **Selective Management**: Can manage different script packages independently
