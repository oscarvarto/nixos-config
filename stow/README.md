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
├── dotnet-tools/         # .NET tools management
│   ├── dotnet-tools.toml# Configuration for .NET SDK and tools
│   └── .local/share/bin/# Management script
├── lazyvim/              # Neovim LazyVim configuration
│   └── .config/nvim/    # LazyVim configuration files
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
stow -t ~ aux-scripts    # Deploy auxiliary scripts
stow -t ~ cargo-tools    # Deploy cargo tools management
stow -t ~ nodejs-tools   # Deploy Node.js tools management
stow -t ~ dotnet-tools   # Deploy .NET tools management
stow -t ~ lazyvim        # Deploy LazyVim configuration
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

### .NET Tools (`dotnet-tools`)
- Configuration-based management of .NET SDK and global tools
- `manage-dotnet-tools` - Install, update, and manage .NET environment
- `dotnet-tools.toml` - Declarative configuration for SDK version and tools
- Integrates with .NET CLI global tool system

### LazyVim Configuration (`lazyvim`)
- Neovim configuration based on LazyVim framework
- Complete Lua configuration with plugins and settings
- Symlinked to `~/.config/nvim/` when deployed
- Works with neovim-nightly installed via nix

## Benefits

1. **Avoid Nix Parsing Issues**: Scripts remain as separate files, avoiding complex escaping
2. **Version Control**: All scripts are tracked in your nixos-config repository
3. **Easy Deployment**: Single command deploys all scripts
4. **System Restoration**: Easily restore all scripts after system rebuilds
5. **Selective Management**: Can manage different script packages independently
