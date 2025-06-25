# Stow-Managed Auxiliary Scripts

This directory contains auxiliary scripts and configurations managed by GNU Stow. These are scripts that need to avoid problematic parsing when embedded in Nix configuration files, but should still be version-controlled and restorable.

## Directory Structure

```
stow/
├── aux-scripts/          # Auxiliary scripts package
│   └── .local/
│       └── share/
│           └── bin/     # Scripts that go in ~/.local/share/bin
└── README.md           # This file
```

## Usage

### Install/Deploy Scripts
To deploy all auxiliary scripts:
```bash
cd ~/nixos-config/stow
stow aux-scripts
```

This will create symlinks from `~/.local/share/bin/` to the scripts in `stow/aux-scripts/.local/share/bin/`.

### Remove Scripts
To remove the symlinks:
```bash
cd ~/nixos-config/stow
stow -D aux-scripts
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

## Current Scripts

- `cleanup-intellij` - Comprehensive IntelliJ IDEA cache and project cleanup utility
- `install-metals-emacs.sh` - Installs Metals language server for Scala development in Emacs
- `lunar` - CLI wrapper for Lunar display control app
- `manage-aux-scripts` - Management utility for this stow-based script system
- `restore-jj-repo` - Restores a Jujutsu colocated repository after .jj directory deletion
- `test-maven-classpath.sh` - Maven classpath testing utility
- `testng-debug` - TestNG debugging wrapper with proper classpath setup
- `testng-debug-zsh` - Zsh version of TestNG debugging wrapper
- `testng-debug.nu` - Nushell version of TestNG debugging wrapper

## Benefits

1. **Avoid Nix Parsing Issues**: Scripts remain as separate files, avoiding complex escaping
2. **Version Control**: All scripts are tracked in your nixos-config repository
3. **Easy Deployment**: Single command deploys all scripts
4. **System Restoration**: Easily restore all scripts after system rebuilds
5. **Selective Management**: Can manage different script packages independently
