# NordVPN Dedicated IP Autoconnect Script

This bash script automatically connects to a NordVPN dedicated IP address on startup and ensures reconnection if the connection drops.

## Features

- **Idempotent**: Only connects if no other VPN is currently active
- **Silent operation**: Runs in the background without user interaction
- **Autoconnect setup**: Configures NordVPN to automatically reconnect
- **Smart detection**: Checks for GlobalProtect and other VPN conflicts
- **Configurable**: Environment variables for easy customization
- **Logging**: Detailed logs for troubleshooting
- **Retry logic**: Multiple connection attempts with exponential backoff

## Requirements

- NordVPN CLI client installed and configured
- 1Password CLI (`op`) installed and authenticated
- Bash shell
- Valid NordVPN dedicated IP address stored in 1Password Personal vault

## Configuration

### Required Setup

- **1Password CLI Authentication**: Must be signed in to 1Password
- **1Password Item**: "NordVPN Dedicated IP" in Personal vault with your dedicated IP

### Optional Environment Variables

- `SKIP_VPN_AUTOCONNECT`: Set to "true" to disable VPN autoconnect entirely
- `GLOBALPROTECT_ACTIVE`: Set to "true" to skip NordVPN when GlobalProtect is active

## Usage

### Basic Usage

```bash
# Ensure you're signed in to 1Password
op signin

# Run the script (it will retrieve IP from 1Password)
./nordvpn-autoconnect.sh
```

### First-time Setup

```bash
# Create 1Password entry (already done)
op item create --category=password --title="NordVPN Dedicated IP" --vault="Personal" password="your.dedicated.ip.here"

# Test retrieval
op item get "NordVPN Dedicated IP" --vault="Personal" --field password --reveal
```

### Skip VPN Connection

```bash
# Temporarily disable VPN autoconnect
SKIP_VPN_AUTOCONNECT="true" ./nordvpn-autoconnect.sh
```

### GlobalProtect Integration

```bash
# Skip NordVPN when GlobalProtect is active
GLOBALPROTECT_ACTIVE="true" ./nordvpn-autoconnect.sh
```

## Startup Integration

### Option 1: Shell Profile

Add to your `.bashrc`, `.zshrc`, or `.fish_config`:

```bash
# Add to ~/.bashrc or ~/.zshrc
if [ -f "$HOME/nixos-config/nordvpn-autoconnect.sh" ]; then
    export NORDVPN_DEDICATED_IP="your.dedicated.ip.here"
    "$HOME/nixos-config/nordvpn-autoconnect.sh" &
fi
```

### Option 2: Home-Manager LaunchAgent (macOS) - INTEGRATED

This script is now integrated into your Home-Manager configuration as a macOS LaunchAgent with 1Password integration.

**Configuration Location**: `~/nixos-config/modules/darwin/home-manager.nix`

**Security**: The dedicated IP is securely stored in 1Password Personal vault (item: "NordVPN Dedicated IP")

**To customize the settings**:
1. Edit the `EnvironmentVariables` section in your nix configuration
2. To disable: uncomment `SKIP_VPN_AUTOCONNECT = "true";`
3. For GlobalProtect priority: uncomment `GLOBALPROTECT_ACTIVE = "true";`
4. Rebuild with: `ns` (nix switch)

**To update the dedicated IP**:
```bash
op item edit "NordVPN Dedicated IP" --vault="Personal" password="new.ip.address.here"
```

**Features of the LaunchAgent integration**:
- Runs automatically at login (with delay for system stability)
- Securely retrieves dedicated IP from 1Password
- Enhanced VPN conflict detection (avoids multiple VPN connections)
- No GUI interaction required for NordVPN
- Logs to `~/.nordvpn-autoconnect-out.log` and `~/.nordvpn-autoconnect-err.log`
- Easily enabled/disabled in one place
- Managed declaratively with nix
- No sensitive information in configuration files

### Option 3: Alternative Nix Configuration

For other nix setups, integrate into your nix configuration by adding the script to your startup programs.

## Script Behavior

1. **Validation**: Checks if NordVPN CLI is available and dedicated IP is set
2. **Skip Checks**: Evaluates environment variables and process detection for conflicts
3. **Status Check**: Determines current VPN connection status
4. **Idempotent Connection**: Only connects if not already connected to target IP
5. **Conflict Resolution**: Disconnects from other VPNs before connecting
6. **Connection**: Attempts connection with retry logic
7. **Autoconnect Setup**: Configures NordVPN autoconnect feature
8. **Logging**: Records all activities to `~/.nordvpn-autoconnect.log`

## Logging

The script logs all activities to `~/.nordvpn-autoconnect.log` with timestamps. Check this file for troubleshooting:

```bash
tail -f ~/.nordvpn-autoconnect.log
```

## Troubleshooting

### Common Issues

1. **NordVPN CLI not found**: Install NordVPN CLI and ensure it's in PATH
2. **Permission denied**: Make sure script is executable (`chmod +x nordvpn-autoconnect.sh`)
3. **Invalid dedicated IP**: Verify your dedicated IP address is correct
4. **Connection failures**: Check NordVPN account status and internet connectivity

### Debug Mode

Run with verbose output:

```bash
bash -x ./nordvpn-autoconnect.sh
```

## Security Considerations

- The script does not store credentials
- Uses existing NordVPN CLI authentication
- Logs are written to user's home directory
- No network traffic interception

## Integration with Nix

To integrate this script into your nix configuration, you can:

1. **Add as a startup service**: Include in your nix-darwin or home-manager configuration
2. **Environment management**: Set environment variables through nix
3. **Package management**: Ensure NordVPN CLI is installed via nix

Example nix integration snippet:

```nix
# In your home-manager configuration
home.sessionVariables = {
  NORDVPN_DEDICATED_IP = "your.dedicated.ip.here";
};

# Add to startup programs
home.startup = [
  {
    command = "${pkgs.bash}/bin/bash ${./nordvpn-autoconnect.sh}";
    always = false;
  }
];
```

## License

This script is provided as-is for personal use. Modify as needed for your environment.

