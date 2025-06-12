#!/bin/bash

# Test script for NordVPN autoconnect functionality
# This script tests all components without actually connecting to VPN

set -euo pipefail

echo "=== Testing NordVPN Autoconnect Script ==="
echo

# Test 1: Check script syntax
echo "1. Checking script syntax..."
if bash -n nordvpn-autoconnect.sh; then
    echo "✅ Script syntax is valid"
else
    echo "❌ Script syntax error"
    exit 1
fi
echo

# Test 2: Check 1Password availability
echo "2. Checking 1Password CLI availability..."
if command -v op >/dev/null 2>&1; then
    echo "✅ 1Password CLI found"
    if op account list >/dev/null 2>&1; then
        echo "✅ 1Password CLI authenticated"
    else
        echo "❌ Not signed in to 1Password"
        echo "Please run: op signin"
        exit 1
    fi
else
    echo "❌ 1Password CLI not found"
    exit 1
fi
echo

# Test 3: Check dedicated IP retrieval
echo "3. Testing dedicated IP retrieval from 1Password..."
if ip=$(op item get "NordVPN Dedicated IP" --vault="Personal" --field password --reveal 2>/dev/null); then
    if [[ -n "$ip" && "$ip" =~ ^[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}$ ]]; then
        echo "✅ Successfully retrieved valid IP: $ip"
    else
        echo "❌ Retrieved value is not a valid IP: $ip"
        exit 1
    fi
else
    echo "❌ Failed to retrieve IP from 1Password"
    exit 1
fi
echo

# Test 4: Check NordVPN CLI availability
echo "4. Checking NordVPN CLI availability..."
if command -v nordvpn >/dev/null 2>&1; then
    echo "✅ NordVPN CLI found"
    # Check if we can get status (without affecting connection)
    if nordvpn status >/dev/null 2>&1; then
        echo "✅ NordVPN CLI is functional"
        current_status=$(nordvpn status 2>/dev/null || echo "Disconnected")
        echo "Current VPN status: $current_status"
    else
        echo "⚠️  NordVPN CLI found but may need login"
    fi
else
    echo "❌ NordVPN CLI not found"
    echo "Please install NordVPN CLI"
    exit 1
fi
echo

# Test 5: Check for VPN conflicts
echo "5. Checking for VPN conflicts..."
vpn_conflicts=false
vpn_processes=("GlobalProtect" "Cisco" "openvpn" "wireguard" "tunnelblick")

for process in "${vpn_processes[@]}"; do
    if pgrep -fi "$process" >/dev/null 2>&1; then
        echo "⚠️  Detected VPN process: $process"
        vpn_conflicts=true
    fi
done

if command -v ifconfig >/dev/null 2>&1; then
    vpn_interfaces=$(ifconfig | grep -E '^(tun|tap|ppp|utun)[0-9]+:' | grep -v 'nordvpn' || true)
    if [[ -n "$vpn_interfaces" ]]; then
        echo "⚠️  Detected active VPN interfaces:"
        echo "$vpn_interfaces" | while read -r line; do
            echo "    $line"
        done
        vpn_conflicts=true
    fi
fi

if [[ "$vpn_conflicts" == "false" ]]; then
    echo "✅ No VPN conflicts detected"
else
    echo "⚠️  VPN conflicts detected - NordVPN would be skipped"
fi
echo

# Test 6: Check environment variables
echo "6. Checking environment variables..."
if [[ "${SKIP_VPN_AUTOCONNECT:-false}" == "true" ]]; then
    echo "⚠️  SKIP_VPN_AUTOCONNECT is set to true - VPN would be skipped"
else
    echo "✅ SKIP_VPN_AUTOCONNECT is not blocking"
fi

if [[ "${GLOBALPROTECT_ACTIVE:-false}" == "true" ]]; then
    echo "⚠️  GLOBALPROTECT_ACTIVE is set to true - VPN would be skipped"
else
    echo "✅ GLOBALPROTECT_ACTIVE is not blocking"
fi
echo

echo "=== Test Summary ==="
echo "✅ All basic functionality tests passed"
echo "✅ 1Password integration working"
echo "✅ NordVPN CLI available"
echo
echo "To test the full script (without connecting):"
echo "  SKIP_VPN_AUTOCONNECT=true ./nordvpn-autoconnect.sh"
echo
echo "To enable the LaunchAgent:"
echo "  ns  # (rebuild nix configuration)"
echo

