#!/usr/bin/env bash
set -e

# --- Configuration ---
REPO="zkFold/utxo-accumulator-server"
RELEASE_API="https://api.github.com/repos/$REPO/releases/latest"
RAW_BASE="https://raw.githubusercontent.com/$REPO/main"

# --- Download latest release binary ---
echo "Fetching latest release info..."
ASSET_URL=$(curl -s $RELEASE_API | grep browser_download_url | grep -E 'linux.*amd64|linux.*x86_64|linux.*64' | head -n1 | cut -d '"' -f4)
if [ -z "$ASSET_URL" ]; then
  echo "Could not find a suitable binary in the latest release." >&2
  exit 1
fi
FILENAME=$(basename "$ASSET_URL")
echo "Downloading binary: $FILENAME"
curl -L "$ASSET_URL" -o "$FILENAME"
chmod +x "$FILENAME"

# --- Download config.yaml ---
echo "Downloading config.yaml..."
curl -L "$RAW_BASE/config.yaml" -o config.yaml

# --- Download crs.json ---
echo "Downloading crs.json..."
curl -L "$RAW_BASE/crs.json" -o crs.json

# --- Download database/cache.json ---
echo "Creating database directory and downloading cache.json..."
mkdir -p database
curl -L "$RAW_BASE/database/cache.json" -o database/cache.json

echo "Setup complete."
