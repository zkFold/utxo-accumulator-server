#!/usr/bin/env bash
set -e

# --- Configuration ---
REPO="zkFold/utxo-accumulator-server"
RELEASE_API="https://api.github.com/repos/$REPO/releases/latest"
RAW_BASE="https://raw.githubusercontent.com/$REPO/main"

# --- Download latest release binary ---
echo "Fetching latest release info..."
ASSET_URL=$(curl -s $RELEASE_API | grep browser_download_url | grep 'utxo-accumulator-server' | head -n1 | cut -d '"' -f4)
if [ -z "$ASSET_URL" ]; then
  echo "Could not find a suitable utxo-accumulator-server binary in the latest release." >&2
  exit 1
fi
FILENAME=$(basename "$ASSET_URL")
echo "Downloading binary: $FILENAME"
curl -L "$ASSET_URL" -o "$FILENAME"
chmod +x "$FILENAME"

# --- Download config directory ---
echo "Downloading config directory ..."
mkdir -p config
CONFIG_API="https://api.github.com/repos/$REPO/contents/config"
CONFIG_FILES=$(curl -s $CONFIG_API | grep '"name"' | cut -d '"' -f4)
for file in $CONFIG_FILES; do
  echo "  Downloading config/$file..."
  curl -L "$RAW_BASE/config/$file" -o config/$file
  sleep 0.2
done

# --- Download crs.json ---
echo "Downloading crs.json..."
curl -L "$RAW_BASE/crs.json" -o crs.json

# --- Download database/cache.json ---
echo "Creating database directory and downloading cache.json..."
mkdir -p database
curl -L "$RAW_BASE/database/cache.json" -o database/cache.json

# --- Download web/openapi/api.json ---
echo "Creating web/openapi directory and downloading api.json..."
mkdir -p web/openapi
curl -L "$RAW_BASE/web/openapi/api.json" -o web/openapi/api.json

echo "Setup complete."
