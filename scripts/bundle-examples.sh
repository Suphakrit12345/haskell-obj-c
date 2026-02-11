#!/usr/bin/env bash
#
# Bundle each example app into a macOS .app bundle under dist/.
#
# Usage: ./scripts/bundle-examples.sh
#
# Prerequisites:
#   - Examples already built: stack build haskell-obj-c-examples
#   - Bundler already built:  stack build haskell-obj-c-bundler

set -euo pipefail

PROJ_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN_DIR="$(cd "$PROJ_ROOT" && stack path --local-install-root)/bin"
OUT_DIR="$PROJ_ROOT/dist"

mkdir -p "$OUT_DIR"

# Map from executable name to display name.
declare -A DISPLAY_NAMES=(
  [hello-world]="Hello World"
  [counter]="Counter"
  [temperature-converter]="Temperature Converter"
  [color-mixer]="Color Mixer"
  [controls-gallery]="Controls Gallery"
  [todo-list]="Todo List"
)

EXAMPLES=(hello-world counter temperature-converter color-mixer controls-gallery todo-list)

echo "Binaries: $BIN_DIR"
echo "Output:   $OUT_DIR"
echo ""

for app in "${EXAMPLES[@]}"; do
  display_name="${DISPLAY_NAMES[$app]}"
  # Convert display name to PascalCase for the .app filename (remove spaces).
  app_filename="${display_name// /}"

  printf "%-25s" "$app"

  stack exec objc-bundler -- \
    --executable "$BIN_DIR/$app" \
    --name "$display_name" \
    --identifier "com.haskell-objc.$app" \
    --output "$OUT_DIR/$app_filename.app" \
    2>/dev/null

  echo "OK  -> dist/$app_filename.app"
done

echo ""
echo "Done. App bundles in $OUT_DIR/"
