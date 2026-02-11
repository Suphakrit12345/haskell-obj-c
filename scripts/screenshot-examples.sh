#!/usr/bin/env bash
#
# Run each example app, screenshot its window, then kill it.
# Outputs PNGs to docs/images/<example>.png.
#
# Usage: ./scripts/screenshot-examples.sh
#
# Prerequisites:
#   - macOS (uses screencapture + osascript)
#   - Examples already built: stack build haskell-obj-c-examples

set -euo pipefail

PROJ_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT_DIR="$PROJ_ROOT/docs/images"
mkdir -p "$OUT_DIR"

BIN_DIR="$(cd "$PROJ_ROOT" && stack path --local-install-root)/bin"

EXAMPLES=(hello-world counter temperature-converter color-mixer controls-gallery)

# Use JXA to query CoreGraphics for the window ID owned by a given PID.
# Returns the kCGWindowNumber of the first on-screen, layer-0 window for that PID.
get_window_id() {
  local pid=$1
  osascript -l JavaScript -e "
    ObjC.import('CoreGraphics');
    var pid = $pid;
    var wins = ObjC.deepUnwrap(
      \$.CGWindowListCopyWindowInfo(
        \$.kCGWindowListOptionOnScreenOnly | \$.kCGWindowListExcludeDesktopElements,
        0
      )
    );
    var result = '';
    for (var i = 0; i < wins.length; i++) {
      var w = wins[i];
      if (w.kCGWindowOwnerPID === pid && w.kCGWindowLayer === 0) {
        result = '' + w.kCGWindowNumber;
        break;
      }
    }
    result;
  " 2>/dev/null
}

# Poll until a window appears for the given PID, or time out.
wait_for_window() {
  local pid=$1
  local max_wait=20
  local elapsed=0
  while [ $elapsed -lt $max_wait ]; do
    # Check the process is still alive
    if ! kill -0 "$pid" 2>/dev/null; then
      echo "  Process $pid exited before a window appeared" >&2
      return 1
    fi
    local wid
    wid=$(get_window_id "$pid")
    if [ -n "$wid" ]; then
      echo "$wid"
      return 0
    fi
    sleep 1
    elapsed=$((elapsed + 1))
  done
  echo "  Timed out waiting for window (PID $pid)" >&2
  return 1
}

echo "Binaries: $BIN_DIR"
echo ""

for example in "${EXAMPLES[@]}"; do
  printf "%-25s" "$example"

  # Run the app directly (not through stack exec) to keep the PID stable.
  "$BIN_DIR/$example" &
  APP_PID=$!

  WINID=$(wait_for_window "$APP_PID") || {
    kill "$APP_PID" 2>/dev/null || true
    wait "$APP_PID" 2>/dev/null || true
    echo "FAILED (no window)"
    continue
  }

  # Give the window a moment to finish rendering.
  sleep 2

  # Capture the window: -x (no sound), -o (no shadow).
  screencapture -x -o -l"$WINID" "$OUT_DIR/$example.png"

  # Tear down.
  kill "$APP_PID" 2>/dev/null || true
  wait "$APP_PID" 2>/dev/null || true

  echo "OK  -> docs/images/$example.png"
done

echo ""
echo "Done. Screenshots in $OUT_DIR/"
