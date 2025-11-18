#!/usr/bin/env bash
set -euo pipefail

# Build a macOS .app that runs `run_payroll.command` inside a Terminal window.
# This script creates a temporary AppleScript with the repository path baked
# in and compiles it using `osacompile` into `dist/TYC Payroll Runner.app`.

ROOT_DIR="$(pwd)"
OUT_DIR="$ROOT_DIR/dist"
TEMPLATE="$ROOT_DIR/mac/run_payroll.applescript.template"
ASCRIPT="$ROOT_DIR/mac/run_payroll.applescript"
APP_NAME="TYC Payroll Runner"
APP_BUNDLE="$OUT_DIR/${APP_NAME}.app"

# Optional first argument: path to a .icns icon file to embed in the app
ICON_PATH="${1-}"

mkdir -p "$OUT_DIR"

if [ ! -f "$TEMPLATE" ]; then
  echo "AppleScript template not found: $TEMPLATE" >&2
  exit 1
fi

# Replace placeholder with the absolute project path
sed "s|__PROJECT_DIR__|$ROOT_DIR|g" "$TEMPLATE" > "$ASCRIPT"

echo "Compiling AppleScript to app: $APP_BUNDLE"
osacompile -o "$APP_BUNDLE" "$ASCRIPT"

echo "Built: $APP_BUNDLE"
echo "You can copy the .app to /Applications or double-click it to run."

# If an icon was provided, copy it into the app bundle and set CFBundleIconFile
if [ -n "$ICON_PATH" ]; then
  if [ ! -f "$ICON_PATH" ]; then
    echo "Icon file not found: $ICON_PATH" >&2
    exit 1
  fi
  ICON_NAME=$(basename "$ICON_PATH")
  ICON_BASENAME="${ICON_NAME%.*}"
  RES_DIR="$APP_BUNDLE/Contents/Resources"
  PLIST="$APP_BUNDLE/Contents/Info.plist"
  mkdir -p "$RES_DIR"
  cp "$ICON_PATH" "$RES_DIR/$ICON_NAME"
  # Use PlistBuddy to add or set CFBundleIconFile
  if /usr/libexec/PlistBuddy -c "Print :CFBundleIconFile" "$PLIST" >/dev/null 2>&1; then
    /usr/libexec/PlistBuddy -c "Set :CFBundleIconFile $ICON_BASENAME" "$PLIST"
  else
    /usr/libexec/PlistBuddy -c "Add :CFBundleIconFile string $ICON_BASENAME" "$PLIST"
  fi
  echo "Embedded icon: $ICON_NAME into app bundle"
fi
