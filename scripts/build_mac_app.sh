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
