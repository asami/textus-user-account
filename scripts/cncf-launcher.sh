#!/usr/bin/env bash
set -euo pipefail

if [[ -n "${CNCF_LAUNCHER_CMD:-}" ]]; then
  exec "$CNCF_LAUNCHER_CMD" "$@"
fi

if [[ -n "${CNCF_LAUNCHER_DEV_DIR:-}" ]]; then
  args_file=$(mktemp "${TMPDIR:-/tmp}/cncf-launcher-args.XXXXXX")
  cleanup() {
    rm -f "$args_file"
  }
  trap cleanup EXIT
  if [[ "$#" -gt 0 ]]; then
    printf '%s\0' "$@" > "$args_file"
  else
    : > "$args_file"
  fi
  cd "$CNCF_LAUNCHER_DEV_DIR"
  export CNCF_LAUNCHER_DEV_DELEGATED=1
  export CNCF_LAUNCHER_ARGS_FILE="$args_file"
  sbt --batch run
  exit $?
fi

exec "${CNCF_LAUNCHER_CMD:-cncf}" "$@"
