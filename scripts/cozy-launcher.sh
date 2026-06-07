#!/usr/bin/env bash
set -euo pipefail

exec "${COZY_LAUNCHER_CMD:-cozy}" "$@"
