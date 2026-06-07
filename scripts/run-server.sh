#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=cncf-common.sh
source "$SCRIPT_DIR/cncf-common.sh"

cd "$PROJECT_ROOT"
exec "$SCRIPT_DIR/cncf-launcher.sh" \
  dev server
