#!/usr/bin/env bash
set -euo pipefail

exec "${CNCF_LAUNCHER_CMD:-cncf}" "$@"
