#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

if [[ -z "${CNCF_VERSION:-}" ]]; then
  CNCF_VERSION_FILE="${CNCF_VERSION_FILE:-$PROJECT_ROOT/versions/cncf-version.conf}"
  if [[ -f "$CNCF_VERSION_FILE" ]]; then
    CNCF_VERSION="$(tr -d '[:space:]' < "$CNCF_VERSION_FILE")"
  else
    CNCF_VERSION="0.4.7-SNAPSHOT"
  fi
fi
export CNCF_VERSION

CNCF_RUNTIME_CLASSPATH_FILE="${CNCF_RUNTIME_CLASSPATH_FILE:-$PROJECT_ROOT/target/cncf.d/runtime-classpath.txt}"
