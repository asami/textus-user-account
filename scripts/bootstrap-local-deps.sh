#!/usr/bin/env bash
set -euo pipefail

SBT_COZY_DIR="/Users/asami/src/dev2026/sbt-cozy"
CNCF_DIR="/Users/asami/src/dev2025/cloud-native-component-framework"
COZY_DIR="/Users/asami/src/dev2025/cozy"
SIMPLE_MODELER_DIR="/Users/asami/src/dev2025/simple-modeler"
KALEIDOX_DIR="/Users/asami/src/dev2025/kaleidox"
WITH_COZY="${WITH_COZY:-true}"

echo "[bootstrap] publish sbt-cozy plugin"
(cd "${SBT_COZY_DIR}" && sbt --batch publishLocal)

echo "[bootstrap] publish CNCF runtime"
(cd "${CNCF_DIR}" && sbt --batch publishLocal)

if [[ "${WITH_COZY}" == "true" ]]; then
  echo "[bootstrap] publish simple-modeler (cozy dependency)"
  (cd "${SIMPLE_MODELER_DIR}" && sbt --batch publishLocal)

  echo "[bootstrap] publish kaleidox (cozy dependency)"
  (cd "${KALEIDOX_DIR}" && sbt --batch publishLocal)

  echo "[bootstrap] publish cozy model compiler"
  (cd "${COZY_DIR}" && sbt --batch publishLocal)
else
  echo "[bootstrap] skip cozy model compiler (set WITH_COZY=true to enable)"
fi

echo "[bootstrap] done"
