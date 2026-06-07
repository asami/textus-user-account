#!/usr/bin/env bash
set -euo pipefail

: "${SBT_COZY_DIR:?Set SBT_COZY_DIR to the local sbt-cozy checkout.}"
: "${CNCF_DIR:?Set CNCF_DIR to the local cloud-native-component-framework checkout.}"
: "${COZY_DIR:?Set COZY_DIR to the local cozy checkout.}"
: "${SIMPLE_MODELER_DIR:?Set SIMPLE_MODELER_DIR to the local simple-modeler checkout.}"
: "${KALEIDOX_DIR:?Set KALEIDOX_DIR to the local kaleidox checkout.}"
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
