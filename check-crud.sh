#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT_DIR"

KEEP_SCRIPTED_ARTIFACTS="${KEEP_SCRIPTED_ARTIFACTS:-0}"

# NOTE:
# DataStoreSpace currently reads sqlite path via ConfigurationAccess.getString and
# obtains wrapped values (e.g. StringValue(db.sqlite)).
# Keep this as-is to preserve the current runtime behavior in reproducible tests.
mkdir -p .cncf
cat > .cncf/config.conf <<'EOF'
"cncf.datastore.sqlite.path" = "db.sqlite"
EOF

DB_FILE="StringValue(db.sqlite)"
rm -f "$DB_FILE"
cleanup() {
  if [[ "$KEEP_SCRIPTED_ARTIFACTS" != "1" ]]; then
    rm -f "$DB_FILE"
    rm -rf .cncf
  fi
}
trap cleanup EXIT

DRIVER_MODE="${TEXTUS_CLI_DRIVER:-sync}" # sync | await
case "$DRIVER_MODE" in
  await)
    DRIVER_MAIN="org.goldenport.cncf.cli.TextusUserAccountAwaitCommandMain"
    ;;
  sync)
    DRIVER_MAIN="org.goldenport.cncf.cli.TextusUserAccountSyncCommandMain"
    ;;
  *)
    echo "Unknown TEXTUS_CLI_DRIVER: $DRIVER_MODE" >&2
    exit 2
    ;;
esac

echo "[1/7] compile"
sbt --batch compile

echo "[2/7] domain mount check"
sbt --batch "runMain org.textus.useraccount.UserAccountCommandMain command admin.component.list"

echo "[3/7] create user"
CREATE_OUT="$(
  sbt --batch "runMain ${DRIVER_MAIN} domain.entity.createUserAccount --displayName Alice --email alice@example.com --status active" \
  2>&1
)" || {
  echo "$CREATE_OUT"
  echo "CREATE failed"
  exit 1
}
echo "$CREATE_OUT"

USER_ID="$(
  {
    echo "$CREATE_OUT" | rg -o '"id":"[^"]+"' | head -n1 | sed -E 's/^"id":"([^"]+)"$/\1/' || true
    echo "$CREATE_OUT" | rg -o "id:[[:space:]]+[^[:space:]]+" | head -n1 | awk '{print $2}' || true
  } | head -n1
)"
if [[ -z "${USER_ID:-}" ]]; then
  echo "Failed to parse user id from create output" >&2
  exit 1
fi
echo "user id: $USER_ID"

echo "[4/7] load user"
sbt --batch "runMain ${DRIVER_MAIN} domain.entity.loadUserAccount --id ${USER_ID}"

echo "[5/7] update user"
sbt --batch "runMain ${DRIVER_MAIN} domain.entity.updateUserAccount --id ${USER_ID} --status suspended"
sbt --batch "runMain ${DRIVER_MAIN} domain.entity.loadUserAccount --id ${USER_ID}" | rg -q "suspended"

echo "[6/7] delete user (hard)"
sbt --batch "runMain ${DRIVER_MAIN} domain.entity.deleteUserAccountHard --id ${USER_ID}"

echo "[7/7] verify sqlite"
if [[ ! -f "$DB_FILE" ]]; then
  echo "SQLite file not found: $DB_FILE" >&2
  exit 1
fi
sqlite3 "$DB_FILE" ".tables"

echo "CRUD scripted check completed."
