# textus-user-account

Textusのユーザー管理コンポーネント開発リポジトリです。

目的:

1. Cozy/CNCFの機能を実運用に近い形で使い、過不足を洗い出す
2. 必要に応じて `sbt-cozy` / `cozy` / `cloud-native-component-framework` を拡張する
3. 副次的に、配布可能なコンポーネント成果物（CAR/SAR）を生成する

## 依存コンポーネント（ローカル開発）

- build plugin: `/Users/asami/src/dev2026/sbt-cozy`
- model compiler: `/Users/asami/src/dev2025/cozy`
- runtime foundation: `/Users/asami/src/dev2025/cloud-native-component-framework`

補足:

- 現在の`textus-user-account`は`sbt-cozy -> cozy modeler-scala`への委譲で生成している。
- `src/main/cozy/user-account.cml`は拡張子が`.cml`でも、内容は`cozy` modelerが読むDoxスタイルで記述する。

## 初期セットアップ

```bash
./scripts/bootstrap-local-deps.sh
# cozy系依存を省略する場合:
WITH_COZY=false ./scripts/bootstrap-local-deps.sh
```

## 開発フロー

```bash
sbt compile
sbt cozyGenerate
sbt cozyBuildCAR
sbt cozyBuildSAR
```

`cozyGenerate`の入力は `src/main/cozy` 配下、最小モデルは
`src/main/cozy/user-account.cml` を起点にする。

## Scripted Driver (CRUD)

`command` 経由の CRUD 検証は `check-crud.sh` で再現できます。

```bash
sh check-crud.sh
```

ドライバMainは以下を使用:

- マウント確認: `org.textus.useraccount.UserAccountCommandMain`
- CRUD本体(既定): `org.goldenport.cncf.cli.TextusUserAccountSyncCommandMain`
- 代替(await): `org.goldenport.cncf.cli.TextusUserAccountAwaitCommandMain`

ドライバ切替:

```bash
TEXTUS_CLI_DRIVER=sync sh check-crud.sh
TEXTUS_CLI_DRIVER=await sh check-crud.sh
# 必要なら await の待機時間(ms)を拡張
TEXTUS_AWAIT_TIMEOUT_MILLIS=120000 TEXTUS_CLI_DRIVER=await sh check-crud.sh
```

## 検証ログ

Cozy/CNCFの不足や拡張要望は `docs/notes/gap-log.md` に記録する。
