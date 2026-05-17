# textus-user-account

Textusのユーザー管理コンポーネント開発リポジトリです。

目的:

1. Cozy/CNCFの機能を実運用に近い形で使い、過不足を洗い出す
2. 必要に応じて `sbt-cozy` / `cozy` / `cloud-native-component-framework` を拡張する
3. 副次的に、配布可能なコンポーネント成果物（CAR/SAR）を生成する

## 依存コンポーネント（ローカル開発）

- build plugin: `/Users/asami/src/dev2026/sbt-cozy`
- runtime foundation: `/Users/asami/src/dev2025/cloud-native-component-framework`

補足:

- 現在の`textus-user-account`は`sbt-cozy -> cozy modeler-scala`への委譲で生成している。
- `src/main/cozy/user-account.cml`は拡張子が`.cml`でも、内容は`cozy` modelerが読むDoxスタイルで記述する。
- 生成は通常 `cozy` コマンドを使うため追加設定は不要。
- ローカルの cozy 開発ディレクトリを継続利用する場合は、`.cozy/config.yaml` の
  `generation.delegate.project_dir` に指定する。一時的な上書きだけなら
  `SBT_COZY_PROJECT_DIR=/path/to/cozy` も使える。

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

## 配備フロー

`textus-user-account` を配布用 CAR として登録する場合は、まず release 版の
version を `build.sbt` に設定する。例:

```scala
ThisBuild / version := "0.1.1"
```

Git に登録するコンポーネント情報は `project.yaml` で管理する。
`.cozy/config.yaml` は個人環境のローカルパスだけを置く。

```yaml
project:
  name: textus-user-account
  title: Textus User Account Component
  kind: car
  path: components/textus/user-account

publication:
  source_manifest:
    enabled: false

packaging:
  kind: car
  car:
    source_dir: src/main/car

warehouse:
  repository_artifacts:
    include:
      - car
    modules:
      - textus-user-account
```

ローカル出力先は Git 管理外の `.cozy/config.yaml` に置く。

```yaml
publication:
  output: /Users/asami/src/dev2025/simplemodeling-org/src/main/publication

warehouse:
  repository: /Users/asami/src/maven-repository
```

配備時の標準手順:

```bash
sbt test
sbt cozyBuildCAR
sbt cozyDistributeCAR
sbt cozyPublishProject
sbt cozyIndexWarehouse
```

`cozyDistributeCAR` は CNCF が component repository として参照する CAR を
以下の形で配置する。

```text
/Users/asami/src/maven-repository/repository/car/textus-user-account/<version>/textus-user-account-<version>.car
```

CAR はデフォルトで依存ライブラリを同梱しない。`textus-user-account` の CAR は
`component/main.jar`、`config/`、`web/` を中心に構成し、実行時の
Scala/CNCF/HTTP/DB driver 等は CNCF サーバー側の runtime classpath から供給する。
コンポーネント固有の追加ライブラリが必要な場合は、CAR に jar を同梱せず、
`project.yaml` の `packaging.car.dependencies.shared` または
`packaging.car.dependencies.local` に Maven coordinate を定義する。
CNCF runtime は CAR 内の `component-dependencies.yaml` を読み、`shared` は
runtime 共通 dependency pool、`local` は component-local classloader に解決する。
`provided` は CNCF / simplemodeling など runtime 側が提供する ABI ライブラリを
記録する用途で、CNCF は解決・追加ロードしない。
Maven repository で公開できない商用・社内・一時 jar は例外として CAR の `lib/`
に配置できる。この場合も Component 専用 local classloader 側で読み込まれ、
CNCF runtime ABI package は parent-first の保護対象になる。

```yaml
packaging:
  car:
    dependencies:
      provided:
        - org.goldenport:goldenport-cncf_3:<cncf-version>
      shared:
        - org.postgresql:postgresql:42.7.3
      local:
        - com.example:legacy-driver:1.2.0
      repositories:
        - maven-central
```

`shared` の version conflict を fail-fast にするのは暫定運用であり、本運用では
CNCF 側で dependency mediation / compatibility validation を設計する。

`cozyPublishProject` と `cozyIndexWarehouse` は SimpleModeling.org で
コンポーネント情報と配布 artifact 情報を公開するための source を更新する。

```text
/Users/asami/src/dev2025/simplemodeling-org/src/main/publication/textus-user-account.json
```

公開 repository 側では、CNCF は以下を取得する。

```text
https://www.simplemodeling.org/repository/car/textus-user-account/maven-metadata.xml
https://www.simplemodeling.org/repository/car/textus-user-account/<version>/textus-user-account-<version>.car
```

## Scripted Driver (CRUD)

`command` 経由の CRUD 検証は `check-crud.sh` で再現できます。
テスト用 `config.conf` は `target/scripted/config/*/config.conf` に生成され、
CNCF起動パラメタ `--cncf.config.file=...` で各Mainに渡されます。
作業ファイル（sqlite等）は `target/scripted/work/*` 配下に作成されます。

```bash
sh check-crud.sh
```

ドライバMainは以下を使用:

- マウント確認: `org.simplemodeling.textus.useraccount.cli.UserAccountCommandMain`
- CRUD本体(既定): `org.simplemodeling.textus.useraccount.cli.TextusUserAccountSyncCommandMain`
- 代替(await): `org.simplemodeling.textus.useraccount.cli.TextusUserAccountAwaitCommandMain`

ドライバ切替:

```bash
TEXTUS_CLI_DRIVER=sync sh check-crud.sh
TEXTUS_CLI_DRIVER=await sh check-crud.sh
# 必要なら await の待機時間(ms)を拡張
TEXTUS_AWAIT_TIMEOUT_MILLIS=120000 TEXTUS_CLI_DRIVER=await sh check-crud.sh
```

## 検証ログ

Cozy/CNCFの不足や拡張要望は `docs/notes/gap-log.md` に記録する。
