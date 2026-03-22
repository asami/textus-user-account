# Cozy/CNCF Gap Log

## Record Template

- Date:
- Scenario:
- Expected:
- Actual:
- Gap type: (missing feature / behavior mismatch / DX issue / docs issue)
- Candidate fix:
- Target repo: (sbt-cozy / cozy / cloud-native-component-framework / this repo)

## Entries

- Date: 2026-03-22
- Scenario: 初期セットアップ
- Expected: textus-user-accountから最小モデルを生成・ビルドできる
- Actual: `goldenport-cncf_3:0.3.14-SNAPSHOT` が未公開で `sbt compile` が依存解決失敗
- Gap type: DX issue
- Candidate fix: `scripts/bootstrap-local-deps.sh` で `sbt-cozy` / `cloud-native-component-framework` / `cozy` の `publishLocal` を統一実行
- Target repo: this repo

- Date: 2026-03-22
- Scenario: 最小モデルから配布物生成
- Expected: CML定義からCAR/SARが生成できる
- Actual: `cozyBuildCAR` / `cozyBuildSAR` が成功し、`target/` に成果物生成
- Gap type: なし（確認）
- Candidate fix: 次フェーズでユーザー管理ユースケースを追加して仕様妥当性を確認
- Target repo: this repo

- Date: 2026-03-22
- Scenario: sbt-cozyをcozy委譲へ移行
- Expected: sbt-cozy内で生成ロジックを二重保持しない
- Actual: `cozy`委譲をdefault化し、`legacy`は互換モードへ分離。委譲実行には`simple-modeler`/`kaleidox`/`cozy`のローカル公開が必要
- Gap type: DX issue
- Candidate fix: `scripts/bootstrap-local-deps.sh` にcozy依存スタックの`publishLocal`を追加
- Target repo: sbt-cozy / this repo

- Date: 2026-03-22
- Scenario: TU-02 package/layout要件
- Expected: Component package overrideとEntityValue `${package}/entity/*` が同時に有効
- Actual: `kaleidox` COMPONENT `package` 取得、`cozy`でComponent package反映、`simple-modeler`でEntityValueを`entity/*`へ統一
- Gap type: なし（実装・検証完了）
- Candidate fix: 不要（TU-02 DONE）
- Target repo: kaleidox / cozy / simple-modeler / this repo
