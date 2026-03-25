# Phase 10 Checklist

- TU-01 textus-user-account contract freeze
  - Status: DONE
  - Evidence: `docs/journal/2026/03/tu-01-contract-freeze-2026-03-22.md`

- TU-02 package/layout implementation slice
  - Status: DONE
  - Target: Component package override + EntityValue `${package}/entity/*`
  - Repos: `kaleidox`, `cozy`, `simple-modeler`, `textus-user-account`

- TU-02 remaining instruction
  - Status: DONE
  - Target: operation metadata integration + runtime/projection visibility
  - Evidence:
    - `runMain org.simplemodeling.textus.useraccount.cli.UserAccountCommandMain --help` exposes the CLI command surface
    - `runMain org.simplemodeling.textus.useraccount.cli.UserAccountCommandMain command meta.schema` now returns component schema with `artifact`, `services`, `aggregateCollections`, `viewCollections`, and `operationDefinitions`
