# ActionCall Store Access Inventory Note

Date: 2026-04-07

## Summary

This note inventories where `textus-user-account` action logic still performs
 direct domain-store access behavior.

The purpose is not to criticize the current implementation. The current code is
 already structured enough to expose the next step clearly:

- ordinary domain access should move behind protected internal DSL entry points;
- current `ActionCall` implementations should become orchestration logic that uses
  those protected DSLs.

## Scope

The current inventory is based on:

- [ComponentFactory.scala](/Users/asami/src/dev2026/textus-user-account/src/main/scala/org/simplemodeling/textus/useraccount/ComponentFactory.scala)

This file contains both:

- action-call implementations;
- local helper methods that directly drive entity persistence and search.

## Current Direct Access Patterns

### Create Path

`createUserAndCredential` performs:

- `entity_create(user)`
- `entity_create(credential)`

This path also performs supporting search before creation:

- `requireEmailAvailable(email)`
- `entity_search[UserAccountEntity](..., Query(query))`

This means the current create flow already mixes:

- business orchestration;
- uniqueness check;
- entity creation;
- credential side-effect creation.

Candidate future DSL split:

- `UserAccountDsl.register(...)`
- `CredentialDsl.provisionForUser(...)`
- `UserAccountDsl.requireEmailAvailable(...)`

### Status Update Path

`updateUserStatus` performs:

- `entity_load[UserAccountEntity](userId)`
- `entity_update(userId, patch)`

This path mixes:

- current-state read;
- state-machine validation;
- persistence update.

Candidate future DSL split:

- `UserAccountDsl.loadForUpdate(userId)`
- `UserAccountDsl.transitionStatus(userId, targetStatus)`

### Delete Path

`deleteUserAccount` performs:

- `credentialsForUser(userId)`
- `entity_search[CredentialEntity](..., Query(query))`
- `entity_delete(credential.id)` for each credential
- `entity_delete(userId)`

This path mixes:

- dependent-resource discovery;
- dependent-resource delete;
- working-set cleanup;
- primary entity delete.

Candidate future DSL split:

- `CredentialDsl.listByUser(userId)`
- `CredentialDsl.deleteAllForUser(userId)`
- `UserAccountDsl.delete(userId)`
- `UserSessionDsl.logout(userId)`

### List/Search Path

`listUserAccounts` performs:

- `entity_search[UserAccountEntity](..., Query.plan(...))`

This path already resembles a future query DSL, but authorization and visibility are
 still not enforced at query/result layer. That is the missing part.

Candidate future DSL split:

- `UserAccountDsl.search(queryPlan)`

### Login Path

`login` performs:

- `userByEmail(email)`
- `entity_search[UserAccountEntity](..., Query(query))`
- `credentialByUserAndPassword(user.id, password)`
- `entity_search[CredentialEntity](..., Query(query))`
- `addLoggedInUser(user)`

This path mixes:

- account lookup;
- credential verification;
- status gate;
- session/working-set activation.

Candidate future DSL split:

- `UserAccountDsl.findByEmail(email)`
- `CredentialDsl.authenticate(userId, password)`
- `UserSessionDsl.login(user)`

### Password Change Path

`changePassword` performs:

- `credentialByUserAndPassword(userId, current)`
- `entity_search[CredentialEntity](..., Query(query))`
- `entity_update(credential.id, patch)`

This path mixes:

- current user resolution;
- current password verification;
- credential update.

Candidate future DSL split:

- `CredentialDsl.verifyCurrentPassword(userId, password)`
- `CredentialDsl.changePassword(userId, newPassword)`

### Current Account Read Path

`getMyAccount` performs:

- `entity_load[UserAccountEntity](userId)`

This is the simplest direct access case and the clearest example of where an
 internal DSL should sit.

Candidate future DSL split:

- `UserAccountDsl.getCurrent(userId)`

## Direct Store-Adjacent Access

The implementation also performs direct store-adjacent memory access for the working
set:

- `component.entitySpace.entityOption(...).storage.memoryRealm.put(user)`
- `component.entitySpace.entityOption(...).storage.memoryRealm.remove(userId)`

These are not ordinary domain-object CRUD operations, but they are still runtime
storage manipulation and should eventually be wrapped by a dedicated internal DSL.

Candidate future DSL split:

- `UserSessionDsl.login(user)`
- `UserSessionDsl.logout(userId)`

## Authorization Implications

The inventory shows that ordinary application logic still reaches:

- load;
- search;
- create;
- update;
- delete;
- memory-realm mutation.

This is exactly why the authorization boundary should move downward.

If these paths are wrapped by protected internal DSL entry points, then:

- owner/group/permission/role/privilege checks can be enforced once;
- search/list result visibility can be centralized;
- action-call logic becomes smaller and less security-sensitive.

## Proposed Internal DSL Modules

For `textus-user-account`, the natural protected internal DSL split appears to be:

- `UserAccountDsl`
- `CredentialDsl`
- `UserSessionDsl`

Suggested responsibilities:

- `UserAccountDsl`
  - create
  - load
  - search
  - update status
  - delete
  - uniqueness checks
- `CredentialDsl`
  - provision
  - authenticate
  - list by user
  - change password
  - delete by user
- `UserSessionDsl`
  - login
  - logout
  - current-user resolution
  - working-set maintenance

## Transitional Interpretation

The current implementation is still acceptable because:

- action-level authorization exists;
- entity-scoped authorization exists;
- manager-only policy exists;
- `SimpleEntity` owner-oriented helper exists.

However, this inventory makes it clear that ordinary data access still lives too
close to action code.

That is the next refactoring target.
