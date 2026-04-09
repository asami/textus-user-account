Textus Identity Subsystem Specification
=====================================

status=proposed
date=2026-03-21

---

# 1. Overview

Textus Identity is a subsystem that manages:

- user identity
- authentication (password-based)
- credential management
- session foundation (future)

This subsystem is part of Textus standard components and runs on CNCF.

---

# 2. Subsystem Structure

Subsystem:

```
textus-identity
```

Components:

```
textus-user-account
```

(future)

```
textus-credential
textus-session
```

---

# 3. Responsibilities

## Identity Subsystem

- identity lifecycle
- authentication boundary
- credential verification

---

## UserAccount Component

- user creation
- password management
- activation / deactivation
- login authentication
- refresh token rotation and reuse detection

### Session Safety

- `refreshAccessToken` remains an anonymous entrypoint because the caller presents only the refresh token
- `logout` revokes only the current access session and its linked refresh session
- `logoutAll` revokes all access sessions and refresh sessions for the target account
- refresh token rotation is mandatory
- reuse of an already-rotated refresh token is treated as a session-family compromise signal
- on detected refresh token reuse, remaining access sessions and refresh sessions for that user are revoked

---

# 4. Domain Model

## Entity

- UserAccount

## Value

- UserId
- Email
- PasswordHash
- PlainPassword

## Command

- CreateUser
- ChangePassword
- ActivateUser
- DeactivateUser
- Login

## Query

- GetUser
- FindUserByEmail

---

# 5. Aggregate (Command-side)

Aggregate:

```
UserAccountAggregate
```

Responsibilities:

- validate commands
- enforce invariants
- verify password
- emit events

---

# 6. View (Query-side)

View:

```
UserAccountView
```

Responsibilities:

- lookup by id
- lookup by email
- no password exposure

---

# 7. ViewSpace Authorization Policy

View construction inside `ViewSpace` should run with privileged internal read semantics.

This means:

- view composition may read underlying entities with framework-internal authority
- entity/object read permission is not the primary gate during view construction
- authorization is enforced when the completed view is exposed to the caller

In other words:

- build phase: privileged internal read
- exposure phase: operation/view access check

This keeps view composition stable while preserving the external security boundary at the point where data is returned.

---

# 8. Authentication Flow

```
Login(Command)
  → Aggregate
    → password verification
    → Event(LoginSucceeded / LoginFailed)
```

---

# 9. Event Model

- UserCreated
- PasswordChanged
- UserActivated
- UserDeactivated
- LoginSucceeded
- LoginFailed

---

# 10. Execution Model

All operations:

```
Operation → ActionCall → Task → Job
```

---

# 11. Security Rules

- password stored as hash only
- plain password exists only in Command
- View must not expose password

---

# 12. UserAccount Boundary And Attribute Policy

`textus-user-account` should support attributes that are generally expected in enterprise systems, especially for EC-style business applications.

`UserAccount` is still kept narrow on purpose.

It is the account/identity/security object, not the full personal profile.

Current `UserAccount` core attributes are:

- `email`
- `loginName`
- `externalSubjectId`
- `status`
- inherited `SimpleEntity` attributes and quality/security metadata

The following remain in-scope for `UserAccount` as account/security attributes:

- email verification state or timestamp
- suspension timestamp and suspension reason
- last login timestamp
- password changed timestamp
- login identifier or external subject identifier
- SMS-oriented security contact
  - `phoneNumber`
  - `phoneVerifiedAt`

These attributes are currently recorded as target design, but not yet emitted into the executable model.

Reason:

- the current generator does not safely handle these optional account/security extensions on `UserAccount`
- therefore the boundary decision is fixed first, and executable attribute expansion remains pending generator support

The following are intentionally out of scope for `UserAccount`:

- birthday
- gender
- address
- avatar
- nickname
- personal preference data

Those belong to profile-oriented models, not the account core.

If an application requires a substantially different account attribute model, it should provide its own user-management component corresponding to `textus-user-account` and configure CNCF to use that component instead.

Validity-period style attributes such as:

- `activatedAt`
- `deactivatedAt`
- `expiresAt`

should be considered on the `SimpleEntity` side rather than as `UserAccount`-specific fields.

---

# 13. UserProfile Policy

`UserProfile` is an optional companion model.

It should provide reusable generic profile information that can be attached to a user when needed, while keeping `UserAccount` itself small.

This means:

- `UserAccount` remains the identity/account/security base
- `UserProfile` holds generic personal/profile information
- applications may choose not to use `UserProfile`

Application-specific profile-like structures are also allowed, but they should not be forced into `UserAccount`.

If required user-facing information does not fit the generic `UserProfile`, the first extension path is to add an application-specific `UserRole` family object before replacing the whole account component.

Current basic `UserProfile` shape:

- `userAccountId`
- `familyName`
- `givenName`
- `familyNameKana`
- `givenNameKana`
- `displayName`
- `nickname`
- `avatarUrl`
- `birthday`
- `locale`
- `timeZone`
- `address`

---

# 14. UserRole Policy

`UserRole` is treated as an application-oriented extension point.

The role definition itself should be defined outside `textus-user-account`.
`textus-user-account` should be able to operate it internally through aggregate/view composition.

The intended shape is:

- external metadata defines role types
- that metadata may include schema information
- `textus-user-account` manages runtime/use-time composition
- aggregate and view join `UserAccount` with role information

`UserRole` is not just a string label.
It may carry attributes and can therefore represent application-specific information.

This also means application-specific profile-like data may be modeled as a kind of `UserRole` family object.

In short:

- use `textus-user-account` when its enterprise-common account/profile model is sufficient
- add application-specific `UserRole` objects when the missing data is role/profile-oriented
- replace it with a custom user-management component only when the account model itself no longer fits

---

# 14. Security Direction

Authentication and authorization are separated from profile concerns.

The current security direction is:

- `UserAccount` carries account/security core information
- `UserRole` provides application-specific role/capability context
- `UserProfile` provides profile-oriented information
- SMS is treated as a security/contact extension, not as generic profile data
- self-service operations that read/update a `UserAccount` should prefer `owner_or_manager`
  rather than plain `authenticated_only`, even when the current implementation resolves the
  target from `ExecutionContext`
- the policy expression should remain declarative in CML/operation metadata
- component/application code should interpret that metadata at the authorization boundary,
  rather than relying on per-operation generated authorization logic

This keeps the model usable while still allowing stronger security requirements than a typical ad-hoc web application account model.

Current `textus-user-account` alignment:

- `logout`
- `changePassword`
- `verifyMyEmail`
- `verifyMyPhone`
- `getMyAccount`

are treated as `owner_or_manager` operations.

Rationale:

- semantically they operate on a protected `UserAccount` object;
- the authorization rule should therefore be stated in object terms;
- using `owner_or_manager` keeps the declaration stable even if the execution path later changes
  from "current user only" to an explicit target-id route.

Additional security-direction clarification:

- `textus-user-account` is the human-subject authentication component, not the single home of all authentication;
- subsystem-to-subsystem and service-to-service authentication should use the same
  `ExecutionContext(SecurityContext)` shape, but they should be provided by separate
  system-subject components rather than by `UserAccount`;
- the common framework responsibility belongs on the CNCF side:
  - transport-level credential intake;
  - credential/session resolution;
  - normalized `SecurityContext` construction;
  - authorization and observability use at the `ActionCall` and `UnitOfWork` chokepoints;
- the component responsibility here is to resolve human-subject sessions into that shared
  `SecurityContext` model.

Current implementation scope:

- align `textus-user-account` with the shared CNCF `SecurityContext`;
- carry user session data through `ExecutionContext`;
- provide human-subject authentication/session resolution only up to that integration boundary;
- defer subsystem/service-subject authentication implementation to CNCF-side common
  session-resolution design plus a separate system-subject component.

---

# 16. Implementation Inventory

Provided now:

- `UserAccount`, `Credential`, `AccessSession`, and `RefreshSession` are modeled as separate persisted entities;
- `login` issues an access token / refresh token pair;
- `refreshAccessToken` rotates the refresh token and issues a new token pair;
- refresh-token reuse is detected and treated as a compromise signal;
- `logout` revokes only the current session pair;
- `logoutAll` revokes all session pairs for the target account;
- `changePassword` revokes all existing sessions after credential update;
- `verifyMyEmail` and `verifyMyPhone` are wired as self-service protected operations;
- `textus-user-account` provides a human-subject authentication provider for shared CNCF `SecurityContext` integration.

Deferred for the next phase:

- stronger password hashing than the current SHA-256 placeholder;
- explicit refresh-session family modeling for audit and chain management;
- SMS verification implementation beyond the current shape and note-level direction;
- subsystem/service-subject authentication components outside `textus-user-account`;
- MFA and OAuth/OIDC style external federation.

---

# 17. Future Extensions

- credential separation
- session management
- MFA
- OAuth

---

# 18. Subsystem Descriptor Wiring Direction

When `textus-user-account` is deployed inside a subsystem, it should be treated as a human-authentication base component.

The intended deployment behavior is:

- if the component is present, CNCF should auto-wire its authentication provider by convention;
- subsystem descriptors should still be able to disable it or prefer another provider;
- the descriptor should eventually become the editable deployment specification source.

In descriptor terms, `textus-user-account` should appear as a `security.authentication.providers[*]` candidate with:

- `component = textus-user-account`
- `kind = human`
- `schemes = bearer, refresh-token`

This keeps `textus-user-account` responsible for human subject/session resolution while CNCF owns common security-context composition and provider selection.

---

End.
