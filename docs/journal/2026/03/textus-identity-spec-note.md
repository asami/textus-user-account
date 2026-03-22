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

# 7. Authentication Flow

```
Login(Command)
  → Aggregate
    → password verification
    → Event(LoginSucceeded / LoginFailed)
```

---

# 8. Event Model

- UserCreated
- PasswordChanged
- UserActivated
- UserDeactivated
- LoginSucceeded
- LoginFailed

---

# 9. Execution Model

All operations:

```
Operation → ActionCall → Task → Job
```

---

# 10. Security Rules

- password stored as hash only
- plain password exists only in Command
- View must not expose password

---

# 11. Future Extensions

- credential separation
- session management
- MFA
- OAuth

---

End.
