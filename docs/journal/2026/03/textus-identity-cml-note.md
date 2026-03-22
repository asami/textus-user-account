# Entity

## UserAccount

### Attribute

id            :: UserId
email         :: Email
passwordHash  :: PasswordHash
status        :: UserStatus

---

### Aggregate

#### Command

createUser
changePassword
activateUser
deactivateUser
login

#### State

status

#### Invariant

email != null

---

### View

#### Attribute

id     :: UserId
email  :: Email
status :: UserStatus

#### Query

getUser
findUserByEmail


# Value

## UserId

### Attribute

value :: String

---

# Value

## Email

### Attribute

value :: String

---

# Value

## PasswordHash

### Attribute

value :: String

---

# Value

## PlainPassword

### Attribute

value :: String


# Command

## CreateUser

### Attribute

userId   :: UserId
email    :: Email
password :: PlainPassword

---

# Command

## ChangePassword

### Attribute

userId   :: UserId
password :: PlainPassword

---

# Command

## ActivateUser

### Attribute

userId :: UserId

---

# Command

## DeactivateUser

### Attribute

userId :: UserId

---

# Command

## Login

### Attribute

email    :: Email
password :: PlainPassword


# Query

## GetUser

### Attribute

userId :: UserId

---

# Query

## FindUserByEmail

### Attribute

email :: Email


# Event

## UserCreated

### Attribute

userId :: UserId
email  :: Email

---

# Event

## PasswordChanged

### Attribute

userId :: UserId

---

# Event

## UserActivated

### Attribute

userId :: UserId

---

# Event

## UserDeactivated

### Attribute

userId :: UserId

---

# Event

## LoginSucceeded

### Attribute

userId :: UserId

---

# Event

## LoginFailed

### Attribute

email :: Email


# Operation

## createUser

### Type

Command

### Input

CreateUser

---

# Operation

## changePassword

### Type

Command

### Input

ChangePassword

---

# Operation

## activateUser

### Type

Command

### Input

ActivateUser

---

# Operation

## deactivateUser

### Type

Command

### Input

DeactivateUser

---

# Operation

## login

### Type

Command

### Input

Login

---

# Operation

## getUser

### Type

Query

### Input

GetUser

---

# Operation

## findUserByEmail

### Type

Query

### Input

FindUserByEmail
