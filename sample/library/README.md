This directory contains the components that define the V8 Library model.
This model, once mature enough, should move to a dedicated repository.

## Sample requests

### User Login Auth

```json
POST _e/Library.Ledger/UserLogin

{
    "Library.Ledger/UserLogin": {
        "UserName": "publisher12",
        "Password": "imightnotbehere23"
    }
}
```

### Member Login Auth
```json
POST /_e/Library.Ledger/MemberLogin

{
  "Library.Ledger/MemberLogin": {
    "UserName": "fdyo",
    "Password": "raskolnikov"
  }
}
```

### Create user

```json
POST _e/Library.Identity/Upsert_User

{
  "Library.Identity/Upsert_User": {
    "Instance": {
      "Library.Identity/User": {
        "UserName": "publisher12",
        "Group": "incharge", ;; or "general" or "intern"
        "Password": "imightnotbehere23",
        "Email": "publisher12@v8.io"
      }
    },
    "EventContext": {
      "Auth": {{uuid-from-user-login-auth}}
    }
  }
}
```

### Create Book

```json
POST _e/Library.Catalog/Upsert_Book

{
    "Library.Catalog/Upsert_Book": {
        "Instance": {
            "Library.Catalog/Book": {
                "Name": "The Godfather",
                "Publisher": "{{publisher}}"
            }
        }
    }
}
```

### Create Member

```json
POST _e/Library.Identity/Upsert_Member

{
  "Library.Identity/Upsert_Member": {
    "Instance": {
      "Library.Identity/Member": {
        "Name": "Fyodor Dyostovesky",
        "UserName": "fdyo",
        "Password": "raskolnikov",
        "Email": "fdyo@v8.io",
        "DOB": "1868-01-02",
        "Designation": "life"
                ;; or "individual" or "family" or
                ;; "remote" or "oversees" or "supported" or,
                ;; "associate" or "temporary"
      }
    },
    "EventContext": {
      "Auth": {{uuid-from-member-login-auth}}
    }
  }
}
```

### Policy Setups:
#### UserCreation Policy(For creating Users):

```json
POST _e/Library.Ledger/UserCreationPolicy

{
  "Library.Ledger/UserCreationPolicy": {}
}
```

#### Service Policy (For creating members):
```json
POST _e/Library.Ledger/ServicePolicy

{
    "Library.Ledger/ServicePolicy": {}
}
```

#### Checkout Policy (For Checkingout books):
```json
POST _e/Library.Ledger/CheckoutPolicy

{
    "Library.Ledger/CheckoutPolicy": {}
}
```

#### Checkin Policy (For checkingIn books):
```json
POST _e/Library.Ledger/CheckinPolicy

{
  "Library.Ledger/CheckinPolicy": {}
}
```

#### Logging Policy:
```json
POST _e/Library.Ledger/RBACPolicyLogging

{
    "Library.Ledger/RBACPolicyLogging": {}
}
```

### Checkout Book

```json
POST _e/Library.Ledger/CheckoutBook

{
    "Library.Ledger/CheckoutBook": {
        "Book": "{{book}}",
        "Member": "{{member}}",
        "Designation": "life",
                ;; or "individual" or "family" or
                ;; "remote" or "oversees" or "supported" or,
                ;; "associate" or "temporary"
        "Backend": "Postmark or SendGrid",
        "Receiver": "{{email}}",
        "Subject": "Book Checked out",
        "Text": "You have checked out {{book}}. Please return it after 2 weeks.",
        "To": "{{phone number}}",
        "EventContext": {
          "Auth": {{uuid-from-member-login-auth}}
      }
    }
}
```

### Checkin Book

```json
POST _e/Library.Ledger/CheckinBook

{
    "Library.Ledger/CheckinBook": {
        "Book": "{{book}}",
        "Member": "{{member}}",
        "Designation": "life",
                ;; or "individual" or "family" or
                ;; "remote" or "oversees" or "supported" or,
                ;; "associate" or "temporary"
        "Backend": "Postmark or SendGrid",
        "Receiver": "{{email}}",
        "Subject": "Book Checked in",
        "Text": "You have checked in {{book name}}. Thank you.",
        "To": "{{phone number}}",
        "EventContext": {
          "Auth": {{uuid-from-member-login-auth}}
        }
    }
}
```

### List all checkouts

```json
POST _e/Library.Ledger/AllCheckouts

{
    "Library.Ledger/AllCheckouts": {}
}
```
### Fetch Books for a specific publisher

```json
POST _e/Library.Catalog/ListBooks

{"Library.Catalog/ListBooks":
  {"Publisher": "123e4567-e89b-12d3-a456-426614174000"}}
```
