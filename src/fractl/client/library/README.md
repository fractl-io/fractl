This directory contains the components that define the V8 Library model.
This model, once mature enough, should move to a dedicated repository.

## Sample requests

### Create user

```
POST _e/Library.Identity/Upsert_User

{"Library.Identity/Upsert_User":
  {"Instance":
    {"Library.Identity/User":
      {"UserName": "User01",
       "Password": "kk123",
       "Email": "user01@v8.io"}}}}
```

### Create Book

```
POST _e/Library.Catalog/Upsert_Book

{"Library.Catalog/Upsert_Book":
  {"Instance":
    {"Library.Catalog/Book":
      {"Name": "An App",
       "Publisher": "123e4567-e89b-12d3-a456-426614174000",
       "Artifact": "UIUIUIUIUIUI"}}}}
```

### Fetch Books for a specific publisher

```
POST _e/Library.Catalog/ListBooks

{"Library.Catalog/ListBooks":
  {"Publisher": "123e4567-e89b-12d3-a456-426614174000"}}
```
