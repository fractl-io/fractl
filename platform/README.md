This directory contains the components that define the V8 Platform model.
This model, once mature enough, should move to a dedicated repository.

## Sample requests

### Create application

```
POST _e/Platform.AppCatalog/Upsert_Application

{"Platform.AppCatalog/Upsert_Application":
  {"Instance":
    {"Platform.AppCatalog/Application":
      {"Name": "An App",
       "Publisher": "123e4567-e89b-12d3-a456-426614174000",
       "Artifact": "UIUIUIUIUIUI"}}}}
```

### Fetch applications for a specific user

```
POST _e/Platform.AppCatalog/ListApplications

{"Platform.AppCatalog/ListApplications":
  {"Publisher": "123e4567-e89b-12d3-a456-426614174000"}}
```