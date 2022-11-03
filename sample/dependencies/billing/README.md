Billing sample model
====================

Run:

```shell
$ lein run -c config.edn
```

Post request:

```
POST http://localhost:8080/_e/Billing.Core/Purchase

{"Billing.Core/Purchase": {"Product": "k01", "Count": 10}}
```

The above request will return the updated stock and the debited bank account details.