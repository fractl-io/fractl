# Sample Requests:

### StaffLogin Auth:
```json
http://localhost:8000/_e/Ecommerce.Authentication/StaffLogin
{
    "Ecommerce.Authentication/StaffLogin": {
        "UserName": "superadmin",
        "Password": "imightnotbehere123"
    }
}
```

```json
[
    {
        "status": "ok",
        "result": [
            {
                "type-*-tag-*-": "entity",
                "name": [
                    "Kernel",
                    "Authentication"
                ],
                "Owner": {
                    "type-*-tag-*-": "entity",
                    "name": [
                        "Ecommerce.Store",
                        "Staff"
                    ],
                    "Name": "Super Admin",
                    "UserName": "superadmin",
                    "Email": "superadmin@v8.io",
                    "Password": "imightnotbehere123",
                    "Group": "admin",
                    "Id": "d2f1a0c1-fa5c-4ba3-84f0-e6fdb7a43475"
                },
                "ExpirySeconds": 300,
                "Id": "989a74c3-1ea4-4a5c-b7ec-da1b948c444c",
                "Issued": "2021-08-09T01:40:56.397908"
            }
        ],
        "message": null
    }
]
```

### StoreRepresentativeLogin
```json
http://localhost:8000/_e/Ecommerce.Authentication/StoreRepresentativeLogin
{
    "Ecommerce.Authentication/StoreRepresentativeLogin": {
        "UserName": "ClintE",
        "Password": "getoffmylawn12"
    }
}
```

```json
[
    {
        "status": "ok",
        "result": [
            {
                "type-*-tag-*-": "entity",
                "name": [
                    "Kernel",
                    "Authentication"
                ],
                "Owner": {
                    "ActiveSince": "2021-08-09T01:37:35.687113",
                    "Group": "owner",
                    "Id": "48b27484-2ba4-436a-9422-64906c621845",
                    "name": [
                        "Ecommerce.Store",
                        "StoreRepresentative"
                    ],
                    "Email": "clint@eastwood.com",
                    "UserName": "ClintE",
                    "Password": "getoffmylawn12",
                    "Name": "Clint Eastwood",
                    "type-*-tag-*-": "entity"
                },
                "ExpirySeconds": 300,
                "Id": "af143cde-2524-4a71-a9e1-0522004885a9",
                "Issued": "2021-08-09T01:42:11.250432"
            }
        ],
        "message": null
    }
]
```

### Upsert_StoreInfo

```json
http://localhost:8000/_e/Ecommerce.Store/Upsert_StoreInfo
{
    "Ecommerce.Store/Upsert_StoreInfo": {
        "Instance": {
            "Ecommerce.Store/StoreInfo": {
                "Name": "Mumford Farmers and Sons",
                "Location": "Sunny Side, California",
                "StoreRatings": "4.5 stars",
                "ItemsSold": "700 items"
            }
        },
        "EventContext": {
            "Auth": "{{staff}}"
        }
    }
}
```

```json
[
    {
        "status": "ok",
        "result": [
            {
                "type-*-tag-*-": "entity",
                "name": "Ecommerce.Store/StoreInfo",
                "Name": "Mumford Farmers and Sons",
                "Location": "Sunny Side, California",
                "StoreRatings": "4.5 stars",
                "ItemsSold": "700 items",
                "Id": "37627e83-6002-4644-9b34-f5fd8c7abe5c"
            }
        ],
        "message": null
    }
]
```

### Upsert_Product
```json
http://localhost:8000/_e/Ecommerce.Store/Upsert_Product
{
    "Ecommerce.Store/Upsert_Product": {
        "Instance": {
            "Ecommerce.Store/Product": {
                "productId": "ce8fe2c8-f068-11eb-9a03-0242ac130003",
                "Brand": "Organic foods",
                "Price": 12,
                "Name": "Packet of premium carrots",
                "ManufacturerName": "Mumford Farmers and Sons",
                "ManufacturerId": "d78c0e9c-f068-11eb-9a03-0242ac130003",
                "Weight": "2 kg"
            }
        },
        "EventContext": {
            "Auth": "{{staff}}"
        }
    }
}
```
```json
[
    {
        "status": "ok",
        "result": [
            {
                "ManufacturerId": "d78c0e9c-f068-11eb-9a03-0242ac130003",
                "Id": "e4a6248c-5763-47cf-ac56-ecce2366ad62",
                "ManufacturerName": "Mumford Farmers and Sons",
                "name": "Ecommerce.Store/Product",
                "Brand": "Organic foods",
                "productId": "ce8fe2c8-f068-11eb-9a03-0242ac130003",
                "Price": 12.0,
                "Name": "Packet of premium carrots",
                "Weight": "2 kg",
                "type-*-tag-*-": "entity"
            }
        ],
        "message": null
    }
]
```

### Upsert_Inventory

```json
http://localhost:8000/_e/Ecommerce.Store/Upsert_Inventory
{
    "Ecommerce.Store/Upsert_Inventory": {
        "Instance": {
            "Ecommerce.Store/Inventory": {
                "Shipping": true,
                "Stock": 7,
                "Product": "{{product}}",
                "Store": "{{store}}"
            }
        },
        "EventContext": {
            "Auth": "{{staff}}"
        }
    }
}
```

```json
[
    {
        "status": "ok",
        "result": [
            {
                "type-*-tag-*-": "entity",
                "name": "Ecommerce.Store/Inventory",
                "Shipping": true,
                "Stock": 7,
                "Product": "e4a6248c-5763-47cf-ac56-ecce2366ad62",
                "Store": "37627e83-6002-4644-9b34-f5fd8c7abe5c",
                "Id": "24e36241-e9fa-42cb-a795-ee43e78403ab"
            }
        ],
        "message": null
    }
]
```

### Upsert_UserInfo

```json
http://localhost:8000/_e/Ecommerce.User/Upsert_UserInfo
{
    "Ecommerce.User/Upsert_UserInfo": {
        "Instance": {
            "Ecommerce.User/UserInfo": {
                "UserName": "mac",
                "Password": "itsalwayssunny",
                "Email": "mac@alwaysunny.com",
                "Name": "Ronald McDonald",
                "CurrentAddress": "Philadelphia, Street no. 10",
                "ShippingAddress": "Philadelphia, Street no. 10",
                "Group": "member"
            }
        }
    }
}
```

```json
[
    {
        "status": "ok",
        "result": [
            {
                "Group": "member",
                "ShippingAddress": "Philadelphia, Street no. 10",
                "Id": "9e878130-05ff-4614-9bf4-1548e68458ed",
                "name": "Ecommerce.User/UserInfo",
                "Email": "mac@alwaysunny.com",
                "CurrentAddress": "Philadelphia, Street no. 10",
                "UserName": "mac",
                "Password": "itsalwayssunny",
                "Name": "Ronald McDonald",
                "type-*-tag-*-": "entity"
            }
        ],
        "message": null
    }
]
```

### UserLogin Auth

```json
http://localhost:8000/_e/Ecommerce.Authentication/UserLogin
{
    "Ecommerce.Authentication/UserLogin": {
        "UserName": "mac",
        "Password": "itsalwayssunny"
    }
}
```

```json
[
    {
        "status": "ok",
        "result": [
            {
                "type-*-tag-*-": "entity",
                "name": [
                    "Kernel",
                    "Authentication"
                ],
                "Owner": {
                    "Group": "member",
                    "ShippingAddress": "Philadelphia, Street no. 10",
                    "Id": "9e878130-05ff-4614-9bf4-1548e68458ed",
                    "name": [
                        "Ecommerce.User",
                        "UserInfo"
                    ],
                    "Email": "mac@alwaysunny.com",
                    "CurrentAddress": "Philadelphia, Street no. 10",
                    "UserName": "mac",
                    "Password": "itsalwayssunny",
                    "Name": "Ronald McDonald",
                    "type-*-tag-*-": "entity"
                },
                "ExpirySeconds": 300,
                "Id": "fcfdb6c3-e23f-423b-8586-8fbfbcf977f6",
                "Issued": "2021-08-09T01:48:30.940169"
            }
        ],
        "message": null
    }
]
```

## AddCart

```json
http://localhost:8000/_e/Ecommerce.Cart/AddCart
{
    "Ecommerce.Cart/AddCart": {
        "Inventory": "24e36241-e9fa-42cb-a795-ee43e78403ab",
        "NoofItems": 2,
        "Price": 12
    }
}
```

```json
[
  {
    "status": "ok",
    "result": [
      {
        "type-*-tag-*-": "entity",
        "name": [
          "Ecommerce.Cart",
          "CartInfo"
        ],
        "Price": 12.0,
        "NoofItems": 2,
        "Inventory": "24e36241-e9fa-42cb-a795-ee43e78403ab",
        "AddedAt": "2021-08-09T02:40:02.789907",
        "Currency": "USD",
        "Id": "26573e91-1385-4dbd-a87d-cbc38c89f6b0"
      }
    ],
    "message": null
  }
]
```

### AddSameProductToCart

```json
http://localhost:8000/_e/Ecommerce.Cart/AddSameProductToCart
{
    "Ecommerce.Cart/AddSameProductToCart": {
        "Cart": "44894bc5-d777-4621-bf49-35e00e3383dd",
        "Inventory": "24e36241-e9fa-42cb-a795-ee43e78403ab",
        "NoofItems": 3,
        "Price": 36.0
    }
}
```

```json
[
    {
        "status": "ok",
        "result": [
            {
                "type-*-tag-*-": "entity",
                "name": [
                    "Ecommerce.Cart",
                    "CartInfo"
                ],
                "Price": 48.0,
                "NoofItems": 5,
                "Inventory": "24e36241-e9fa-42cb-a795-ee43e78403ab",
                "AddedAt": "2021-08-09T02:54:18.923549",
                "Currency": "USD",
                "Id": "c48fefd4-2b12-4a71-8eb6-fd8383a1dd72"
            }
        ],
        "message": null
    }
]
```

### AllCartItems

```json
http://localhost:8000/_e/Ecommerce.Cart/AllCartItems
{
    "Ecommerce.Cart/AllCartItems": {}
}
```

```json
[
    {
        "status": "ok",
        "result": [
            {
                "type-*-tag-*-": "entity",
                "name": [
                    "Ecommerce.Cart",
                    "CartInfo"
                ],
                "Inventory": "b1f0a35b-489b-4804-80b1-526cbc0587a3",
                "AddedAt": "2021-08-09T03:15:08.102247",
                "NoofItems": 2,
                "Price": 12.0,
                "Currency": "USD",
                "Id": "7d755337-2ed8-4095-88a1-33d717453713"
            }
        ],
        "message": null
    }
]
```

### TotalFromCart

```json
http://localhost:8000/_e/Ecommerce.Cart/TotalFromCart
{
"Ecommerce.Cart/TotalFromCart": {}
}
```

### CheckoutItem

```json
http://localhost:8000/_e/Ecommerce.Checkout/CheckoutItem
{
  "Ecommerce.Checkout/CheckoutItem": {
    "Cart": "874db1a4-9324-4228-acb6-2430552869f7",
    "User": "8a744e56-b6c8-4ab9-8c93-baaadf00d917",
    "ShippingDate": "2021-07-29",
    "PaymentMethod": "Cash on Delivery",
    "EventContext": {
        "Auth": "{{user_auth}}"
    }
  }
}
```


```json
[
    {
        "status": "ok",
        "result": [
            {
                "type-*-tag-*-": "entity",
                "name": [
                    "Ecommerce.Cart",
                    "CartInfo"
                ],
                "Inventory": "fad23b9f-4f8e-4727-9bbe-8fc14c4c3cd4",
                "AddedAt": "2021-08-09T03:56:51.291027",
                "NoofItems": 2,
                "Price": 12.0,
                "Currency": "USD",
                "Id": "874db1a4-9324-4228-acb6-2430552869f7"
            }
        ],
        "message": null
    }
]
```

### CheckoutItem
```json
http://localhost:8000/_e/Ecommerce.Checkout/CheckoutItem
{
    "Ecommerce.Checkout/CheckoutItem": {
        "Cart": "874db1a4-9324-4228-acb6-2430552869f7",
        "User": "8a744e56-b6c8-4ab9-8c93-baaadf00d917",
        "ShippingDate": "2021-07-29",
        "PaymentMethod": "Cash on Delivery",
        "EventContext": {
            "Auth": "dd6c9ce3-a421-4a55-88e1-55bf81264244"
        }
    }
}
```