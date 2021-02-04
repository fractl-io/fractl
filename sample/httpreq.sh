#!/bin/bash

curl --header "Content-Type: application/json" --request POST --data '{"Sample.Simple/Upsert_E2": {"Instance": {"Sample.Simple/E2": {"X":100}}}}' http://localhost:8000/_e/Sample.Simple/Upsert_E2

#curl --header "Content-Type: application/json" --request POST --data '{"Sample.Simple/Lookup_E2": {"Id": "df40b76b-b309-4857-bd33-3bf6afaf2f60"}}' http://localhost:8000/_e/Sample.Simple/Lookup_E2
