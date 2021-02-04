#!/bin/bash

#curl --header "Content-Type: application/json" --request POST --data '{"Sample.Simple/Upsert_E2": {"Instance": {"Sample.Simple/E2": {"X":100}}}}' http://localhost:8000/_e/Sample.Simple/Upsert_E2

curl --header "Content-Type: application/json" --request POST --data '{"Sample.Simple/Lookup_E2": {"Id": "40ecb9b7-4b61-4b07-b05f-f0e4bd9123f3"}}' http://localhost:8000/_e/Sample.Simple/Lookup_E2
