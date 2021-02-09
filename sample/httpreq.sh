#!/bin/bash

curl --header "Content-Type: application/json" --request POST --data '{"Sample.Simple/Upsert_E2": {"Instance": {"Sample.Simple/E2": {"X":100}}}}' http://localhost:8000/_e/Sample.Simple/Upsert_E2

#curl --header "Content-Type: application/json" --request POST --data '{"Sample.Simple/Lookup_E2": {"Id": "ee1e35a0-979a-4c20-975d-174ee0155aeb"}}' http://localhost:8000/_e/Sample.Simple/Lookup_E2
