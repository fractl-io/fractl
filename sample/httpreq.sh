#!/bin/bash

curl --header "Content-Type: application/json" --request POST --data '{"Sample.Simple/Upsert_E2": {"Instance": {"Sample.Simple/E2": {"X":100}}}}' http://localhost:8000/_e/Sample.Simple/Upsert_E2
