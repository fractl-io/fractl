#!/bin/bash

curl --header "Content-Type: application/json" --request POST --data '{"Sample.Simple/Upsert_E1": {"Instance": {"Sample.Simple/E1": {"X":100}}}}' http://localhost:8080/_e/Sample.Simple/Upsert_E1
