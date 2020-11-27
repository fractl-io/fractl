#!/bin/bash

curl --header "Content-Type: application/json" --request POST --data '{"Sample.Simple/Create_E": {"Instance": {"Sample.Simple/E": {"X":100}}}}' http://localhost:8080/_e/Sample.Simple/Create_E
