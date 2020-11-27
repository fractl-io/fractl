#!/bin/bash

curl --header "Content-Type: application/json" --request POST --data '{"Sample.Simple/E": {"X":100}}' http://localhost:8080/Sample/Simple/E
