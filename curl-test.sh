#! /usr/bin/env bash

HOST=localhost:9000

curl --request OPTIONS http://$HOST/users
curl --request GET http://$HOST/users
curl --request GET http://$HOST/users/031F2E84-01FB-7344-8CDD-B92FAC09007B
curl --request POST --data "name=curl+test" http://$HOST/users

echo 

