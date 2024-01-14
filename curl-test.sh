#! /usr/bin/env bash

HOST=localhost:9000

curl --silent --request OPTIONS http://$HOST/users
echo
curl --silent --request GET http://$HOST/users
echo
curl --silent --request GET http://$HOST/users/031F2E84-01FB-7344-8CDD-B92FAC09007B
echo
curl --silent --request POST --data "name=curl+test" http://$HOST/users
echo
curl --silent --request PATCH --data "name=patched" http://$HOST/users/031F2E84-01FB-7344-8CDD-B92FAC09007B
echo
