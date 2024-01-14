#! /usr/bin/env bash

HOST=localhost:9000
ID=03C97EC3-93DE-CD4E-AB62-A00C6F8CE902

curl --silent --request OPTIONS http://$HOST/users
echo
curl --silent --request GET http://$HOST/users
echo
curl --silent --request GET http://$HOST/users/$ID
echo
curl --silent --request POST --data "name=curl+test&email=posted&created_at=1980" http://$HOST/users
echo
curl --silent --request PATCH --data "name=patched&email=patched&created_at=1998-01-01+12:00" http://$HOST/users/$ID
echo

curl --silent --request POST http://$HOST/users \
   --header 'Content-Type: application/json' \
   --data '{"name":"posted name","created_at":"1970-01-01 00:00", "email":"name@domain.org"}'
echo

