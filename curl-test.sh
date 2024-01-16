#! /usr/bin/env bash

HOST=localhost:9000
ID=03C97EC3-93DE-CD4E-AB62-A00C6F8CE902

curl --silent --request OPTIONS \
  --header 'Content-Type: application/json' \
  http://$HOST/users
echo
curl --silent --request GET \
  --header 'Content-Type: application/json' \
  http://$HOST/users
echo
curl --silent --request GET \
  --header 'Content-Type: application/json' \
  http://$HOST/users/$ID
echo
curl --silent --request POST \
  --header 'Content-Type: application/json' \
  --data '{"name":"posted name 0","created_at":"1970-01-01 00:00", "email":"name0@domain.org"}' \
  http://$HOST/users
echo
curl --silent --request PATCH \
  --header 'Content-Type: application/json' \
  --data '{"name":"patched name 1","created_at":"1970-01-01 00:01", "email":"name1@domain.org"}' \
  http://$HOST/users/$ID
echo

curl --silent --request POST \
  --header 'Content-Type: application/json' \
  --data '{"name":"posted name 2","created_at":"1970-01-01 00:02", "email":"name2@domain.org"}' \
  http://$HOST/users
echo

