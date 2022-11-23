#!/bin/bash
certs=$(curl -s -d '{"fqdn": "dog-agent"}' http://csc:9001/2015-03-31/functions/myfunction/invocations)
echo $certs | jq -r .server_key > /etc/dog/private/server.key
echo $certs | jq -r .server_crt > /etc/dog/certs/server.crt
echo $certs | jq -r .ca_crt >     /etc/dog/certs/ca.crt
