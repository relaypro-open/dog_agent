#!/bin/bash
GROUP=$(cat /etc/dog/config.json | jq -r '.group')
HOSTNAME=$(hostname -f)
echo agent:
/opt/dog/scripts/hashes.escript ; 
echo host on trainer:
curl -s http://dog-ubuntu-server.lxd:7070/api/host?name=$HOSTNAME | jq '.' ;
echo group on trainer:
curl -s http://dog-ubuntu-server.lxd:7070/api/group?name=$GROUP | jq '.'
