#!/bin/sh
grep create /etc/dog/ipset.txt | awk '{print $2}' | sort | uniq > /etc/dog/1.tmp
sudo /sbin/ipset list -name | sort | uniq > /etc/dog/2.tmp
for name in $(comm -1 -3 /etc/dog/1.tmp /etc/dog/2.tmp );
do 
  echo destroy $name;
done > /etc/dog/ipset_cleanup.txt
#cat /etc/dog/ipset_cleanup.txt | sudo /sbin/ipset restore
input="/etc/dog/ipset_cleanup.txt"
while IFS= read -r line
do
  echo "$line"
  sudo /sbin/ipset $line
done < "$input"
