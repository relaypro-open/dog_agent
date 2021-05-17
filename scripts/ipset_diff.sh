#!/bin/bash
diff  <(sudo /sbin/ipset save | grep add | sed 's/\/32//g' | sed 's/\/128//g' | sort -n ) <(cat /etc/dog/ipset.txt | grep add | sed 's/n / /g'| sed 's/\/32//g'| sed 's/\/128//g'| sort -n )
