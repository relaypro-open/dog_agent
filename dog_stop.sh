#!/bin/bash
/bin/kill -HUP ${cat /run/dog/dog.pid}
rm -f /run/dog/dog.pid
