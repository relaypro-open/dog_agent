#!/bin/bash
echo "#!/bin/bash
/bin/kill -HUP \${cat /run/dog/dog.pid}
rm -f /run/dog/dog.pid" > dog_stop.sh
