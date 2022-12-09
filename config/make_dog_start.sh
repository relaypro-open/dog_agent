#!/bin/bash -x
set -euo pipefail
IFS=$'\n\t'

echo "#!/bin/bash
export BINDIR=/opt/dog/erts-"${erts_version}"/bin
export HOME=/opt/dog
export HOSTKEY=

/opt/dog/erts-"${erts_version}"/bin/erlexec -boot /opt/dog/releases/${dog_env}-${UPSTREAM_BUILD_ID}/start -mode embedded -config /opt/dog/releases/${dog_env}-${UPSTREAM_BUILD_ID}/sys.config -noinput -start_epmd false" > dog_start.sh
