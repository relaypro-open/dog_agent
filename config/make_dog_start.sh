echo "#!/bin/bash
export BINDIR=/opt/dog/erts-"${ERTS_VERSION}"/bin
export HOME=/opt/dog
export HOSTKEY=$(sha1sum /var/consul/data/pki/certs/server.crt | cut -d' ' -f1)

/opt/dog/erts-"${ERTS_VERSION}"/bin/erlexec -boot /opt/dog/releases/${DOG_ENV}-${UPSTREAM_BUILD_ID}/start -mode embedded -config /opt/dog/releases/${DOG_ENV}-${UPSTREAM_BUILD_ID}/sys.config -noinput -start_epmd false" > dog_start.sh
