#!/bin/bash
echo "CERT_SOURCE: ${CERT_SOURCE}"
#Using envsubst template instead of sys.config.src so we get errors when bad values/template.
cat /data/config/sys.config.template | envsubst > /data/config/dog.config
/data/scripts/validate_config.escript /data/config/dog.config

if [ "$CERT_SOURCE" == "csc" ]; then 
    /bin/bash -c "sed -i 's/hostname/`hostname`/g' /etc/dog/config.json" && /bin/bash -c "/etc/rabbitmq/csc_certs.sh" && exec /opt/dog/bin/dog foreground;
    #/bin/bash -c "sed -i 's/hostname/`hostname`/g' /etc/dog/config.json" && /bin/bash -c "/etc/rabbitmq/csc_certs.sh" && /bin/bash -c "sleep infinity";
    #/bin/bash -c "/etc/rabbitmq/csc_certs.sh" && /bin/bash -c "/ec2_mock/_build/default/rel/ec2_mock/bin/ec2_mock daemon" && -c "/opt/dog/bin/dog foreground";
    #/bin/bash -c "/etc/rabbitmq/csc_certs.sh" && /bin/bash -c "/opt/dog/bin/dog foreground";
    #/bin/bash -c "/etc/rabbitmq/csc_certs.sh" && /bin/bash -c "sleep infinity"
else
    exec /opt/dog/bin/dog foreground;
fi
