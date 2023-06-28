#!/bin/bash
source /opt/kerl/lib/24.3.4.2/activate
export BINDIR=/opt/dog/erts-12.3.2.2/bin
export HOME=/opt/dog
export HOSTKEY=d1f0e019db4f73c1ea12d5880e6c626dfaf597ba

/opt/dog/erts-12.3.2.2/bin/erlexec -boot /opt/dog/releases/02b724c47395986104e7cf50e57d6ff346f7884b/start -config /opt/dog/releases/02b724c47395986104e7cf50e57d6ff346f7884b/sys.config 
