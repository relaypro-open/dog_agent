%-include_lib("eunit/include/eunit.hrl").

-define(IPsExchange, <<"ips">>).
-define(IptablesExchange, <<"iptables">>).
-define(ConfigExchange, <<"config">>).

-define(RUNDIR, "/etc/dog").
-define(CONFIG_FILE, "/etc/dog/config.json").

-define(SERVER, ?MODULE).
-define(CMD(S), os:cmd(S)).

-define(PID_FILE, "/var/run/dog/dog.pid").

-define(EC2_METADATA_BASE_URL, "http://169.254.169.254").
-define(IBM_METADATA_BASE_URL, "https://api.service.softlayer.com").
%For mock testing:
%-define(EC2_METADATA_BASE_URL, "http://localhost:3000").
%-define(IBM_METADATA_BASE_URL, "http://localhost:3000").
