-module(dog_ips_agent_tests).

-include_lib("eunit/include/eunit.hrl").

dog_ips_agent_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_C) ->
             [
               ?_assertMatch(ok, dog_ips_agent:keepalive())
              %, ?_assertMatch(ok, dog_ips_agent:watch_config())
              , ?_assertMatch(ok, dog_ips_agent:watch_interfaces())
              , ?_assertMatch(ok, dog_ips_agent:watch_iptables())
              , ?_assertMatch(ok, dog_ips_agent:create_ipsets([]))
              , ?_assertMatch(<<"3e75aaf2416a450fb747b9ef46b0bb31b2b8c016c809ad248cf726054e25fc0c">>, dog_ips_agent:read_hash())
             ]
     end}.

setup() ->
    dog_fixture:setup([dog_ec2_app]).
    %dog_fixture:setup([
    %     dog_liet:dog_os(),
    %     dog_liet:timer_nosleep(),
    %     dog_liet:file_write_nothing(),
    %     dog_liet:file_read_config_map(),
    %     dog_liet:inet_ifs(),
    %     dog_liet:dog_turtle_allow(),
    %     dog_liet:turtle_publish(),
    %     dog_liet:hackney_ec2(),
    %     dog_liet:lager_none(),
    %     dog_liet:dog_version(),
    %     dog_liet:turtle_noconnect()
    %                  ]).

teardown(Context) ->
    dog_fixture:teardown(Context).
