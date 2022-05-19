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

teardown(Context) ->
    dog_fixture:teardown(Context).
