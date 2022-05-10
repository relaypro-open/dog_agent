-module(dog_config_agent_tests).

-include_lib("eunit/include/eunit.hrl").

dog_config_agent_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_C) ->
             [
                ?_assertEqual(list_to_binary(dog_fixture:group_routing_key_expect()), dog_config_agent:get_group_routing_key())
              , ?_assertEqual(list_to_binary(dog_fixture:host_routing_key_expect()), dog_config_agent:get_host_routing_key())

              , ?_assertEqual(dog_fixture:environment(), dog_config_agent:get_environment())
              , ?_assertEqual(dog_fixture:group(),       dog_config_agent:get_group())
              , ?_assertEqual(dog_fixture:hostkey(),     dog_config_agent:get_hostkey())
              , ?_assertEqual(dog_fixture:hostname(),    dog_config_agent:get_hostname())
              , ?_assertEqual(dog_fixture:location(),    dog_config_agent:get_location())

              , ?_assertEqual(dog_fixture:interfaces_expect(),  dog_config_agent:get_interfaces())

              , ?_assertMatch(#{}, dog_state:to_map(dog_config_agent:get_state()))

              , ?_assertEqual(dog_fixture:group_routing_key_expect(), dog_config_agent:group_routing_key())
              , ?_assertEqual(dog_fixture:host_routing_key_expect(), dog_config_agent:host_routing_key())

              , ?_assertMatch(ok, dog_config_agent:set_environment(dog_fixture:environment()))
              , ?_assertMatch(ok, dog_config_agent:set_group(dog_fixture:group()))
              , ?_assertMatch(ok, dog_config_agent:set_hostkey(dog_fixture:hostkey()))
              , ?_assertMatch(ok, dog_config_agent:set_hostname(dog_fixture:hostname()))
              , ?_assertMatch(ok, dog_config_agent:set_interfaces(dog_fixture:interfaces()))
              , ?_assertMatch(ok, dog_config_agent:set_location(dog_fixture:location()))
              , ?_assertMatch(ok, dog_config_agent:set_state(dog_config_agent:get_state()))
             ]
     end}.

setup() ->
    dog_fixture:setup([dog_app]).

teardown(Context) ->
    ?debugFmt("Context: ~p", [Context]),
    dog_fixture:teardown(Context).
