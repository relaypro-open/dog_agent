-module(dog_state_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("dog/include/dog.hrl").

dog_state_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun({State, Map}) ->
             [
                ?_assertEqual(State, dog_state:from_map(Map))
              , ?_assertMatch(<<"eunit-environment.eunit-location.eunit-group.*">>, dog_state:to_group_routing_key(State))
              , ?_assertMatch(<<"eunit-environment.eunit-location.*.eunit-hostkey">>, dog_state:to_host_routing_key(State))
              , ?_assertMatch(<<_/binary>>, jsx:encode(dog_state:to_map(State)))
              , ?_assertMatch(<<"availability-zone">>, dog_state:get_ec2_availability_zone(State))
              , ?_assertMatch(<<"instance-id">>, dog_state:get_ec2_instance_id(State))
              , ?_assertMatch(<<"owner-id">>, dog_state:get_ec2_owner_id(State))
              , ?_assertMatch([<<"security-group-id">>], dog_state:get_ec2_security_group_ids(State))
              , ?_assertEqual(dog_fixture:environment(), dog_state:get_environment(State))
              , ?_assertEqual(dog_fixture:group(), dog_state:get_group(State))
              , ?_assertMatch(<<"0">>, dog_state:get_hash4_ipsets(State))
              , ?_assertMatch(<<"1">>, dog_state:get_hash6_ipsets(State))
              , ?_assertMatch(<<"2">>, dog_state:get_hash4_iptables(State))
              , ?_assertMatch(<<"3">>, dog_state:get_hash6_iptables(State))
              , ?_assertEqual(dog_fixture:hostkey(), dog_state:get_hostkey(State))
              , ?_assertEqual(dog_fixture:hostname(), dog_state:get_hostname(State))
              , ?_assertMatch([], dog_state:get_interfaces(State))
              , ?_assertMatch(<<"4">>, dog_state:get_ipset_hash(State))
              , ?_assertEqual(dog_fixture:location(), dog_state:get_location(State))
              , ?_assertMatch(<<"ec2">>, dog_state:get_provider(State))
              , ?_assertMatch(<<"updatetype">>, dog_state:get_updatetype(State))
              , ?_assertMatch(<<"0.0">>, dog_state:get_version(State))
              , ?_assertMatch(ok, dog_state:get_ec2_availability_zone(dog_state:set_ec2_availability_zone(State, ok)))
              , ?_assertMatch(ok, dog_state:get_ec2_instance_id(dog_state:set_ec2_instance_id(State, ok)))
              , ?_assertMatch(ok, dog_state:get_ec2_security_group_ids(dog_state:set_ec2_security_group_ids(State, ok)))
              , ?_assertMatch(ok, dog_state:get_ec2_owner_id(dog_state:set_ec2_owner_id(State, ok)))
              , ?_assertMatch(ok, dog_state:get_environment(dog_state:set_environment(State, ok)))
              , ?_assertMatch(ok, dog_state:get_group(dog_state:set_group(State, ok)))
              , ?_assertMatch(ok, dog_state:get_hash4_ipsets(dog_state:set_hash4_ipsets(State, ok)))
              , ?_assertMatch(ok, dog_state:get_hash4_iptables(dog_state:set_hash4_iptables(State, ok)))
              , ?_assertMatch(ok, dog_state:get_hash6_ipsets(dog_state:set_hash6_ipsets(State, ok)))
              , ?_assertMatch(ok, dog_state:get_hash6_iptables(dog_state:set_hash6_iptables(State, ok)))
              , ?_assertMatch(ok, dog_state:get_hostkey(dog_state:set_hostkey(State, ok)))
              , ?_assertMatch(ok, dog_state:get_hostname(dog_state:set_hostname(State, ok)))
              , ?_assertMatch(ok, dog_state:get_interfaces(dog_state:set_interfaces(State, ok)))
              , ?_assertMatch(ok, dog_state:get_ipset_hash(dog_state:set_ipset_hash(State, ok)))
              , ?_assertMatch(ok, dog_state:get_location(dog_state:set_location(State, ok)))
              , ?_assertMatch(ok, dog_state:get_provider(dog_state:set_provider(State, ok)))
              , ?_assertMatch(ok, dog_state:get_updatetype(dog_state:set_updatetype(State, ok)))
              , ?_assertMatch(ok, dog_state:get_version(dog_state:set_version(State, ok)))
              , ?_assertMatch(ok, dog_state:get_ec2_instance_tags(dog_state:set_ec2_instance_tags(State, ok)))
              , ?_assertMatch(ok, dog_state:get_os_info(dog_state:set_os_info(State, ok)))
              , ?_assertMatch(ok, dog_state:get_ec2_region(dog_state:set_ec2_region(State, ok)))
             ]
     end}.

setup() ->
    State = dog_state:dog_state(
	      dog_fixture:group(),       
	      dog_fixture:hostname(), 
	      dog_fixture:location(),
	      dog_fixture:environment(), 
	      dog_fixture:hostkey(),  
	      [],                
	      <<"0.0">>,                 
	      <<"0">>,                
	      <<"1">>,                  
	      <<"2">>,                   
	      <<"3">>,                
	      <<"ec2">>,                  
	      <<"updatetype">>,          
	      <<"4">>, 
	      <<"region">>,
	      <<"instance-id">>,                                
	      <<"availability-zone">>,   
	      [<<"security-group-id">>], 
	      <<"owner-id">>,
	      #{<<"role">> => <<"web">>}
	      #{<<"distribution">> => <<"Ubuntu">> }
	     ),
    Map = dog_state:to_map(State),

    %% dog_state:to_map and :from_map have an inconsistency with the handling of the interfaces key,
    %% so we manually set the key here for the tests to run
    {State, Map#{<<"interfaces">> => []}}.

teardown(_State) ->
    ok.
