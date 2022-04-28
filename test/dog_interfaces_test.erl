-module(dog_interfaces_test).

-include_lib("eunit/include/eunit.hrl").

dog_app_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(C) ->
             [
              ?_assertMatch("availability-zone", dog_interfaces:ec2_availability_zone())
              , ?_assertMatch({"instance-id", "availability-zone", [<<"security-group">>], <<"owner-id">>}, dog_interfaces:ec2_info())

              , ?_assertMatch("instance-id",       dog_interfaces:ec2_instance_id())
              , ?_assertEqual([dog_fixture:mac()], dog_interfaces:ec2_macs())

              , ?_assertEqual([list_to_binary(dog_fixture:public_ip())], dog_interfaces:ec2_public_ipv4())

              , ?_assertMatch([<<"security-group">>],          dog_interfaces:ec2_security_group_ids())
              , ?_assertMatch(<<"owner-id">>,                  dog_interfaces:ec2_owner_id())
              , ?_assertEqual(dog_fixture:hostname(),          dog_interfaces:fqdn())
              , ?_assertEqual({ok, dog_fixture:environment()}, dog_interfaces:get_environment_key())
              , ?_assertEqual({ok, dog_fixture:hostname()},    dog_interfaces:get_fqdn())
              , ?_assertEqual({ok, dog_fixture:group()},       dog_interfaces:get_group_key())
              , ?_assertEqual({ok, dog_fixture:hostkey()},     dog_interfaces:get_host_key())

              , ?_assertEqual({ok, dog_fixture:interfaces_expect()}, dog_interfaces:get_interfaces(<<"local">>, []))
              , ?_assertEqual({ok, dog_fixture:interfaces_expect() ++ [{<<"ec2_public_ipv4">>,[list_to_binary(dog_fixture:public_ip())]}]}, dog_interfaces:get_interfaces(<<"ec2">>, []))

              , ?_assertEqual({ok, dog_fixture:interfaces()},        dog_interfaces:get_interfaces_with_ips())
              , ?_assertEqual({ok, dog_fixture:interfaces_expect()}, dog_interfaces:get_local_interfaces())
              , ?_assertEqual({ok, dog_fixture:location()},          dog_interfaces:get_location_key())

              , ?_assertMatch(<<"ec2">>, dog_interfaces:get_provider())
              , ?_assertMatch(ok,        dog_interfaces:ip_to_queue())
              , ?_assertMatch(false,     dog_interfaces:is_docker_instance())
              , ?_assertMatch(true,      dog_interfaces:is_ec2_instance())
              , ?_assertMatch(false,     dog_interfaces:is_ec2_private_instance())
              , ?_assertMatch(false,     dog_interfaces:is_softlayer_instance())
              , ?_assertMatch(ok,        dog_interfaces:publish_to_queue(<<>>))
             ]
     end}.

setup() ->
    dog_fixture:setup([dog_version, dog_envconfig, hackney_ec2, inet_ifs, thumper_publish]).

teardown(Context) ->
    dog_fixture:teardown(Context).
