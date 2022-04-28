-module(dog_docker_tests).

-include_lib("eunit/include/eunit.hrl").

dog_docker_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_C) ->
             [
                ?_assertMatch(true, dog_docker:any_docker_containers())
              , ?_assertMatch([#{"host_ip" := _,
                                 "nets" := [#{"bridge_interface" := _}]}], dog_docker:container_networks())
              , ?_assertMatch(#{"bridge_interface" := _,
                                "container_network" := _}, dog_docker:default_network())
              , ?_assertMatch("mock-bridge-name", dog_docker:for_default_network(dog_docker:default_network(), <<"{{bridge_interface}}">>))
              , ?_assertMatch([#{"bridge_interface" := _}, #{"bridge_interface" := _}], dog_docker:get_interfaces(dog_docker:container_networks(), dog_docker:default_network()))
              , ?_assertMatch(true, dog_docker:is_docker_instance())
              , ?_assertMatch({<<"*nat", _/binary>>, <<":DOCKER", _/binary>>}, list_to_tuple([iolist_to_binary(X) || X <- tuple_to_list(dog_docker:iptables())]))
              , ?_assertMatch([<<"127.0.0.1">>], dog_docker:per_container(dog_docker:container_networks(), dog_docker:default_network(), <<"{{host_ip}}">>))
              , ?_assertMatch("br-lo\nmock-bridge-name", dog_docker:per_network(dog_docker:container_networks(), dog_docker:default_network(), <<"{{bridge_interface}}">>))
             ]
     end}.

setup() ->
    dog_fixture:setup([hackney_docker, docker_container]).

teardown(Context) ->
    dog_fixture:teardown(Context).
