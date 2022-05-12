-module(dog_config_tests).

-include_lib("eunit/include/eunit.hrl").

dog_config_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(C) ->
             [
                ?_assertMatch(ok, dog_config:do_watch_config())
               
              , ?_assertEqual(dog_fixture:environment(),  dog_config:environment())
              , ?_assertEqual(dog_fixture:group(),        dog_config:group())
              , ?_assertEqual(dog_fixture:hostkey(),      dog_config:hostkey())
              , ?_assertEqual(dog_fixture:location(),     dog_config:location())

              , ?_assertEqual(dog_fixture:get_state(config_map, C),       dog_config:get_config())
              , ?_assertEqual({ok, dog_fixture:get_state(config_map, C)}, dog_config:read_config_file())

              , ?_assertEqual(list_to_binary(dog_fixture:routing_key_expect()), dog_config:routing_key())

              , ?_assertMatch(ok, dog_config:write_config_file(dog_fixture:group(),
                                                               dog_fixture:location(),
                                                               dog_fixture:environment(),
                                                               dog_fixture:hostkey()))
             ]
     end}.

setup() ->
    dog_fixture:setup([
                       dog_turtle_allow,
                       dog_turtle_service_allow,
                       file_read_config_map, 
                       file_write_nothing 
                      ]).

teardown(Context) ->
    dog_fixture:teardown(Context).
