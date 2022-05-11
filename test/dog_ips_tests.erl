-module(dog_ips_tests).

-include_lib("eunit/include/eunit.hrl").

dog_ips_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(#{dog_state := DogState}) ->
             [

               ?_assertMatch({ok, _}, dog_ips:do_watch_interfaces(DogState))
              %, ?_assertMatch([{_, <<"ips">>, <<"ips">>}], get_publishes())

              %% There are 2 sets of down/up here because dog_ips calls ensure_iptables_consumer twice. Bug?
              , ?_assertMatch({ok, _}, dog_ips:do_watch_iptables(DogState))
              %, ?_assertMatch([
              %                 {dog_thumper_sup, ensure_consumer, [down,'iptables.eunit-hostkey']},
              %                 {dog_thumper_sup, ensure_consumer, [up,'iptables.eunit-hostkey'|_]},
              %                 {dog_thumper_sup, ensure_consumer, [down,'iptables.eunit-hostkey']},
              %                 {dog_thumper_sup, ensure_consumer, [up,'iptables.eunit-hostkey'|_]}
              %                ], get_ensure_consumer_calls())

              , ?_assertMatch({ok, _}, dog_ips:do_keepalive(DogState))
              %, ?_assertMatch([{_, <<"ips">>, <<"ips">>}], get_publishes())

              , ?_assertEqual({ok, list_to_binary(dog_fixture:host_routing_key_expect())}, dog_ips:do_get_host_routing_key(DogState))
              , ?_assertEqual({ok, list_to_binary(dog_fixture:group_routing_key_expect())}, dog_ips:do_get_group_routing_key(DogState))
             ]
     end}.

%get_ensure_consumer_calls() ->
%    Result = [ {dog_thumper_sup, ensure_consumer, X} || {_, {dog_thumper_sup, ensure_consumer, X}, _} <- meck:history(dog_thumper_sup) ],
%    meck:reset(dog_thumper_sup),
%    Result.
%
%get_publishes() ->
%    Result = [ {binary_to_term(Payload), Exchange, RoutingKey} || {_, {thumper, publish, [Payload, Exchange, RoutingKey]}, _} <- meck:history(thumper) ],
%    meck:reset(thumper),
%    Result.

setup() ->
    %% We need to generate the dog_state, so we do it by starting the dog app, and
    %% then stopping it again so that it is not running in the background polluting
    %% our test cases.
    Fixture = dog_fixture:setup([dog_app]),
    DogState = dog_config_agent:get_state(),
    %?debugFmt("DogState: ~p",[DogState]),
    dog_fixture:teardown(Fixture),

    %% Set the provider so that our dog_ips will look for an ec2 interfaces change and
    %% publish the necessary response
    %application:set_env(dog, is_ec2_instance, true),
    DogState2 = dog_state:set_provider(DogState, <<"ec2">>),
    DogState3 = dog_state:set_interfaces(DogState2, <<"">>), %interfaces must be defined.

    Fixture2 = dog_fixture:setup([
                                  dog_version,
                                  dog_envconfig,
                                  hackney_ec2,
                                  inet_ifs,
                                  turtle_publish,
                                  dog_turtle_allow,
                                  dog_turtle_service_allow,
                                  lager_app,
                                  file_read_config_map
                                 ]),

    #{fixture => Fixture2, dog_state => DogState3}.

teardown(#{fixture := Fixture2}) ->
    dog_fixture:teardown(Fixture2).
