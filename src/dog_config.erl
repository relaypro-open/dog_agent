-module(dog_config).

-include("dog.hrl").

-export([
         do_init_config/0, 
         do_watch_config/0, 
         environment/0,
         get_config/0, 
         group/0, 
         hostkey/0, 
         location/0,
         read_config_file/0, 
         routing_key/0,
         subscriber_loop/4,
         write_config_file/4
        ]).

-spec do_init_config() -> atom().
do_init_config() ->
    Environment = environment(),
    Location = location(),
    Group = group(),
    Hostkey = hostkey(),
    ?LOG_DEBUG("~p, ~p, ~p, ~p",[Environment, Location, Group, Hostkey]),
    dog_turtle_sup:start_mq_services(Environment, Location, Group, Hostkey),
    ok.

%% @doc watches for config from queue.
-spec do_watch_config() -> atom().

do_watch_config() ->
    Environment = environment(),
    Location = location(),
    Group = group(),
    Hostkey = hostkey(),
    ?LOG_DEBUG("~p, ~p, ~p, ~p",[Environment, Location, Group, Hostkey]),
    dog_turtle_sup:restart_mq_services(Environment, Location, Group, Hostkey),
    ok.

subscriber_loop(_RoutingKey, _CType, Payload, State) -> 
    ?LOG_DEBUG("Payload: ~p", [Payload]),
    Proplist = binary_to_term(Payload),
    ?LOG_DEBUG("Proplist: ~p", [Proplist]),
    UserData = proplists:get_value(user_data, Proplist),
    ?LOG_DEBUG("UserData: ~p", [UserData]),
    Config = maps:get(config, UserData),
    ?LOG_DEBUG("Config: ~p", [Config]),
    Group = maps:get(<<"group">>, Config),
    Location = maps:get(<<"location">>, Config),
    Environment = maps:get(<<"environment">>, Config),
    Hostkey = maps:get(<<"hostkey">>, Config),
    dog_agent:set_group(Group),
    dog_agent:set_location(Location),
    dog_agent:set_environment(Environment),
    dog_agent:set_hostkey(Hostkey),
    write_config_file(Group, Location, Environment,
                      Hostkey),
    %Restarts ips_agent to have config change take effect, and immediate request of new iptables
    dog_turtle_sup:restart_mq_services(Environment, Location, Group, Hostkey),
    {ack,State}.

-spec write_config_file(_, _, _, _) -> ok.

write_config_file(Group, Location, Environment,
                  Hostkey) ->
    ConfigMap = #{group => Group, location => Location,
                  environment => Environment, hostkey => Hostkey},
    ok = file:write_file(?CONFIG_FILE,
                         jsx:encode(ConfigMap)),
    ok.

-spec read_config_file() -> Map :: {ok, map()} | atom().

read_config_file() ->
    ?LOG_INFO("read_config_file()"),
    case file:read_file(?CONFIG_FILE) of
        {ok, ConfigJson} ->
            {ok, jsx:decode(ConfigJson, [return_maps])};
        _ -> file_read_error
    end.

-spec get_config() -> any().

get_config() ->
    ConfigMap = case dog_config:read_config_file() of
                    {ok, Map} -> Map;
                    file_read_error ->
                        #{<<"group">> => <<"">>, <<"location">> => <<"">>,
                          <<"environment">> => <<"">>, <<"hostkey">> => <<"">>}
                end,
    ConfigMap.

map_to_routing_key(ConfigMap) ->
    binary:list_to_bin([maps:get(<<"environment">>,
                                 ConfigMap),
                        <<".">>, maps:get(<<"location">>, ConfigMap), <<".">>,
                        maps:get(<<"group">>, ConfigMap), <<".">>,
                        maps:get(<<"hostkey">>, ConfigMap)]).

routing_key() ->
    ConfigMap = get_config(),
    RoutingKey = map_to_routing_key(ConfigMap),
    RoutingKey.

environment() ->
    ConfigMap = get_config(),
    Environment = maps:get(<<"environment">>, ConfigMap,
                           <<"*">>),
    Environment.

location() ->
    ConfigMap = get_config(),
    Location = maps:get(<<"location">>, ConfigMap, <<"*">>),
    Location.

group() ->
    ConfigMap = get_config(),
    Group = maps:get(<<"group">>, ConfigMap, <<"">>),
    Group.

hostkey() ->
    ConfigMap = get_config(),
    Hostkey = maps:get(<<"hostkey">>, ConfigMap, <<"">>),
    Hostkey.
