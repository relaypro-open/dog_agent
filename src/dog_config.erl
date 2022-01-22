-module(dog_config).

-include("dog.hrl").

-export([
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


-export([
         restart_mq_services/4,
         start_config_service/1,
         stop_config_service/0
        ]).

restart_mq_services(Environment, Location, Group, Hostkey) ->
    supervisor:terminate_child(dog_sup, config_agent),
    %stop_config_service(),
    start_config_service(Hostkey),
    dog_iptables:stop_iptables_service(),
    dog_iptables:start_iptables_service(Environment, Location, Group, Hostkey),
    dog_file_transfer:stop_file_transfer_service(),
    dog_file_transfer:start_file_transfer_service(Environment, Location, Group, Hostkey),
    supervisor:terminate_child(dog_sup, ips_agent),
    supervisor:restart_child(dog_sup, dog_ips_agent),
    supervisor:restart_child(dog_sup, dog_config_agent).

%% @doc watches for config from queue.
-spec do_watch_config() -> atom().

do_watch_config() ->
    Environment = environment(),
    Location = location(),
    Group = group(),
    Hostkey = hostkey(),
    restart_mq_services(Environment, Location, Group, Hostkey).

start_config_service(Hostkey) ->
    turtle_service:new(dog_turtle_sup,dog_turtle_sup:config_service_spec(Hostkey)).

stop_config_service() ->
    turtle_service:stop(dog_turtle_sup,config_service).

subscriber_loop(_RoutingKey, _CType, Payload, State) -> 
    lager:debug("Payload: ~p", [Payload]),
    Proplist = binary_to_term(Payload),
    lager:debug("Proplist: ~p", [Proplist]),
    UserData = proplists:get_value(user_data, Proplist),
    lager:debug("UserData: ~p", [UserData]),
    Config = maps:get(config, UserData),
    lager:debug("Config: ~p", [Config]),
    Group = maps:get(<<"group">>, Config),
    Location = maps:get(<<"location">>, Config),
    Environment = maps:get(<<"environment">>, Config),
    Hostkey = maps:get(<<"hostkey">>, Config),
    dog_config_agent:set_group(Group),
    dog_config_agent:set_location(Location),
    dog_config_agent:set_environment(Environment),
    dog_config_agent:set_hostkey(Hostkey),
    write_config_file(Group, Location, Environment,
                      Hostkey),
    %Restarts ips_agent to have config change take effect, and immediate request of new iptables
    restart_mq_services(Environment, Location, Group, Hostkey),
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
    lager:info("read_config_file()"),
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
