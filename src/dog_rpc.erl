-module(dog_rpc).

-include("dog.hrl").

-export([ensure_config_updates/0, environment/0,
         do_watch_config/0, 
         get_config/0, group/0, hostkey/0, location/0,
         read_config_file/0, routing_key/0,
         subscribe_to_config_updates/0,
        write_config_file/4]).

-export([broker_config/0]).

-spec subscribe_to_config_updates() -> ok.

subscribe_to_config_updates() ->
    Hostkey = hostkey(),
    %{ok, Hostname} = dog_interfaces:get_fqdn(),
    ok = ensure_config_consumer(Hostkey).

-spec get_config_queue() -> map().

get_config_queue() ->
    %{ok, Hostname} = dog_interfaces:get_fqdn(),
    Hostkey = hostkey(),
    QueueName = "config." ++ binary_to_list(Hostkey),
    Name = list_to_atom(QueueName),
    #{broker => default, name => Name, queue => QueueName}.

-spec ensure_config_consumer(RoutingKey ::
                             binary()) -> no_return().

ensure_config_consumer(RoutingKey) ->
    Queue = get_config_queue(),
    ok = unsubscribe_to_config_updates(Queue),
    create_config_queue(Queue),
    ok = bind_config_updates(Queue, RoutingKey),
    ok = subscribe_to_config_updates(Queue),
    ok.

ensure_config_updates() ->
    Queue = get_config_queue(),
    ok = subscribe_to_config_updates(Queue).

-spec unsubscribe_to_config_updates(Name ::
                                    map()) -> ok.

unsubscribe_to_config_updates(#{name := Name}) ->
    dog_thumper_sup:ensure_consumer(down, Name), ok.

-spec create_config_queue(QueueDefinition ::
                          map()) -> ok | no_channel.

create_config_queue(#{broker := Broker, name := Name,
                      queue := QueueName}) ->
    Op = {'queue.declare',
          [{queue, list_to_binary(QueueName)},
           {auto_delete, true}, {durable, true}]},
    case dog_thumper_sup:amqp_op(Broker, Name, [Op]) of
        ok -> 
            ok;
        {ok, _} -> 
            ok;
        {error, Error} -> 
            lager:error("Error: ~p", [Error]), 
            Error
    end.

%% @doc watches for config from queue.
-spec do_watch_config() -> atom().

do_watch_config() ->
    ok = subscribe_to_config_updates(), ok.

-spec subscribe_to_config_updates(QueueDefinition ::
                                  map()) -> atom().

subscribe_to_config_updates(#{broker := Broker,
                              name := Name, queue := QueueName}) ->
    Pid = erlang:self(),
    Callback = fun (A, B, C) ->
                       config_subscriber_callback(A, B, C, Pid)
               end,
    case dog_thumper_sup:ensure_consumer(up, Name, Broker,
                                         list_to_binary(QueueName), Callback)
    of
        {ok, _ChildPid} -> ok;
        {error, {already_up, _ChildPid}} ->
            lager:info("already_up"), ok
    end.

-spec bind_config_updates(QueueDefintion :: map(),
                          RoutingKey :: binary()) -> atom().

bind_config_updates(#{broker := Broker,
                      name := ConsumerName, queue := QueueName},
                    RoutingKey) ->
    Op = {'queue.bind',
          [{queue, list_to_binary(QueueName)},
           {exchange, <<"config">>}, {routing_key, RoutingKey}]},
    case dog_thumper_sup:amqp_op(Broker, ConsumerName, [Op])
    of
        ok -> ok;
        {ok, Reason} -> lager:debug("Reason: ~p", [Reason]), ok;
        {error, Error} -> lager:error("Error: ~p", [Error]), Error
    end.

-spec config_subscriber_callback(_, _, binary(),
                                 pid()) -> ack.

config_subscriber_callback(DeliveryTag, RoutingKey,
                           Payload, Pid) ->
    lager:debug("Pid: ~p", [Pid]),
    lager:debug("message: ~p, ~p, ~p",
                [DeliveryTag, RoutingKey, binary_to_term(Payload)]),
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
    Pid ! sub,
    spawn(fun () ->
                  handle_callback(Group, Location, Environment, Hostkey)
          end),
    ack.

handle_callback(Group, Location, Environment,
                Hostkey) ->
    dog_config_agent:set_group(Group),
    dog_config_agent:set_location(Location),
    dog_config_agent:set_environment(Environment),
    dog_config_agent:set_hostkey(Hostkey),
    write_config_file(Group, Location, Environment,
                      Hostkey),
    %Restarts ips_agent to have config change take effect, and immediate request of new iptables
    supervisor:terminate_child(dog_sup, ips_agent),
    supervisor:restart_child(dog_sup, ips_agent).

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

broker_config() ->
    BaseConfig =
    [
     {'queue.declare',
      [{queue, <<"ips">>}, {auto_delete, false}, {durable, true}]},
     {'queue.bind',
      [{queue, <<"ips">>}, {exchange, <<"ips">>}, {routing_key, <<"#">>}]},
     {'queue.declare',
      [{queue, erlang:iolist_to_binary([<<"config.">>, hostkey()]) }, {auto_delete, true}, {durable, true}]},
     {'queue.bind',
      [{queue, erlang:iolist_to_binary([<<"config.">>, hostkey()]) }, {exchange, <<"config">>},
       {routing_key, hostkey() }]},
     {'queue.declare',
      [{queue, erlang:iolist_to_binary([<<"iptables.">>, hostkey()]) }, {auto_delete, true}, {durable, true}]},
     {'queue.bind',
      [{queue, erlang:iolist_to_binary([<<"iptables.">>, hostkey()]) }, {exchange, <<"ipsets">>}, {routing_key, <<"fanout">>}]}
    ],
    %routing_key ignored bound to fanout exchange
    ExtendedConfig =
    [
     {'queue.bind',
      [{queue, erlang:iolist_to_binary([<<"iptables.">>, hostkey()]) }, {exchange, <<"iptables">>},
       {routing_key, <<"{{ environment }}.{{ location }}.{{ " "group }}.*">>}]},
     {'queue.bind',
      [{queue, erlang:iolist_to_binary([<<"iptables.">>, hostkey()]) }, {exchange, <<"iptables">>},
       {routing_key, <<"{{ environment }}.{{ location }}.*.{{ " "hostkey }}">>}]}
    ],
    {ok,
     [
      {tx, BaseConfig ++ ExtendedConfig
      }
     ]}.
