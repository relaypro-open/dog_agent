-module(dog_interfaces).

-include("dog.hrl").
-define(CONSUME_TIMEOUT, 1000).

-export([
        create_rpc_queue/1,
        ec2_macs/0,
        ec2_public_ipv4/0, 
        fqdn/0,
        get_environment_key/0, 
        get_fqdn/0,
        get_group_key/0, 
        get_host_key/0,
        get_interfaces/2,
        get_interfaces_with_ips/0,
        get_local_interfaces/0,
        get_location_key/0,
        get_provider/0,
        ip_to_queue/0,
        is_docker_instance/0,
        is_ec2_instance/0,
        is_softlayer_instance/0,
        publish_to_queue/1
        ]).

-spec get_provider() -> binary().
get_provider() ->
    case is_ec2_instance() of
        true -> <<"ec2">>;
        false ->
            case is_softlayer_instance() of
                true -> <<"softlayer">>;
                false ->
                    <<"unknown">>
            end
    end.

-spec is_softlayer_instance() -> boolean().
is_softlayer_instance() ->
    Url = ?IBM_METADATA_BASE_URL ++ "/rest/v3/SoftLayer_Resource_Metadata/getPrimaryIpAddress",
    Method = get,
    Headers = [],
    Payload = <<>>,
    Options = [{connect_timeout,1000}],
    % -> {ok, integer(), list(), client_ref()} | {ok, integer(), list()} | {error, term()}
    case hackney:request(Method, Url, Headers, Payload, Options) of
        {error, _Error} ->
            lager:error("Error checking if softlayer instance"),
            false;
        {ok, StatusCode, _RespHeaders, _ClientRef} ->
            case StatusCode of
                200 -> true;
                _ -> false
            end
    end.

-spec is_ec2_instance() -> boolean().
is_ec2_instance() ->
    Url = ?EC2_METADATA_BASE_URL ++ "/latest/meta-data/",
    Method = get,
    Headers = [],
    Payload = <<>>,
    Options = [{connect_timeout,1000}],
    %{ok, integer(), list(), client_ref()} | {ok, integer(), list()} | {error, term()}
    case hackney:request(Method, Url, Headers, Payload, Options) of
        {error, _Error} ->
            lager:error("Error checking if ec2 instance"),
            false;
        {ok, StatusCode, _RespHeaders, _ClientRef} ->
            case StatusCode of
                200 -> true;
                _ -> false
            end
    end.

-spec is_docker_instance() -> boolean().
is_docker_instance() ->
  dog_docker:is_docker_instance().

-spec ec2_public_ipv4() -> list() | {error, notfound}.
ec2_public_ipv4() ->
    case ec2_macs() of
        {error, _} ->
            {error, notfound};
        Macs ->
            Results = lists:map(fun(Mac) -> 
                ec2_public_ipv4(Mac)
            end, Macs),
            case lists:any(fun(Result) -> Result == {error, notfound} end, Results) of
                true ->
                    {error, notfound};
                false ->
                    lists:flatten(Results)
            end
    end.

-spec ec2_public_ipv4(Mac :: string()) -> list() | {error, notfound}.
ec2_public_ipv4(Mac) ->
    Url = ?EC2_METADATA_BASE_URL ++ "/latest/meta-data/network/interfaces/macs/" ++ Mac ++ "/public-ipv4s",
    Method = get,
    Headers = [],
    Payload = <<>>,
    Options = [{connect_timeout,1000}],
    %{ok, integer(), list(), client_ref()} | {ok, integer(), list()} | {error, term()}
    case hackney:request(Method, Url, Headers, Payload, Options) of
        {error, _Error} ->
            lager:error("Error getting ec2_public_ipv4"),
            {error, notfound};
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            case StatusCode of
                200 ->
                    {ok,Body} = hackney:body(ClientRef),
                    %Ips = string:split(Body,"\n"),
                    Ips = re:split(Body, "\n", [{return, list}]),
                    IPStrings = [list_to_binary(Ip) || Ip <- Ips],
                    IPStrings;
                _ ->
                    lager:error("Error getting ec2_public_ipv4"),
                    {error,notfound}
            end
    end.

-spec ec2_macs() -> list() | {error, atom()}.
ec2_macs() ->
    Url = ?EC2_METADATA_BASE_URL ++ "/latest/meta-data/network/interfaces/macs/",
    Method = get,
    Headers = [],
    Payload = <<>>,
    Options = [{connect_timeout,1000}],
    case hackney:request(Method, Url, Headers, Payload, Options) of
        {error, _Error} ->
            lager:error("Error getting ec2 macs"),
            {error, notfound};
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            case StatusCode of
                200 ->
                    {ok,Body} = hackney:body(ClientRef),
                    %Macs = string:split(Body,"\n"),
                    Macs = re:split(Body, "\n", [{return, list}]),
                    %MacStrings@0 = [binary_to_list(Mac) || Mac <- Macs],
                    MacStrings@0 = [Mac || Mac <- Macs],
                    %MacStrings@1 = [lists:flatten(string:split(Mac,"/")) || Mac <- MacStrings@0],
                    MacStrings@1 = [lists:flatten(re:split(Mac,"/",[{return, list}])) || Mac <- MacStrings@0],
                    MacStrings@1;
                _ ->
                    lager:error("Error getting ec2 macs"),
                    {error, notfound}
            end
    end.

-spec get_interfaces_with_ips() -> {'ok',[{_,_}]}.
get_interfaces_with_ips() ->
    {ok, Interfaces } = inet:getifaddrs(),
    IP_Interfaces = [ {Name,Flags} || {Name,Flags} <- Interfaces, lists:any(fun(Flag) -> Flag == addr end, proplists:get_keys(Flags)) ],
    {ok, IP_Interfaces}.

-spec get_interfaces(Provider :: binary(), OldInterfaces :: any()) -> {ok, iolist()}.
get_interfaces(Provider, OldInterfaces) ->
    case Provider of
        <<"ec2">> ->
            {ok, LocalInterfaces} = get_local_interfaces(),
            case ec2_public_ipv4() of
                {error, notfound} ->
                    lager:error("Using cached ec2_public_ipv4"),
                    OldPublicIpv4 = proplists:get_value(<<"ec2_public_ipv4">>,OldInterfaces,[]),
                    Both = lists:append(LocalInterfaces,[{<<"ec2_public_ipv4">>,OldPublicIpv4}]),
                    {ok, Both};
                Ec2PublicIpv4 ->
                    Both = lists:append(LocalInterfaces,[{<<"ec2_public_ipv4">>,Ec2PublicIpv4}]),
                    lager:info("Both: ~p",[Both]),
                    {ok, Both}
           end;
        _ ->
            get_local_interfaces()
    end.

-spec get_local_interfaces() -> {ok, list()}.
get_local_interfaces() ->
    {ok, IP_Interfaces} = get_interfaces_with_ips(),
    Interfaces = lists:map(fun({X,Y}) -> {list_to_binary(X),[ A || A <- Y, proplists:get_keys([A]) == [addr]]} end, IP_Interfaces),
    Interfaces2 = [{X, [  list_to_binary(inet_parse:ntoa(A)) || A <- proplists:get_all_values(addr,Y)]} || {X,Y} <- Interfaces],
    {ok, Interfaces2}.

-spec get_fqdn() -> {ok, binary()}.
get_fqdn() ->
    {ok, Hostname} = inet:gethostname(),
    {ok,{hostent,FullHostname,_,inet,_,_}} = inet:gethostbyname(Hostname),
    %If unable to get a unique hostname, use a random uuid
    Fqdn = case string:left(FullHostname,9) of
        "localhost" ->
            quickrand:seed(),
            uuid:get_v4_urandom();
        _ ->
            list_to_binary(FullHostname)
    end,
    {ok, Fqdn}.

-spec fqdn() -> binary().
fqdn() ->
    {ok, Fqdn} = get_fqdn(),
    Fqdn.

-spec get_group_key() -> {'ok',binary()}.
get_group_key() ->
    {ok, Group} = application:get_env(dog, group),
    {ok, list_to_binary(Group)}.

-spec get_environment_key() -> {'ok',binary()}.
get_environment_key() ->
    {ok, Environment} = application:get_env(dog, environment),
    {ok, list_to_binary(Environment)}.

-spec get_location_key() -> {'ok',binary()}.
get_location_key() ->
    {ok, Location} = application:get_env(dog, location),
    {ok, list_to_binary(Location)}.

-spec get_host_key() -> {'ok',binary()}.
get_host_key() ->
    {ok, HostKey} = application:get_env(dog, hostkey),
    {ok, list_to_binary(HostKey)}.

-spec publish_to_queue(Config :: map() ) -> any().
publish_to_queue(Config) ->
	rpc_request(Config).
%publish_to_queue(Config) ->
%    lager:info("publish_to_queue: ~p",[Config]),
%    UserData = #{config => Config},
%    Count = 1,
%    BrokerRoutingKey = <<"ips">>,
%    Pid = erlang:self(),
%    Message = term_to_binary([{count, Count}, {local_time, calendar:local_time()}, {pid, Pid}, {user_data, UserData}]),
%    Response = thumper:publish(Message, ?IPsExchange, BrokerRoutingKey),
%    lager:info("Response: ~p~n", [Response]),
%    Response.


-spec rpc_request(Config :: map() ) -> any().
rpc_request(Config) ->
    lager:info("publish_to_queue: ~p",[Config]),
    UserData = #{config => Config},
    Count = 1,
    BrokerRoutingKey = <<"ips">>,
    Pid = erlang:self(),
    CorrelationId = base64:encode(erlang:integer_to_binary(erlang:unique_integer())),
    Hostkey = dog_config:hostkey(),
    ReplyQueue = "reply." ++ binary_to_list(Hostkey),
    Message = term_to_binary([{count, Count}, {local_time, calendar:local_time()}, {pid, Pid}, {user_data, UserData},{reply_to, ReplyQueue},{correlation_id, CorrelationId}]),
    Response = thumper:publish(Message, ?IPsExchange, BrokerRoutingKey),
    lager:info("Response: ~p~n", [Response]),
    consume_response(default, list_to_binary(ReplyQueue), CorrelationId),
    Response.

-spec create_rpc_queue(QueueDefinition ::
                map()) -> atom().

create_rpc_queue(#{broker := Broker, name := Name,
            queue := QueueName}) ->
    Op = {'queue.declare',
      [{queue, list_to_binary(QueueName)},
       {auto_delete, true}, {durable, true}]},
    lager:debug("Name: ~p", [Name]),
    case dog_thumper_sup:amqp_op(Broker, Name, [Op]) of
      ok -> ok;
      Error -> lager:error("Error: ~p", [Error]), Error
    end.

consume_response(Broker, Queue, CorrelationId) ->
    Ref = make_ref(),
    Me = self(),
    Callback = fun(DT, RK, Payload) ->
                       Me ! {Ref, {DT, RK, Payload}},
                       ack
               end,
    {ok, ConsumerPid} = thumper_consumer:start_link(undefined, Broker, Queue, Callback),
    lager:debug("created consumer ~p", [ConsumerPid]),
    fun Rcv(true) ->
            unlink(ConsumerPid),
            thumper_consumer:stop(ConsumerPid),
			ack;
        Rcv(false) ->
            receive
                {Ref, {_DT, _RK, Payload}} ->
					lager:debug("Payload: ~p~n",[Payload]),
					Proplist = binary_to_term(Payload),
					lager:debug("Proplist: ~p", [Proplist]),
					ReceivedCorrelationId = proplists:get_value(correlation_id, Proplist),
					case ReceivedCorrelationId == CorrelationId of
						true ->
							Rcv(true);
						false ->
							Rcv(false)
					end
            after ?CONSUME_TIMEOUT ->
                      {error, timeout}
            end
	end.

%-spec subscribe_to_rpc_response(QueueDefinition ::
%                    map()) -> atom().
%
%subscribe_to_rpc_response(#{broker := Broker,
%                name := Name, queue := QueueName},CorrelationId) ->
%    Pid = erlang:self(),
%    Callback = fun (A, B, C) ->
%               rpc_response_callback(A, B, C, Pid, CorrelationId)
%           end,
%    case dog_thumper_sup:ensure_consumer(up, Name, Broker,
%                     list_to_binary(QueueName), Callback)
%    of
%      {ok, _ChildPid} -> ok;
%      {error, {already_up, _ChildPid}} -> ok
%    end.

%-spec rpc_response_callback(_, _, binary(),
%                   pid()) -> ack.
%
%rpc_response_callback(DeliveryTag, RoutingKey,
%                 Payload, Pid, CorrelationId) ->
%    lager:debug("message: ~p, ~p, ~p",
%        [DeliveryTag, RoutingKey, binary_to_term(Payload)]),
%    lager:debug("Payload: ~p", [Payload]),
%    Proplist = binary_to_term(Payload),
%    lager:debug("Proplist: ~p", [Proplist]),
%    ReceivedCorrelationId = proplists:get_value(correlation_id, Proplist),
%    case ReceivedCorrelationId == CorrelationId of
%        true ->
%            Pid ! sub,
%            ack;
%        false ->
%            pass
%    end.

-spec ip_to_queue() -> any().
ip_to_queue() ->
    Group = "test",
    Hostname = "test-tt-aws01.test.test",
    Environment = "qa",
    Location = "us-east-1",
    HostKey = "test-tt-aws01",
    Interfaces = [{"lo",[{addr,{127,0,0,1}}]}, {"eth0",[{addr,{10,1,1,51}}]}],
    UpdateType = update,
    Config = #{
        <<"name">> => Hostname,
        <<"interfaces">> => Interfaces,
        <<"group">> => Group,
        <<"location">> => Location,
        <<"environment">> => Environment,
        <<"hostkey">> => HostKey,
        <<"updatetype">> => UpdateType
    },
    publish_to_queue(Config).
