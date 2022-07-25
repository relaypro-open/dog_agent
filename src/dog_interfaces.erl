-module(dog_interfaces).

-include("dog.hrl").

-export([
        ec2_availability_zone/0,
        ec2_info/0,
        ec2_instance_id/0,
        ec2_macs/0,
        ec2_public_ipv4/0, 
        ec2_security_group_ids/0,
        ec2_owner_id/0,
        ec2_instance_tags/0,
        ec2_instance_tag/1,
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
        is_ec2_private_instance/0,
        is_softlayer_instance/0,
        publish_to_queue/1
        ]).

-spec get_provider() -> binary().
get_provider() ->
    case is_ec2_instance() of
        true -> 
            is_ec2_private_instance(),
            <<"ec2">>;
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
    IsSoftlayerInstance = case application:get_env(dog,is_softlayer_instance) of
        {ok,Boolean} ->
            Boolean;
        _ ->
            case hackney:request(Method, Url, Headers, Payload, Options) of
                {error, _Error} ->
                    logger:info("Not a softlayer instance"),
                    false;
                {ok, StatusCode, _RespHeaders, _ClientRef} ->
                    case StatusCode of
                        200 -> 
                            true;
                        _ -> 
                            false
                    end
            end
    end,
    application:set_env(dog,is_softlayer_instance,IsSoftlayerInstance), 
    IsSoftlayerInstance.

-spec is_ec2_instance() -> boolean().
is_ec2_instance() ->
    IsEc2Istance = case application:get_env(dog,is_ec2_instance) of
        {ok,Boolean} ->
            Boolean;
        _ ->
            Url = ?EC2_METADATA_BASE_URL ++ "/latest/meta-data/",
            Method = get,
            Headers = [],
            Payload = <<>>,
            Options = [{connect_timeout,1000}],
            case hackney:request(Method, Url, Headers, Payload, Options) of
                {error, _Error} ->
                    logger:info("Not an ec2 instance"),
                    false;
                {ok, StatusCode, _RespHeaders, _ClientRef} ->
                    case StatusCode of
                        200 ->
                           true;
                        _ -> 
                           false
                    end
            end
    end,
    application:set_env(dog,is_ec2_instance,IsEc2Istance), 
    IsEc2Istance.

-spec is_ec2_private_instance() -> boolean().
is_ec2_private_instance() ->
    case application:get_env(dog,is_ec2_private_instance) of
        {ok,Boolean} ->
            Boolean;
        _ ->
            case ec2_public_ipv4() of
                {error, notfound} ->
                    application:set_env(dog,is_ec2_private_instance,true),
                    true;
                {error, _} ->
                    % inconclusive, do not set env var
                    false;
                _Addresses ->
                    application:set_env(dog,is_ec2_private_instance,false),
                    false
            end
    end.

-spec is_docker_instance() -> boolean().
is_docker_instance() ->
  dog_docker:is_docker_instance().


-spec ec2_info() -> {Ec2InstanceId :: string(), Ec2AvailabilityZone :: string(), Ec2SecurityGroupIds :: string(), Ec2OwnerId :: string(), Ec2InstanceTags :: map()}.
ec2_info() ->
    case is_ec2_instance() of
        true ->
            {
             ec2_instance_id(),
             ec2_availability_zone(),
             ec2_security_group_ids(),
             ec2_owner_id(),
             ec2_instance_tags()
             };
        false ->
            {
             <<"">>,
             <<"">>,
             <<"">>,
             <<"">>,
             #{}
            }
    end.

-spec ec2_public_ipv4() -> list() | {error, atom()}.
ec2_public_ipv4() ->
    case application:get_env(dog,is_ec2_private_instance) of
        {ok, true} ->
            {error, notfound};
        _ ->
            case ec2_macs() of
                {error, Reason} ->
                    {error, Reason};
                Macs ->
                    Results = lists:flatten([ ec2_public_ipv4(Mac) || Mac <- Macs ]),
                    AnyNotFound = lists:any(fun({error, notfound}) -> true; (_) -> false end, Results),
                    AnyFailures = lists:any(fun({error, Reason}) when Reason =/= notfound -> true; (_) -> false end, Results) ,
                    Addresses = lists:filter(fun(X) when is_binary(X) -> true; (_) -> false end, Results),

                    %% If we got at least 1 real public IP, return it
                    if length(Addresses) > 0 ->
                           lists:flatten(Addresses);

                    %% Else if we got any request failures (anything not a 404), signal that the request failed
                       AnyFailures ->
                           {error, request_failed};

                    %% Else if we got no real addresses, and no failures, then we assume this instance has no public IP
                       AnyNotFound ->
                           {error, notfound};

                    %% Else.. This clause isn't logically possible, but provided for safety (TODO - dialyzer)
                       true ->
                           {error, unknown}
                    end
            end
    end.

-spec ec2_availability_zone() -> string().
ec2_availability_zone() ->
    Url = ?EC2_METADATA_BASE_URL ++ "/latest/meta-data/placement/availability-zone",
    Method = get,
    Headers = [],
    Payload = <<>>,
    Options = [{connect_timeout,1000}],
    case hackney:request(Method, Url, Headers, Payload, Options) of
        {error, _Error} ->
            logger:error("Error getting ec2_availability_zone"),
            <<"">>;
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            case StatusCode of
                200 ->
                    {ok,InstanceId} = hackney:body(ClientRef),
                    InstanceId;
                _ ->
                    logger:error("Error getting ec2_availability_zone"),
                    <<"">>
            end
    end.

-spec ec2_instance_id() -> string().
ec2_instance_id() ->
    Url = ?EC2_METADATA_BASE_URL ++ "/latest/meta-data/instance-id",
    Method = get,
    Headers = [],
    Payload = <<>>,
    Options = [{connect_timeout,1000}],
    case hackney:request(Method, Url, Headers, Payload, Options) of
        {error, _Error} ->
            logger:error("Error getting ec2_instance_id"),
            <<"">>;
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            case StatusCode of
                200 ->
                    {ok,InstanceId} = hackney:body(ClientRef),
                    InstanceId;
                _ ->
                    logger:error("Error getting ec2_instance_id"),
                    <<"">>
            end
    end.

-spec ec2_security_group_ids() -> list() | [].
ec2_security_group_ids() ->
    case ec2_macs() of
        {error, _} ->
            [];
        Macs ->
            Results = lists:map(fun(Mac) -> 
                ec2_security_group_ids(Mac)
            end, Macs),
            case lists:any(fun(Result) -> Result == {error, notfound} end, Results) of
                true ->
                    [];
                false ->
                    lists:flatten(Results)
            end
    end.

-spec ec2_security_group_ids(Mac :: string()) -> list() | [].
ec2_security_group_ids(Mac) ->
    Url = ?EC2_METADATA_BASE_URL ++ "/latest/meta-data/network/interfaces/macs/" ++ Mac ++ "/security-group-ids",
    Method = get,
    Headers = [],
    Payload = <<>>,
    Options = [{connect_timeout,1000}],
    case hackney:request(Method, Url, Headers, Payload, Options) of
        {error, _Error} ->
            logger:error("Error getting ec2_security_group_ids"),
            [];
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            case StatusCode of
                200 ->
                    {ok,Body} = hackney:body(ClientRef),
                    SecurityGroups = re:split(Body, "\n", [{return, list}]),
                    SecurityGroupsStrings = [list_to_binary(Sg) || Sg <- SecurityGroups],
                    SecurityGroupsStrings;
                _ ->
                    logger:error("Error getting ec2_security_group_ids"),
                    []
            end
    end.

-spec ec2_owner_id() -> string().
ec2_owner_id() ->
    case ec2_macs() of
        {error, _} ->
            <<"">>;
        Macs ->
            Results = lists:map(fun(Mac) -> 
                ec2_owner_id(Mac)
            end, Macs),
            case lists:any(fun(Result) -> Result == {error, notfound} end, Results) of
                true ->
                    <<"">>;
                false ->
                   hd(Results)
            end
    end.

-spec ec2_owner_id(Mac :: string()) -> string().
ec2_owner_id(Mac) ->
    Url = ?EC2_METADATA_BASE_URL ++ "/latest/meta-data/network/interfaces/macs/" ++ Mac ++ "/owner-id",
    Method = get,
    Headers = [],
    Payload = <<>>,
    Options = [{connect_timeout,1000}],
    case hackney:request(Method, Url, Headers, Payload, Options) of
        {error, _Error} ->
            logger:error("Error getting ec2_owner_id"),
            {error, notfound};
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            case StatusCode of
                200 ->
                    {ok,Body} = hackney:body(ClientRef),
                    Owners = re:split(Body, "\n", [{return, list}]),
                    OwnersStrings = [list_to_binary(Sg) || Sg <- Owners],
                    hd(OwnersStrings);
                _ ->
                    logger:error("Error getting ec2_owner_id"),
                    {error,notfound}
            end
    end.

-spec ec2_public_ipv4(Mac :: string()) -> list() | {error, atom() | integer()}.
ec2_public_ipv4(Mac) ->
    Url = ?EC2_METADATA_BASE_URL ++ "/latest/meta-data/network/interfaces/macs/" ++ Mac ++ "/public-ipv4s",
    Method = get,
    Headers = [],
    Payload = <<>>,
    Options = [{connect_timeout,1000}],
    case hackney:request(Method, Url, Headers, Payload, Options) of
        {error, Error} ->
            logger:error("Error getting ec2_public_ipv4: ~p", [Error]),
            {error, failed};
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            case StatusCode of
                200 ->
                    {ok,Body} = hackney:body(ClientRef),
                    %Ips = string:split(Body,"\n"),
                    lists:flatten(re:split(Body, "\n", [{return, binary}]));
                404 ->
                    logger:warning("Failed to get ec2_public_ipv4: ~p. This mac was not assigned a public-ipv4", [StatusCode]),
                    {error, notfound};
                _ ->
                    logger:error("Error getting ec2_public_ipv4: ~p", [StatusCode]),
                    {error, StatusCode}
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
            logger:error("Error getting ec2 macs"),
            {error, notfound};
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            case StatusCode of
                200 ->
                    {ok,Body} = hackney:body(ClientRef),
                    Macs = re:split(Body, "\n", [{return, list}]),
                    MacStrings@0 = [Mac || Mac <- Macs],
                    MacStrings@1 = [lists:flatten(re:split(Mac,"/",[{return, list}])) || Mac <- MacStrings@0],
                    MacStrings@1;
                _ ->
                    logger:error("Error getting ec2 macs"),
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
            case is_ec2_private_instance() of
                true ->
                    {ok,LocalInterfaces};
                false ->
                    case ec2_public_ipv4() of
                        {error, _} ->
                            logger:error("Using cached ec2_public_ipv4"),
                            OldPublicIpv4 = proplists:get_value(<<"ec2_public_ipv4">>,OldInterfaces,[]),
                            Both = lists:append(LocalInterfaces,[{<<"ec2_public_ipv4">>,OldPublicIpv4}]),
                            {ok, Both};
                        Ec2PublicIpv4 ->
                            Both = lists:append(LocalInterfaces,[{<<"ec2_public_ipv4">>,Ec2PublicIpv4}]),
                            logger:info("Both: ~p",[Both]),
                            {ok, Both}
                   end
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
            case Hostname of
                "localhost" ->
                    quickrand:seed(),
                    uuid:get_v4_urandom();
                _ ->
                    Hostname
            end;
        _ ->
            FullHostname
    end,
    {ok, binary:list_to_bin(Fqdn)}.

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
    logger:info("publish_to_queue: ~p",[Config]),
    UserData = #{config => Config},
    Count = 1,
    BrokerRoutingKey = <<"ips">>,
    Pid = erlang:self(),
    Message = term_to_binary([{count, Count}, {local_time, calendar:local_time()}, {pid, Pid}, {user_data, UserData}]),
	Response = turtle:publish(ips_publisher,
	?IPsExchange,
	BrokerRoutingKey,
	<<"text/json">>,
	Message,
	#{ delivery_mode => persistent }),

    logger:info("Response: ~p~n", [Response]),
    Response.

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

-spec ec2_instance_tag(Tag :: string()) -> list() | [].
ec2_instance_tag(Tag) ->
    Url = ?EC2_METADATA_BASE_URL ++ "/latest/meta-data/tags/instance/" ++ Tag,
    Method = get,
    Headers = [],
    Payload = <<>>,
    Options = [{connect_timeout,1000}],
    case hackney:request(Method, Url, Headers, Payload, Options) of
        {error, _} ->
            [];
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            case StatusCode of
                200 ->
                    {ok,Body} = hackney:body(ClientRef),
                    {Tag, Body}
                        %case lists:any(fun(Result) -> Result == {error, notfound} end, Results) of
                        %    true ->
                        %        [];
                        %    false ->
                        %        lists:flatten(Results)
                        %end
            end
    end.

-spec ec2_instance_tags() -> list() | [].
ec2_instance_tags() ->
    Url = ?EC2_METADATA_BASE_URL ++ "/latest/meta-data/tags/instance/",
    Method = get,
    Headers = [],
    Payload = <<>>,
    Options = [{connect_timeout,1000}],
    case hackney:request(Method, Url, Headers, Payload, Options) of
        {error, _Error} ->
            logger:error("Error getting ec2_instance_tags"),
            [];
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            case StatusCode of
                200 ->
                    {ok,Body} = hackney:body(ClientRef),
                    TagNames = re:split(Body, "\n", [{return, list},trim]),
                    TagNamesStrings = [list_to_binary(Tn) || Tn <- TagNames],
                    Results = lists:map(fun(Tag) -> 
                        ec2_instance_tag(Tag)
                    end, TagNamesStrings),
                    maps:from_list(Results);
                _ ->
                    logger:error("Error getting ec2_instance_tags"),
                    []
            end
    end.

