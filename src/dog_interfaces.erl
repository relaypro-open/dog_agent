-module(dog_interfaces).

-include("dog.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-export([
         ec2_availability_zone/0,
         ec2_info/0,
         ec2_instance_id/0,
         ec2_instance_tag/1,
         ec2_instance_tags/0,
         ec2_macs/0,
         ec2_owner_id/0,
         ec2_public_ipv4/0,
         ec2_region/0,
         ec2_security_group_ids/0,
         ec2_subnet_id/0,
         ec2_vpc_id/0,
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
         maybe_imdsv2_session_token/1,
         os_info/0,
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
                                ?LOG_INFO("Not a softlayer instance"),
                                false;
                              {ok, StatusCode, _RespHeaders, ClientRef} ->
                                hackney:close(ClientRef),
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
                     Headers = [{<<"Content-Type">>, <<"text/plain">>}],
                     Payload = <<>>,
                     Options = [{connect_timeout,1000}],
                     case hackney:request(Method, Url, Headers, Payload, Options) of
                       {error, _Error} ->
                         ?LOG_INFO("Not an ec2 instance"),
                         false;
                       {ok, StatusCode, _RespHeaders, ClientRef} ->
                         hackney:close(ClientRef),
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


-spec ec2_info() -> {Ec2Region :: string(), Ec2InstanceId :: string(), Ec2AvailabilityZone :: string(), Ec2SecurityGroupIds :: string(), Ec2OwnerId :: string(), Ec2InstanceTags :: map(),
                     Ec2VpcId :: string(), Ec2SubnetId :: string() }.
ec2_info() ->
  case is_ec2_instance() of
    true ->
      {
       ec2_region(),
       ec2_instance_id(),
       ec2_availability_zone(),
       ec2_security_group_ids(),
       ec2_owner_id(),
       ec2_instance_tags(),
       ec2_vpc_id(),
       ec2_subnet_id()
      };
    false ->
      {
       <<"">>,
       <<"">>,
       <<"">>,
       <<"">>,
       <<"">>,
       #{},
       <<"">>,
       <<"">>
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

-spec ec2_region() -> string().
ec2_region() ->
  case ec2_availability_zone() of
    <<"">> ->
      <<"">>;
    AZ ->
      string:slice(AZ,0,string:length(AZ) - 1)
  end.

-spec get_availability_zone() -> {ok, binary()} | {error, term()}.
get_availability_zone() ->
  case application:get_env(erlcloud, availability_zone) of
    {ok, AZ} = OkResult when is_binary(AZ) ->
      OkResult;
    _ ->
      cache_instance_metadata_availability_zone()
  end.

-spec cache_instance_metadata_availability_zone() -> {ok, binary()} | {error, term()}.
cache_instance_metadata_availability_zone() ->
  % it fine to use default here - no IAM is used, only for http client
  % one cannot use auto_config()/default_cfg() as it creates an infinite recursion.
  case erlcloud_ec2_meta:get_instance_metadata("placement/availability-zone", #aws_config{}) of
    {ok, AZ} = OkResult ->
      application:set_env(erlcloud, availability_zone, AZ),
      OkResult;
    {error, _} = Error ->
      Error
  end.

-spec ec2_availability_zone() -> string().
ec2_availability_zone() ->
  {ok, Az} = get_availability_zone(),
  Az.

-spec ec2_instance_id() -> {ok, binary()} | {error, term()}.
ec2_instance_id() ->
  {ok, Id} = get_ec2_instance_id(),
  Id.

-spec get_ec2_instance_id() -> {ok, binary()} | {error, term()}.
get_ec2_instance_id() ->
  case application:get_env(erlcloud, instance_id) of
    {ok, Id} = OkResult when is_binary(Id) ->
      OkResult;
    _ ->
      cache_ec2_instance_id()
  end.

-spec cache_ec2_instance_id() -> string().
cache_ec2_instance_id() ->
  Config = #aws_config{},
  IMDSv2Token = maybe_imdsv2_session_token(Config),
  case erlcloud_ec2_meta:get_instance_metadata("instance-id", Config, IMDSv2Token) of
    {ok, Id} = OkResult ->
      application:set_env(erlcloud, instance_id, Id),
      OkResult;
    {error, _} = Error ->
      Error
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
  {ok, Ids} = get_ec2_security_group_ids(Mac),
  Ids.

-spec get_ec2_security_group_ids(Mac :: iolist()) -> string().
get_ec2_security_group_ids(Mac) ->
  Config = #aws_config{},
  IMDSv2Token = maybe_imdsv2_session_token(Config),
  case erlcloud_ec2_meta:get_instance_metadata("network/interfaces/macs/" ++ Mac ++ "/security-group-ids", Config, IMDSv2Token) of
    {ok, _Ids} = OkResult ->
      OkResult;
    {error, _} = Error ->
      Error
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
  {ok, Id} = get_ec2_owner_id(Mac),
  Id.

-spec get_ec2_owner_id(Mac :: iolist()) -> {ok, binary()} | {error, term()}.
get_ec2_owner_id(Mac) ->
  case application:get_env(erlcloud, ec2_owner_id) of
    {ok, Ids} = OkResult when is_binary(Ids) ->
      OkResult;
    _ ->
      cache_ec2_owner_id(Mac)
  end.

-spec cache_ec2_owner_id(Mac :: iolist()) -> string().
cache_ec2_owner_id(Mac) ->
  Config = #aws_config{},
  IMDSv2Token = maybe_imdsv2_session_token(Config),
  case erlcloud_ec2_meta:get_instance_metadata("network/interfaces/macs/" ++ Mac ++ "/owner-id", Config, IMDSv2Token) of
    {ok, Id} = OkResult ->
      application:set_env(erlcloud, ec2_owner_id, Id),
      OkResult;
    {error, _} = Error ->
      Error
  end.

-spec ec2_subnet_id() -> string().
ec2_subnet_id() ->
  case ec2_macs() of
    {error, _} ->
      <<"">>;
    Macs ->
      Results = lists:map(fun(Mac) ->
                              ec2_subnet_id(Mac)
                          end, Macs),
      case lists:any(fun(Result) -> Result == {error, notfound} end, Results) of
        true ->
          <<"">>;
        false ->
          hd(Results)
      end
  end.

-spec ec2_subnet_id(Mac :: string()) -> string().
ec2_subnet_id(Mac) ->
  {ok, Id} = get_ec2_subnet_id(Mac),
  Id.

-spec get_ec2_subnet_id(Mac :: iolist()) -> {ok, binary()} | {error, term()}.
get_ec2_subnet_id(Mac) ->
  case application:get_env(erlcloud, ec2_subnet_id) of
    {ok, Ids} = OkResult when is_binary(Ids) ->
      OkResult;
    _ ->
      cache_ec2_subnet_id(Mac)
  end.

-spec cache_ec2_subnet_id(Mac :: iolist()) -> string().
cache_ec2_subnet_id(Mac) ->
  Config = #aws_config{},
  IMDSv2Token = maybe_imdsv2_session_token(Config),
  case erlcloud_ec2_meta:get_instance_metadata("network/interfaces/macs/" ++ Mac ++ "/subnet-id", Config, IMDSv2Token) of
    {ok, Id} = OkResult ->
      application:set_env(erlcloud, ec2_subnet_id, Id),
      OkResult;
    {error, _} = Error ->
      Error
  end.

-spec ec2_vpc_id() -> string().
ec2_vpc_id() ->
  case ec2_macs() of
    {error, _} ->
      <<"">>;
    Macs ->
      Results = lists:map(fun(Mac) ->
                              ec2_vpc_id(Mac)
                          end, Macs),
      case lists:any(fun(Result) -> Result == {error, notfound} end, Results) of
        true ->
          <<"">>;
        false ->
          hd(Results)
      end
  end.

-spec ec2_vpc_id(Mac :: string()) -> string().
ec2_vpc_id(Mac) ->
  {ok, Id} = get_ec2_vpc_id(Mac),
  Id.

-spec get_ec2_vpc_id(Mac :: iolist()) -> {ok, binary()} | {error, term()}.
get_ec2_vpc_id(Mac) ->
  case application:get_env(erlcloud, ec2_vpc_id) of
    {ok, Ids} = OkResult when is_binary(Ids) ->
      OkResult;
    _ ->
      cache_ec2_vpc_id(Mac)
  end.

-spec cache_ec2_vpc_id(Mac :: iolist()) -> string().
cache_ec2_vpc_id(Mac) ->
  Config = #aws_config{},
  IMDSv2Token = maybe_imdsv2_session_token(Config),
  case erlcloud_ec2_meta:get_instance_metadata("network/interfaces/macs/" ++ Mac ++ "/vpc-id", Config, IMDSv2Token) of
    {ok, Id} = OkResult ->
      application:set_env(erlcloud, ec2_vpc_id, Id),
      OkResult;
    {error, _} = Error ->
      Error
  end.

-spec ec2_public_ipv4(Mac :: string()) -> string().
ec2_public_ipv4(Mac) ->
  {ok, Id} = get_ec2_public_ipv4(Mac),
  Id.

-spec get_ec2_public_ipv4(Mac :: iolist()) -> string().
get_ec2_public_ipv4(Mac) ->
  Config = #aws_config{},
  IMDSv2Token = maybe_imdsv2_session_token(Config),
  case erlcloud_ec2_meta:get_instance_metadata("network/interfaces/macs/" ++ Mac ++ "/public-ipv4s", Config, IMDSv2Token) of
    {ok, _Id} = OkResult ->
      OkResult;
    {error, _} = Error ->
      Error
  end.

-spec ec2_macs() -> string().
ec2_macs() ->
  {ok, Macs} = get_ec2_macs(),
  MacStrings@0 = re:split(Macs, "\n", [{return, list}]),
  MacStrings@1 = [Mac || Mac <- MacStrings@0],
  MacStrings@2 = [lists:flatten(re:split(Mac,"/",[{return, list}])) || Mac <- MacStrings@1],
  MacStrings@2.

-spec get_ec2_macs() -> string().
get_ec2_macs() ->
  Config = #aws_config{},
  IMDSv2Token = maybe_imdsv2_session_token(Config),
  case erlcloud_ec2_meta:get_instance_metadata("network/interfaces/macs/", Config, IMDSv2Token) of
    {ok, _Id} = OkResult ->
      OkResult;
    {error, _} = Error ->
      Error
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
              ?LOG_ERROR("Using cached ec2_public_ipv4"),
              OldPublicIpv4 = proplists:get_value(<<"ec2_public_ipv4">>,OldInterfaces,[]),
              Both = lists:append(LocalInterfaces,[{<<"ec2_public_ipv4">>,OldPublicIpv4}]),
              {ok, Both};
            Ec2PublicIpv4 ->
              Both = lists:append(LocalInterfaces,[{<<"ec2_public_ipv4">>,Ec2PublicIpv4}]),
              ?LOG_INFO("Both: ~p",[Both]),
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
  ?LOG_INFO("publish_to_queue: ~p",[Config]),
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

-spec ec2_instance_tags() -> {ok, binary()} | {error, term()}.
ec2_instance_tags() ->
  {ok, Tags} = get_ec2_instance_tags(),
  TagNames = re:split(Tags, "\n", [{return, list},trim]),
  TagNamesStrings = [list_to_binary(Tn) || Tn <- TagNames],
  Results = lists:map(fun(Tag) ->
                          ec2_instance_tag(Tag)
                      end, TagNamesStrings),
  Results.
%maps:from_list(Results).

-spec get_ec2_instance_tags() -> string().
get_ec2_instance_tags() ->
  Config = #aws_config{},
  IMDSv2Token = maybe_imdsv2_session_token(Config),
  case erlcloud_ec2_meta:get_instance_metadata("tags/instance", Config, IMDSv2Token) of
    {ok, _Id} = OkResult ->
      OkResult;
    {error, _} = Error ->
      Error
  end.

-spec ec2_instance_tag(Tag :: string()) -> string().
ec2_instance_tag(Tag) ->
  {ok, Value} = get_ec2_instance_tag(Tag),
  {Tag, Value}.

-spec get_ec2_instance_tag(Tag :: iolist()) -> string().
get_ec2_instance_tag(Tag) ->
  Config = #aws_config{},
  IMDSv2Token = maybe_imdsv2_session_token(Config),
  case erlcloud_ec2_meta:get_instance_metadata("tags/instance/" ++ Tag, Config, IMDSv2Token) of
    {ok, _Id} = OkResult ->
      OkResult;
    {error, _} = Error ->
      Error
  end.

exec(Command) ->
  Result = exec:run(Command, [sync, stdout, stderr]),
  case Result of
    {ok,[{stdout,StdOut}]} ->
      string:trim(StdOut);
    _ ->
      []
  end.

-spec os_info() -> tuple().
os_info() ->
  OS_Distribution = exec("lsb_release -s -i"),
  OS_Version = exec("lsb_release -s -r"),
  {OS_Distribution,OS_Version}.

-spec maybe_imdsv2_session_token(aws_config()) -> binary() | undefined.
maybe_imdsv2_session_token(Config) ->
  case erlcloud_ec2_meta:generate_session_token(60, Config) of
    {ok, Token} -> Token;
    _Error -> undefined
  end.
