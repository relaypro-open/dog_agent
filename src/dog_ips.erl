-module(dog_ips).

-export([do_watch_interfaces/1,
         do_watch_iptables/1, do_keepalive/1,
         do_get_host_routing_key/1,
         do_get_group_routing_key/1]).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec
     do_get_group_routing_key(dog_state:dog_state()) -> {ok,
                             binary()}.

do_get_group_routing_key(State) ->
    lager:debug("State: ~p", [State]),
    RoutingKey = dog_state:to_group_routing_key(State),
    {ok, RoutingKey}.

-spec
     do_get_host_routing_key(dog_state:dog_state()) -> {ok,
                            binary()}.

do_get_host_routing_key(State) ->
    lager:debug("State: ~p", [State]),
    RoutingKey = dog_state:to_host_routing_key(State),
    {ok, RoutingKey}.

%% @doc watches queue for iptables updates.
%-spec do_watch_iptables(State :: dog_state()) -> {ok, State :: dog_state()}.
-spec do_watch_iptables(State ::
                dog_state:dog_state()) -> no_return().

do_watch_iptables(State) ->
    lager:debug("do_watch_iptables"),
    {ok, GroupRoutingKey} = do_get_group_routing_key(State),
    lager:debug("GroupRoutingKey: ~p", [GroupRoutingKey]),
    ok =
    dog_iptables:ensure_iptables_consumer(GroupRoutingKey),
    {ok, HostRoutingKey} = do_get_host_routing_key(State),
    lager:debug("HostRoutingKey: ~p", [HostRoutingKey]),
    ok =
    dog_iptables:ensure_iptables_consumer(HostRoutingKey),
    {ok, State}.


%% @doc watches for IP updates, publishes to queue.
-spec do_watch_interfaces(State ::
                  dog_state:dog_state()) -> {ok,
                             State ::
                                 dog_state:dog_state()}.

do_watch_interfaces(StateOld) ->
    %lager:info("State0: ~p", [State0]),
    Provider = dog_state:get_provider(StateOld),
    {Ec2InstanceId, Ec2AvailabilityZone, Ec2SecurityGroupIds, Ec2OwnerId} = dog_interfaces:ec2_info(),
    HostnameOld = dog_state:get_hostname(StateOld),
    InterfacesOld = dog_state:get_interfaces(StateOld),
    {ok, InterfacesNew} =
    dog_interfaces:get_interfaces(Provider, InterfacesOld),
    {ok, Hostname} = dog_interfaces:get_fqdn(),
    Group = dog_state:get_group(StateOld),
    Location = dog_state:get_location(StateOld),
    Environment = dog_state:get_environment(StateOld),
    HostKey = dog_state:get_hostkey(StateOld),
    Hash4Ipsets =
    dog_iptables:create_hash(dog_iptables:read_current_ipv4_ipsets()),
    Hash6Ipsets =
    dog_iptables:create_hash(dog_iptables:read_current_ipv6_ipsets()),
    Hash4Iptables =
    dog_iptables:create_hash(dog_iptables:read_current_ipv4_iptables()),
    Hash6Iptables =
    dog_iptables:create_hash(dog_iptables:read_current_ipv6_iptables()),
    IpsetHash = dog_ipset:read_hash(),
    {ok, Version} = dog_app:get_version(),
    UpdateType = update,
    StateNew = dog_state:from_map(#{<<"name">> => Hostname,
                    <<"interfaces">> => InterfacesNew,
                    <<"group">> => Group,
                    <<"location">> => Location,
                    <<"environment">> => Environment,
                    <<"hostkey">> => HostKey,
                    <<"version">> => Version,
                    <<"hash4_ipsets">> => Hash4Ipsets,
                    <<"hash6_ipsets">> => Hash6Ipsets,
                    <<"hash4_iptables">> => Hash4Iptables,
                    <<"hash6_iptables">> => Hash6Iptables,
                    <<"provider">> => Provider,
                    <<"updatetype">> => UpdateType,
                    <<"ipset_hash">> => IpsetHash,
                    <<"ec2_instance_id">> => Ec2InstanceId,
                    <<"ec2_availability_zone">> => Ec2AvailabilityZone,
                    <<"ec2_security_group_ids">> => Ec2SecurityGroupIds,
                    <<"ec2_owner_id">> => Ec2OwnerId
                                   }),
    case InterfacesOld == InterfacesNew of
      false ->
      lager:debug("HostnameOld, InterfacesOld: ~p, ~p",
              [HostnameOld, InterfacesOld]),
      lager:debug("Hostname, Interfaces: ~p, ~p",
              [Hostname, InterfacesNew]),
      lager:debug("StateNew: ~p", [StateNew]),
      StateMap = dog_state:to_map(StateNew),
      dog_interfaces:publish_to_queue(StateMap);
      true -> ok
    end,
    {ok, StateNew}.

-spec do_keepalive(State ::
               dog_state:dog_state()) -> {ok,
                          State ::
                              dog_state:dog_state()}.

do_keepalive(State) ->
    lager:debug("do_keepalive"),
    UpdateType = keepalive,
    StateNew = dog_state:set_updatetype(State, UpdateType),
    lager:debug("StateNew: ~p", [StateNew]),
    StateMap = dog_state:to_map(StateNew),
    dog_interfaces:publish_to_queue(StateMap),
    {ok, StateNew}.
