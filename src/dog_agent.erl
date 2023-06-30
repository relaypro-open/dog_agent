-module(dog_agent).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-include("dog.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
     keepalive/0, 
     start_link/0,  
     watch_docker/0,
     watch_interfaces/0,
     watch_iptables/0,
     create_ipsets/1, read_hash/0
  ]).

-export([
         get_group_routing_key/0,
         get_host_routing_key/0,
         get_environment/0,
         get_group/0,
         get_hostkey/0,
         get_hostname/0,
         get_interfaces/0,
         get_location/0,
         get_state/0,
         group_routing_key/0,
         host_routing_key/0, 
         set_environment/1,
         set_group/1,
         set_hostkey/1,
         set_hostname/1,
         set_interfaces/1,
         set_location/1,
         set_state/1,
         watch_config/0
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([code_change/3, handle_call/3, handle_cast/2,
     handle_info/2, init/1, terminate/2]).

%% ------------------------------------------------------------------
%% test Function Exports
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, Pid :: pid()} | ignore |
              {error, {already_started, Pid :: pid()} | term()}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],
              []).

-spec watch_iptables() -> ok.

watch_iptables() ->
    gen_server:call(?MODULE, watch_iptables).

-spec watch_interfaces() -> ok.

watch_interfaces() ->
    gen_server:call(?MODULE, watch_interfaces).

-spec keepalive() -> ok.

keepalive() -> gen_server:call(?MODULE, keepalive).

-spec watch_docker() -> ok.

watch_docker() ->
    gen_server:call(?MODULE, watch_docker).

-spec read_hash() -> Hash :: list().

read_hash() ->
  try 
    gen_server:call(?MODULE, read_hash, 20000)
  catch 
    Class:Reason:Stacktrace -> 
      ?LOG_ERROR(
              "~nStacktrace:~s",
              [Stacktrace]),
      {Class, Reason} 
  end.

-spec create_ipsets(Ipsets :: iolist()) -> ok.

create_ipsets(Ipsets) ->
  try 
    gen_server:call(?MODULE, {create_ipsets,Ipsets}, 20000)
  catch 
    Class:Reason:Stacktrace -> 
      ?LOG_ERROR(
              "~nStacktrace:~s",
              [Stacktrace]),
      {Class, Reason} 
  end.

-spec set_state(State :: dog_state:dog_state()) -> {ok,
                            State ::
                            dog_state:dog_state()}.
set_state(State) ->
    gen_server:call(?MODULE, {set_state, State}).

-spec get_group() -> {ok, Hostkey :: binary()}.
get_group() ->
    gen_server:call(?MODULE, {get_group}).

-spec set_group(Group :: binary()) -> {ok,
                       State :: dog_state:dog_state()}.
set_group(Group) ->
    gen_server:call(?MODULE, {set_group, Group}).

-spec get_hostname() -> {ok, Hostkey :: binary()}.
get_hostname() ->
    gen_server:call(?MODULE, {get_hostname}).

-spec set_hostname(Hostname :: binary()) -> {ok,
                         State :: dog_state:dog_state()}.
set_hostname(Hostname) ->
    gen_server:call(?MODULE, {set_hostname, Hostname}).

-spec get_interfaces() -> {ok, Hostkey :: binary()}.
get_interfaces() ->
    gen_server:call(?MODULE, {get_interfaces}).

-spec set_interfaces(Interfaces :: binary()) -> {ok,
                         State ::
                             dog_state:dog_state()}.
set_interfaces(Interfaces) ->
    gen_server:call(?MODULE, {set_interfaces, Interfaces}).

-spec get_location() -> {ok, Hostkey :: binary()}.
get_location() ->
    gen_server:call(?MODULE, {get_location}).

-spec set_location(Location :: binary()) -> {ok,
                         State :: dog_state:dog_state()}.
set_location(Location) ->
    gen_server:call(?MODULE, {set_location, Location}).

-spec get_environment() -> {ok, Hostkey :: binary()}.
get_environment() ->
    gen_server:call(?MODULE, {get_environment}).

-spec set_environment(Environment :: binary()) -> {ok,
                           State ::
                               dog_state:dog_state()}.
set_environment(Environment) ->
    gen_server:call(?MODULE,
            {set_environment, Environment}).

-spec get_hostkey() -> {ok, Hostkey :: binary()}.
get_hostkey() ->
    gen_server:call(?MODULE, {get_hostkey}).

-spec set_hostkey(Hostkey :: binary()) -> {ok,
                       State :: dog_state:dog_state()}.
set_hostkey(Hostkey) ->
    gen_server:call(?MODULE, {set_hostkey, Hostkey}).

-spec get_state() -> {ok,
              State :: dog_state:dog_state()}.
get_state() -> gen_server:call(?MODULE, get_state).

-spec get_host_routing_key() -> {ok,
                 State :: dog_state:dog_state()}.
get_host_routing_key() ->
    gen_server:call(?MODULE, host_routing_key).

-spec host_routing_key() -> string().
host_routing_key() ->
    RoutingKey = get_host_routing_key(),
    binary_to_list(RoutingKey).

-spec get_group_routing_key() -> {ok,
                  State :: dog_state:dog_state()}.
get_group_routing_key() ->
    gen_server:call(?MODULE, group_routing_key).

-spec group_routing_key() -> string().
group_routing_key() ->
    RoutingKey = get_group_routing_key(),
    binary_to_list(RoutingKey).

-spec watch_config() -> ok.

watch_config() ->
    gen_server:call(?MODULE, watch_config).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State} |
%%          {ok, State, Timeout} |
%%          ignore |
%%          {stop, Reason}
%%----------------------------------------------------------------------

-spec init(term()) -> no_return().

init(_Args) ->
    WaitSeconds = 15,
    WaitMilliSeconds = WaitSeconds * 1000,
    ?LOG_INFO("Waiting %s seconds for rabbitmq initialization",[WaitSeconds]),
    timer:sleep(WaitMilliSeconds),
    WatchInterfacesPollMilliseconds = application:get_env(dog, watch_interfaces_poll_seconds, 5) * 1000,
    _IpsTimer = erlang:send_after(WatchInterfacesPollMilliseconds, self(),
                  watch_interfaces),
    KeepalivePollMilliseconds = application:get_env(dog, keepalive_initial_delay_seconds, 60) * 1000,
    _KeepaliveTimer = erlang:send_after(KeepalivePollMilliseconds, self(),
                    keepalive),
    WatchDockerPollMilliseconds = application:get_env(dog, watch_docker_poll_seconds, 15) * 1000,
    _DockerTimer = erlang:send_after(WatchDockerPollMilliseconds, self(),
                  watch_docker),
    State = init_state(),
    ?LOG_DEBUG("State: ~p", [State]),
    StateMap = dog_state:to_map(State),
    ?LOG_DEBUG("StateMap: ~p~n", [StateMap]),
    ?LOG_DEBUG("force update"),
    dog_interfaces:publish_to_queue(StateMap), %force update
    {ok, State}.


init_state() ->
    ok = dog_config:do_init_config(),
    Provider = dog_interfaces:get_provider(),
    {ok, Interfaces} =
    dog_interfaces:get_interfaces(Provider, []),
    {Ec2Region,Ec2InstanceId, Ec2AvailabilityZone, Ec2SecurityGroupIds, Ec2OwnerId, Ec2InstanceTags, Ec2VpcId, Ec2SubnetId} = dog_interfaces:ec2_info(),
    {OS_Distribution,OS_Version} = dog_interfaces:os_info(),
    {ok, Hostname} = dog_interfaces:get_fqdn(),
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
    UpdateType = force,
    {Group, Location, Environment, Hostkey} = case dog_config:read_config_file() of
        {ok, ConfigMap} ->
            ?LOG_DEBUG("ConfigMap: ~p", [ConfigMap]),
            {
              maps:get(<<"group">>, ConfigMap),
              maps:get(<<"location">>, ConfigMap),
              maps:get(<<"environment">>, ConfigMap),
              maps:get(<<"hostkey">>, ConfigMap)
            };
        file_read_error ->
            {
            <<"">>,
            <<"*">>,
            <<"*">>,
            <<"">>
            }
    end,
    Hostkey1 = case Hostkey of
      <<>> ->
        throw("hostkey_not_set");
      _ ->
        Hostkey
    end,
    ?LOG_DEBUG("Hostkey: ~p",[Hostkey1]),
    DockerContainerIds = dog_docker:get_container_ids(),
    State = dog_state:dog_state(Group, Hostname,
                Location, Environment,
                Hostkey1, Interfaces, Version,
                Hash4Ipsets, Hash6Ipsets,
                Hash4Iptables, Hash6Iptables,
                Provider, UpdateType,
                IpsetHash,
		Ec2Region,
		Ec2InstanceId,
	        Ec2AvailabilityZone,
	        Ec2SecurityGroupIds,
	        Ec2OwnerId,Ec2InstanceTags,
		OS_Distribution,OS_Version,
		Ec2VpcId, Ec2SubnetId, DockerContainerIds),
    State.

%
%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, Reply, State} | (terminate/2 is called)
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()},
          State :: dog_state:dog_state()) -> {reply, ok, any()}.

handle_call({create_ipsets,Ipsets}, _From, State) ->
    dog_ipset:create_ipsets(Ipsets),
    {reply, ok, State};
handle_call(read_hash, _From, State) ->
    Hash = dog_ipset:read_hash(), {reply, Hash, State};
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call({set_state, State}, _From, _OldState) ->
    {reply, ok, State};
handle_call({get_group}, _From, State) ->
    Hostkey = dog_state:get_group(State),
    {reply, Hostkey, State};
handle_call({set_group, Group}, _From, StateOld) ->
    StateNew = dog_state:set_group(StateOld, Group),
    {reply, ok, StateNew};
handle_call({get_hostname}, _From, State) ->
    Hostkey = dog_state:get_hostname(State),
    {reply, Hostkey, State};
handle_call({set_hostname, Hostname}, _From,
        StateOld) ->
    StateNew = dog_state:set_hostname(StateOld, Hostname),
    {reply, ok, StateNew};
handle_call({get_location}, _From, State) ->
    Hostkey = dog_state:get_location(State),
    {reply, Hostkey, State};
handle_call({set_location, Location}, _From,
        StateOld) ->
    StateNew = dog_state:set_location(StateOld, Location),
    {reply, ok, StateNew};
handle_call({get_environment}, _From, State) ->
    Hostkey = dog_state:get_environment(State),
    {reply, Hostkey, State};
handle_call({set_environment, Environment}, _From,
        StateOld) ->
    StateNew = dog_state:set_environment(StateOld,
                     Environment),
    {reply, ok, StateNew};
handle_call({get_hostkey}, _From, State) ->
    Hostkey = dog_state:get_hostkey(State),
    {reply, Hostkey, State};
handle_call({set_hostkey, Hostkey}, _From, StateOld) ->
    StateNew = dog_state:set_hostkey(StateOld, Hostkey),
    {reply, ok, StateNew};
handle_call({get_interfaces}, _From, State) ->
    Hostkey = dog_state:get_interfaces(State),
    {reply, Hostkey, State};
handle_call({set_interfaces, Interfaces}, _From,
        StateOld) ->
    StateNew = dog_state:set_interfaces(StateOld,
                    Interfaces),
    {reply, ok, StateNew};
handle_call(host_routing_key, _From, State) ->
    {ok, RoutingKey} = dog_ips:do_get_host_routing_key(State),
    {reply, RoutingKey, State};
handle_call(group_routing_key, _From, State) ->
    {ok, RoutingKey} = dog_ips:do_get_group_routing_key(State),
    {reply, RoutingKey, State};
handle_call(watch_config, _From, State) ->
    dog_config:do_watch_config(), 
    {reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
-spec handle_cast(_, _) -> {noreply, _} |
               {stop, normal, _}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(Msg, State) ->
    ?LOG_ERROR("unknown_message: Msg: ~p, State: ~p",
        [Msg, State]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
% TODO: be more specific about Info in spec
-spec handle_info(term(),
          State :: dog_state:dog_state()) -> {stop, normal,
                              State ::
                              dog_state:dog_state()} |
                             {noreply,
                              State ::
                              dog_state:dog_state()}.

handle_info(sub, State) ->
    ?LOG_DEBUG("sub: ~p", [State]), {noreply, State};
handle_info(watch_interfaces, State) ->
    ?LOG_DEBUG("State: ~p", [State]),
    {ok, NewState} = dog_ips:do_watch_interfaces(State),
    WatchInterfacesPollMilliseconds = application:get_env(dog, watch_interfaces_poll_seconds, 5) * 1000,
    erlang:send_after(WatchInterfacesPollMilliseconds, self(), watch_interfaces),
    {noreply, NewState};
handle_info(keepalive, State) ->
    ?LOG_DEBUG("State: ~p", [State]),
    {ok, NewState} = dog_ips:do_keepalive(State),
    KeepalivePollSeconds = application:get_env(dog, keepalive_poll_seconds, 60) * 1000,
    erlang:send_after(KeepalivePollSeconds, self(), keepalive),
    {noreply, NewState};
handle_info(watch_docker, State) ->
    {ok, NewState} = dog_docker:do_watch_docker(State),
    WatchDockerPollMilliseconds = application:get_env(dog, watch_docker_poll_seconds, 15) * 1000,
    erlang:send_after(WatchDockerPollMilliseconds, self(), watch_docker),
    {noreply, NewState};
handle_info(Info, State) ->
    ?LOG_ERROR("unknown_message: Info: ~p, State: ~p",
        [Info, State]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
-spec terminate(_, dog_state:dog_state()) -> {close}.

terminate(Reason, State) ->
    ?LOG_INFO("terminate: Reason: ~p, State: ~p",
           [Reason, State]),
    {close}.

-spec code_change(_, State :: dog_state:dog_state(),
          _) -> {ok, State :: dog_state:dog_state()}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
