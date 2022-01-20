-module(dog_config_agent).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-include("dog.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

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
         start_link/0,
         watch_config/0
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([code_change/3, handle_call/3, handle_cast/2,
     handle_info/2, init/1, terminate/2]).

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
    {ok, RoutingKey} = get_host_routing_key(),
    binary_to_list(RoutingKey).

-spec get_group_routing_key() -> {ok,
                  State :: dog_state:dog_state()}.
get_group_routing_key() ->
    gen_server:call(?MODULE, group_routing_key).

-spec group_routing_key() -> string().
group_routing_key() ->
    {ok, RoutingKey} = get_group_routing_key(),
    binary_to_list(RoutingKey).

-spec watch_config() -> ok.

watch_config() ->
    gen_server:cast(?MODULE, watch_config).

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
init(_Args) ->
    ok = dog_config_agent:watch_config(),
    Provider = dog_interfaces:get_provider(),
    {ok, Interfaces} =
    dog_interfaces:get_interfaces(Provider, []),
    {Ec2InstanceId, Ec2AvailabilityZone, Ec2SecurityGroupIds, Ec2OwnerId} = dog_interfaces:ec2_info(),
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
            lager:debug("ConfigMap: ~p", [ConfigMap]),
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
    lager:info("Hostkey: ~p",[Hostkey1]),
    State = dog_state:dog_state(Group, Hostname,
                Location, Environment,
                Hostkey1, Interfaces, Version,
                Hash4Ipsets, Hash6Ipsets,
                Hash4Iptables, Hash6Iptables,
                Provider, UpdateType,
                IpsetHash,Ec2InstanceId,
                               Ec2AvailabilityZone,
                               Ec2SecurityGroupIds,
                               Ec2OwnerId),
    lager:debug("State: ~p", [State]),
    StateMap = dog_state:to_map(State),
    dog_interfaces:publish_to_queue(StateMap),
    lager:debug("StateMap: ~p~n", [StateMap]),
    lager:debug("State: ~p", [State]),
    lager:info("force update"),
  {ok, State}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, Pid :: pid()} | ignore |
              {error, {already_started, Pid :: pid()} | term()}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],
              []).
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
    {reply, RoutingKey, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
-spec handle_cast(_, _) -> {noreply, _} |
               {stop, normal, _}.
handle_cast(watch_config, State) ->
    dog_config:do_watch_config(), {noreply, State};
handle_cast(stop, State) -> {stop, normal, State};
handle_cast(Msg, State) ->
    lager:error("unknown_message: Msg: ~p, State: ~p",
        [Msg, State]),
    {noreply, State}.


%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
% TODO: be more specific about Info in spec
handle_info(Info, State) ->
    lager:error("unknown_message: Info: ~p, State: ~p",
        [Info, State]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
-spec terminate(_, dog_state:dog_state()) -> {close}.

terminate(Reason, State) ->
    lager:info("terminate: Reason: ~p, State: ~p",
           [Reason, State]),
    {close}.

-spec code_change(_, State :: dog_state:dog_state(),
          _) -> {ok, State :: dog_state:dog_state()}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
