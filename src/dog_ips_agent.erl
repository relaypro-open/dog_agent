-module(dog_ips_agent).

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
     start_link/0,  watch_interfaces/0,
     watch_iptables/0,
     create_ipsets/1, read_hash/0
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

-spec read_hash() -> Hash :: list().

read_hash() ->
  try 
    gen_server:call(?MODULE, read_hash, 20000)
  catch 
    Class:Reason:Stacktrace -> 
      logger:error(
              "~nStacktrace:~s",
              [logger:pr_stacktrace(Stacktrace, {Class, Reason})]),
      {Class, Reason} 
  end.

-spec create_ipsets(Ipsets :: iolist()) -> ok.

create_ipsets(Ipsets) ->
  try 
    gen_server:call(?MODULE, {create_ipsets,Ipsets}, 20000)
  catch 
    Class:Reason:Stacktrace -> 
      logger:error(
              "~nStacktrace:~s",
              [logger:pr_stacktrace(Stacktrace, {Class, Reason})]),
      {Class, Reason} 
  end.


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
    WatchInterfacesPollMilliseconds = application:get_env(dog, watch_interfaces_poll_seconds, 5) * 1000,
    _IpsTimer = erlang:send_after(WatchInterfacesPollMilliseconds, self(),
                  watch_interfaces),
    KeepalivePollMilliseconds = application:get_env(dog, keepalive_initial_delay_seconds, 60) * 1000,
    _KeepaliveTimer = erlang:send_after(KeepalivePollMilliseconds, self(),
                    keepalive),
    NewState = dog_config_agent:get_state(),
    dog_ips:do_watch_interfaces(NewState), %send initial force update
    logger:error("NewState: ~p",[NewState]),
    logger:error("hostname: ~p",[dog_state:get_hostname(NewState)]),
    %{ok, State} = dog_ips:do_watch_iptables(NewState),
    {ok, NewState}.

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
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
%handle_call(watch_iptables, _From, _State) ->
%    %{ok, NewState} = dog_ips:do_watch_iptables(State),
%    NewState = dog_config_agent:get_state(),
%    {ok, _} = dog_ips:do_watch_iptables(NewState),
%    {reply, NewState};
%handle_call(watch_interfaces, _From, State) ->
%    logger:debug("State: ~p", [State]),
%    {ok, NewState} = dog_ips:do_watch_interfaces(State),
%    erlang:send_after(10000, self(), watch_interfaces),
%    {reply, NewState};

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
    logger:error("unknown_message: Msg: ~p, State: ~p",
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
    logger:debug("sub: ~p", [State]), {noreply, State};
handle_info(watch_interfaces, State) ->
    logger:debug("State: ~p", [State]),
    {ok, NewState} = dog_ips:do_watch_interfaces(State),
    WatchInterfacesPollMilliseconds = application:get_env(dog, watch_interfaces_poll_seconds, 5) * 1000,
    erlang:send_after(WatchInterfacesPollMilliseconds, self(), watch_interfaces),
    {noreply, NewState};
handle_info(keepalive, State) ->
    {ok, NewState} = dog_ips:do_keepalive(State),
    KeepalivePollSeconds = application:get_env(dog, keepalive_poll_seconds, 60) * 1000,
    erlang:send_after(KeepalivePollSeconds, self(), keepalive),
    {noreply, NewState};
handle_info(Info, State) ->
    logger:error("unknown_message: Info: ~p, State: ~p",
        [Info, State]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
-spec terminate(_, dog_state:dog_state()) -> {close}.

terminate(Reason, State) ->
    logger:info("terminate: Reason: ~p, State: ~p",
           [Reason, State]),
    {close}.

-spec code_change(_, State :: dog_state:dog_state(),
          _) -> {ok, State :: dog_state:dog_state()}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
