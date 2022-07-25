-module(dog_iptables_agent).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-include("dog.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/0
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([code_change/3, handle_call/3, handle_cast/2,
     handle_info/2, init/1, terminate/2]).

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
  State = #{},
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
          State :: #{}) -> {reply, ok, any()}.

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
handle_info(Info, State) ->
    logger:error("unknown_message: Info: ~p, State: ~p",
        [Info, State]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
-spec terminate(_, #{}) -> {close}.

terminate(Reason, State) ->
    logger:info("terminate: Reason: ~p, State: ~p",
           [Reason, State]),
    {close}.

-spec code_change(_, State :: #{},
          _) -> {ok, State :: #{}}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
