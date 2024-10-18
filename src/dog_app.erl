%%%-------------------------------------------------------------------
%% @doc dog public API
%% @end
%%%-------------------------------------------------------------------

-module(dog_app).

-include("dog.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([get_version/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    %ok = write_pid_file(), 
    dog_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
-spec get_version() -> {ok, binary()}.

get_version() ->
    {ok, Version} = application:get_env(dog, version),
    {ok, binary:list_to_bin(Version)}.
