-module(ec2_mock).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API exports
-export([main/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) -> ec2_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) -> ok.

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    ec2_sup:start_link(),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

