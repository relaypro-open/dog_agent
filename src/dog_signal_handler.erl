-module(dog_signal_handler).
-behaviour(gen_event).
%% signal event handler.
%% Replacement for default `erl_signal_handler`.

-include("dog.hrl").

-export([start_link/0, init/1, handle_event/2, handle_info/2, handle_call/2, terminate/2]).

%% Arguments:
%% ShutdownDelay - delay (in ms) before stopping the VM after `sigterm`.
%%
%% In test mode no actual `init:stop/1` call is made, since this would interfere with tests!
%%
%%
%% Returns `ignore` since this doesn't actually start a process itself,
%% it gets the `gen_event` server to start one.
%%
start_link() ->
    ShutdownDelay = application:get_env(dog, shutdown_delay_seconds, 5) * 1000,
    ?LOG_INFO("*** Swapping in dog_signal_handler", []),
    ok = gen_event:swap_handler(
        erl_signal_server,
        {erl_signal_handler, []},
        {dog_signal_handler, [ShutdownDelay]}),
    ignore.

% note weird signature because we use gen_event:swap_sup_handler/3
init({[ShutdownDelay], _}) ->
    {ok, {ShutdownDelay}}.

handle_event(sigterm, {ShutdownDelay} = State) ->
    ?LOG_INFO("*** SIGTERM received. Sending inactive update and then stopping in ~p ms~n",
              [ShutdownDelay]),
    erlang:send_after(ShutdownDelay, self(), stop),
    dog_agent:terminate("Application shutdown",{}),
    {ok, State};
handle_event(ErrorMsg, S) ->
    % everything else goes to default handler
    erl_signal_handler:handle_event(ErrorMsg, S),
    {ok, S}.

handle_info(stop, State) ->
    ?LOG_INFO("*** Stopping due to earlier SIGTERM~n", []),
    ok = init:stop(),
    {ok, State};
handle_info(_, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

terminate(_Args, {_, _, false}) ->
    ok;
terminate(_Args, _State) ->
    ?LOG_INFO("*** Handler Terminating~n", []),
    ok.
