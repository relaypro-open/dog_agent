-module(dog_sup).

-include("dog.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 10,
         period => 60},
    ChildSpecs = [
          #{id => dog_signal_handler, 
            start => {dog_signal_handler, start_link, dog_signal_handler_opts()},
            restart => permanent, shutdown => 5000, type => worker,
            modules => [dog_signal_handler]},
          #{id => dog_turtle_sup,
            start => {dog_turtle_sup, start_link, []},
            type => supervisor},
          #{id => dog_agent,
            start => {dog_agent, start_link, []},
            restart => permanent, shutdown => 5000, type => worker,
            modules => [dog_agent]}
          ],
    {ok, {SupFlags, ChildSpecs}}.

dog_signal_handler_opts() ->
    TerminateDelayMilliseconds = application:get_env(dog, terminate_delay_seconds, 5) * 1000,
    [TerminateDelayMilliseconds].

