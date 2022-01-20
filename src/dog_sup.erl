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
          #{id => dog_config_agent,
            start => {dog_config_agent, start_link, []},
            restart => permanent, shutdown => 5000, type => worker,
            modules => [dog_config_agent]},
          #{id => dog_turtle_sup,
            start => {dog_turtle_sup, start_link, []},
            type => supervisor},
          #{id => dog_ips_agent,
            start => {dog_ips_agent, start_link, []},
            restart => permanent, shutdown => 5000, type => worker,
            modules => [dog_ips_agent]}
          ],
    {ok, {SupFlags, ChildSpecs}}.
