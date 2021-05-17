-module(ec2_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ElliOpts = [{callback, ec2_callback}, {port, 3000}],
    ElliSpec = {ec2_http, {elli, start_link, [ElliOpts]},
		permanent, 5000, worker, [elli]},
    {ok, {{one_for_one, 5, 10}, [ElliSpec]}}.
