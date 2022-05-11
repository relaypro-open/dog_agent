-module(dog_fixture).

-export([setup/1, teardown/1,
        group/0, location/0, environment/0,
        hostkey/0, hostname/0, interfaces/0,
        mac/0, public_ip/0,

        get_state/2,

        group_routing_key_expect/0, host_routing_key_expect/0,
        routing_key_expect/0, interfaces_expect/0]).

-include_lib("eunit/include/eunit.hrl").

-define(Timeout, 10000).

%% Input data
group() -> <<"eunit-group">>.
location() -> <<"eunit-location">>.
environment() -> <<"eunit-environment">>.
hostkey() -> <<"eunit-hostkey">>.
hostname() -> <<"eunit-hostname">>.
interfaces() ->
    [
     {"lo", [{flags, [up,loopback,running]},
             {addr, {127,0,0,1}}]},
     {"eth0", [{flags, [up,broadcast,running,multicast]},
               {addr, {10,0,0,1}}]}
    ].
mac() -> "00:11:22:33:44:ff".
public_ip() -> "11.11.11.11".

vars() ->
    #{group => group(),
      location => location(),
      environment => environment(),
      hostkey => hostkey(),
      interfaces => interfaces(),
      hostname => hostname(),
      mac => mac(),
      public_ip => public_ip()
     }.

%% Expect data (transformations of input)
group_routing_key_expect() -> "eunit-environment.eunit-location.eunit-group.*".
host_routing_key_expect()  -> "eunit-environment.eunit-location.*.eunit-hostkey".
routing_key_expect()       -> "eunit-environment.eunit-location.eunit-group.eunit-hostkey".
interfaces_expect()        -> [{<<"lo">>,[<<"127.0.0.1">>]},{<<"eth0">>,[<<"10.0.0.1">>]}].

get_state(Resource, {_, _, State}) ->
    maps:get(Resource, State).

setup(Targets) ->
    error_logger:tty(false), %% hides stdout from application stops

    {ok, Apps} = application:ensure_all_started(liet),

    % For meck to run in parallel, cover must be explicitly started first.
    cover:start(),

    {ok, Pid} = gen_liet:start_link(dog_liet, #{targets => Targets,
                                                vars => vars(),
                                                apply_timeout => ?Timeout}),
    {Pid, Apps, gen_liet:get_state(Pid, ?Timeout)}.

teardown({Pid, Apps, _}) ->
    %% gen_liet must be stopped explicitly because the link termination takes
    %% too long, and meck has parallelization problems

    gen_liet:stop(Pid, ?Timeout),
    lists:foreach(fun(App) ->
         io:format("Stopping App: ~p~n",[App]), 
         application:stop(App)
                  end, lists:reverse(Apps)).
    %[application:stop(X) || X <- lists:reverse(Apps)].
