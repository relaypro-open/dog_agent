#!/usr/bin/env escript
main([ConfigFile]) ->
    {ok, Terms} = file:consult(ConfigFile),
    io:format("~p~n",[Terms]).
