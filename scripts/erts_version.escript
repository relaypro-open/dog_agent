#!/usr/bin/env escript
%% -*- erlang -*-
main(_) ->
    Erts_Version = erlang:system_info(version),
    io:format("~s", [Erts_Version]).
