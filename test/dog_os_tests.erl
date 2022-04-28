-module(dog_os_tests).

-include_lib("eunit/include/eunit.hrl").

dog_os_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_C) ->
             [
              ?_assertMatch("eunit\n", dog_os:cmd("echo eunit"))
             ]
     end}.

setup() ->
    ok.

teardown(_Context) ->
    ok.
