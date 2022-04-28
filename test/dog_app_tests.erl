%% Template module for dog module tests
-module(dog_app_tests).

-include_lib("eunit/include/eunit.hrl").

dog_app_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_C) ->
             [
              ?_assertMatch({ok, <<"0.0">>}, dog_app:get_version())
             ]
     end}.

setup() ->
    dog_fixture:setup([dog_version]).

teardown(Context) ->
    dog_fixture:teardown(Context).
