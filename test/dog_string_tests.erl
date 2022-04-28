-module(dog_string_tests).

-include_lib("eunit/include/eunit.hrl").

dog_string_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_C) ->
             [
                ?_assertMatch(["a", "b"], dog_string:split("a,b", ","))
              , ?_assertMatch(["a", "b"], dog_string:split("a,b", ",", all))
              , ?_assertMatch("a", dog_string:trim("a ", trailing, " "))
              , ?_assertMatch(["b"], dog_string:replace("ab", "a", "", all))
             ]
     end}.

setup() ->
    ok.

teardown(_Context) ->
    ok.
