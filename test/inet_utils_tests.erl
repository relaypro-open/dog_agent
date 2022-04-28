-module(inet_utils_tests).

-include_lib("eunit/include/eunit.hrl").

inet_utils_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_C) ->
             [
                ?_assertMatch(2130706433, inet_utils:inet_aton(<<"127.0.0.1">>))
              , ?_assertMatch(<<"127.0.0.1">>, inet_utils:inet_ntoa(2130706433))
              , ?_assertMatch(ok, inet_utils:inet_bits(<<"127.0.0.1">>))
              , ?_assertMatch(32512, inet_utils:mask_address({127,0,0,1}, 8))
              , ?_assertMatch(true, inet_utils:ip_between(<<"127.0.0.1">>, <<"127.0.0.0">>, 8))
              , ?_assertMatch(false, inet_utils:ip_between(<<"10.0.0.1">>, <<"127.0.0.0">>, 8))
             ]
     end}.

setup() ->
    ok.

teardown(_Context) ->
    ok.
