-module(dog_string).

-include("dog.hrl").

-export([split/2, split/3, trim/3,replace/4]).

-spec split(String :: string() , Delimiter :: string()) -> list().
-ifdef(OTP_RELEASE).
    %% Code that will work in OTP 20 or higher
  -if(?OTP_RELEASE >= 20).
split(String, Delimiter, all) ->
    string:split(String, Delimiter, all).
split(String, Delimiter) ->
    string:split(String, Delimiter).
  -endif.
-else.
  %% OTP 20 or lower.
split(String, Delimiter, all) ->
    split(String, Delimiter).
split(String, Delimiter) ->
    re:split(String, Delimiter, [{return, list}]).
-endif.

-spec trim(String :: string() , trailing, TrimCharacters :: string()) -> list().
-ifdef(OTP_RELEASE).
    %% Code that will work in OTP 20 or higher
  -if(?OTP_RELEASE >= 20).
trim(String, trailing, " ") ->
    string:trim(String, trailing, " ").
  -endif.
-else.
  %% OTP 20 or lower.
trim(String, trailing, " ") ->
    re:replace(re:replace(String, "\\s+$", "",
              [global, {return, list}]),
           "^\\s+", "", [global, {return, list}]).
-endif.

replace(String,SearchPattern,Replacement,all) ->
    Replaced = re:replace(String,SearchPattern,Replacement,[global,{return,list}]),
    [Replaced].
