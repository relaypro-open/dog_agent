%% Wrapper around os:cmd/1
%%
%% Reasons:
%%    1. The os module cannot be mocked with meck
-module(dog_os).

-include("dog.hrl").

-export([cmd/1]).

cmd(Cmd) -> os:cmd(Cmd).
