-module(inet_utils).

-include("dog.hrl").

-export([inet_aton/1, inet_ntoa/1, inet_bits/1, mask_address/2]).
-export([ip_between/3]).

%% @doc Converts a binary string with a human readable ip
%% address representation into an uint32.
-spec inet_aton(binary()) -> pos_integer().
inet_aton(Ip) ->
  [O1Bin, O2Bin, O3Bin, O4Bin] = binary:split(Ip, <<".">>, [global]),
  B1 = binary_to_integer(O1Bin) bsl 24,
  B2 = binary_to_integer(O2Bin) bsl 16,
  B3 = binary_to_integer(O3Bin) bsl 8,
  B4 = binary_to_integer(O4Bin),
  B1 + B2 + B3 + B4.

%% @doc Converts the given uint32 into a binary string with the
%% human-readable ip address representation, i.e: <<"x.x.x.x">>.
-spec inet_ntoa(pos_integer()) -> binary().
inet_ntoa(Num) ->
  B1 = (Num band 2#11111111000000000000000000000000) bsr 24,
  B2 = (Num band 2#00000000111111110000000000000000) bsr 16,
  B3 = (Num band 2#00000000000000001111111100000000) bsr 8,
  B4 = Num band 2#00000000000000000000000011111111,
  <<
    (integer_to_binary(B1))/binary, ".",
    (integer_to_binary(B2))/binary, ".",
    (integer_to_binary(B3))/binary, ".",
    (integer_to_binary(B4))/binary
  >>.

%% @doc Checks if the given IP address falls into the given network
%% range. E.g: ip_between(<<"192.168.0.1">>, <<"192.168.0.0">>, 16).
-spec ip_between(binary(), binary(), pos_integer()) -> boolean().
ip_between(Ip, Network, NetworkBits) ->
  IpNum = inet_aton(Ip),
  NetLow = inet_aton(Network),
  BitsHosts = 32 - NetworkBits,
  NetHigh = NetLow + erlang:trunc(math:pow(2, BitsHosts)) - 1,
  IpNum >= NetLow andalso IpNum =< NetHigh.

%% @doc Converts a binary string with a human readable ip
%% address representation into bits.
-spec inet_bits(binary()) -> pos_integer().
inet_bits(Ip) ->
  [O1Bin, O2Bin, O3Bin, O4Bin] = binary:split(Ip, <<".">>, [global]),
  B1 = binary_to_integer(O1Bin) bsl 24,
  B2 = binary_to_integer(O2Bin) bsl 16,
  B3 = binary_to_integer(O3Bin) bsl 8,
  B4 = binary_to_integer(O4Bin),
  io:fwrite("~*.*.0s",[8, 8, integer_to_list(B1,2)]),
  io:fwrite("~*.*.0s",[8, 8, integer_to_list(B2,2)]),
  io:fwrite("~*.*.0s",[8, 8, integer_to_list(B3,2)]),
  io:fwrite("~*.*.0s~n",[8, 8, integer_to_list(B4,2)]).

mask_address(Addr, Maskbits) ->
  B = list_to_binary(tuple_to_list(Addr)),
  Rest = (size(B) * 8) - Maskbits,
  <<Subnet:Maskbits, _Host:Rest>> = B,
  Subnet bsl Maskbits.
