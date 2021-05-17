-module(dog_test).

-export([cleanup/1, setup/0]).

setup() ->
    lager:info("dog_test:setup()"),
    application:start(sasl),
    application:start(asn1),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(inets),
    application:start(hackney),
    thumper_app:start(),
    ok.

cleanup(_Ok) ->
    application:stop(thumper),
    application:stop(hackney),
    application:stop(inets),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto),
    application:stop(public_key),
    application:stop(asn1),
    application:stop(sasl),
    ok.


interfaces_empty_list_publish_tests() ->
  dog_interfaces:publish_to_queue(#{<<"group">> => [] , <<"name">> => [], <<"location">> => [], <<"environment">> => [], <<"hostkey">> => [], <<"interfaces">> => [], <<"version">> => [], <<"hash4_ipsets">> => [], <<"hash6_ipsets">> => [], <<"hash4_iptables">> => [], <<"hash6_iptables">> => [], <<"provider">> => [], <<"updatetype">> => []}).

interfaces_empty_map_publish_tests() ->
  dog_interfaces:publish_to_queue(#{}).
