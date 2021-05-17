-module(dog_ipset_test).

-export([
         ip_test/0,
         serial_cmd_test/0,
         serial_test/0,
         parallel_test/0,
         read_test/0,
         test/0,
         write_test/0
        ]).

test() ->
  %parallel_test().
  %serial_test().
  ip_test().

ip_test() ->
  lists:foreach(fun(_X) ->
                    os:cmd("sudo ifup eth0:1"),
                    timer:sleep(3000),
                    os:cmd("sudo ifdown eth0:1"),
                    timer:sleep(3000)
                end, lists:seq(1,500)).

parallel_test() ->
  spawn(write_test()),
  spawn(read_test()).

read_test() ->
  lists:foreach(fun(_X) ->
                    io:format("~p~n",[dog_ipset:read_hash()])
                end, lists:seq(1,500)).

write_test() ->
  {ok, Ipset} = file:read_file("/etc/dog/ipset.txt"),
  lists:foreach(fun(_X) ->
                    dog_ipset:create_ipsets(Ipset)
                end, lists:seq(1,500)).

serial_test() ->
  lists:foreach(fun(_X) ->
                    %{ok, Ipset} = file:read_file("/etc/dog/ipset.txt"),
                    Ipset = dog_ipset:read_current_ipset(),
                    ok = dog_ipset:create_ipsets(Ipset),
                    io:format("~p~n",[dog_ipset:read_hash()])
                end, lists:seq(1,500)).

serial_cmd_test() ->
  lists:foreach(fun(_X) ->
                    io:format("~s~n",[
                                      os:cmd("cat /etc/dog/ipset.txt | sudo /sbin/ipset restore -exist")
                                     ]),
                    io:format("~s~n",[
                                      os:cmd("sudo /sbin/ipset save | md5sum")
                                     ])
                end, lists:seq(1,500)).
