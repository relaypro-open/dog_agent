-module(dog_ipset_test).

-export([
         ip_tester/0,
         serial_cmd_tester/0,
         serial_tester/0,
         parallel_tester/0,
         read_tester/0,
         tester/0,
         write_tester/0
        ]).

tester() ->
  %parallel_tester().
  %serial_tester().
  ip_tester().

ip_tester() ->
  lists:foreach(fun(_X) ->
                    os:cmd("sudo ifup eth0:1"),
                    timer:sleep(3000),
                    os:cmd("sudo ifdown eth0:1"),
                    timer:sleep(3000)
                end, lists:seq(1,500)).

parallel_tester() ->
  spawn(write_tester()),
  spawn(read_tester()).

read_tester() ->
  lists:foreach(fun(_X) ->
                    io:format("~p~n",[dog_ipset:read_hash()])
                end, lists:seq(1,500)).

write_tester() ->
  {ok, Ipset} = file:read_file("/etc/dog/ipset.txt"),
  lists:foreach(fun(_X) ->
                    dog_ipset:create_ipsets(Ipset)
                end, lists:seq(1,500)).

serial_tester() ->
  lists:foreach(fun(_X) ->
                    %{ok, Ipset} = file:read_file("/etc/dog/ipset.txt"),
                    Ipset = dog_ipset:read_current_ipset(),
                    ok = dog_ipset:create_ipsets(Ipset),
                    io:format("~p~n",[dog_ipset:read_hash()])
                end, lists:seq(1,500)).

serial_cmd_tester() ->
  lists:foreach(fun(_X) ->
                    io:format("~s~n",[
                                      os:cmd("cat /etc/dog/ipset.txt | sudo /sbin/ipset restore -exist")
                                     ]),
                    io:format("~s~n",[
                                      os:cmd("sudo /sbin/ipset save | md5sum")
                                     ])
                end, lists:seq(1,500)).
