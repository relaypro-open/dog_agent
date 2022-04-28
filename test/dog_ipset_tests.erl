-module(dog_ipset_tests).

-include_lib("eunit/include/eunit.hrl").

-define(N(X), dog_ipset:normalize_ipset(X)).

dog_ipset_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_C) ->
             [
                ?_assertMatch(<<"cbb86bffb9a97498f64ec0c86da01c54962abaeba4f59eb1948bc666c4b69c06">>, dog_ipset:create_hash(<<"not_a_real_ipset">>))

              % ==================================================================
              % CREATION
              % ==================================================================
              , ?_assertMatch(ok, dog_ipset:create_ipsets([]))

              % confirms backup file was written to disk during test
              , ?_assertMatch([{_, {file, open, ["/etc/dog/ipset.txt", [write]]}, _}|_], meck:history(file))

              % confirms the list of commands executed during test
              , ?_assertMatch([
                  "cat /etc/dog/ipset.txt | /home/dog/bin/ipset restore -exist",
                  "/home/dog/bin/ipset save | tee /etc/iptables/rules.ipset",
                  "grep create /etc/dog/ipset.txt | awk '{print $2}' | sort | uniq > /etc/dog/1.tmp",
                  "/home/dog/bin/ipset list -name | sort | uniq > /etc/dog/2.tmp",
                  "for name in `comm -1 -3 /etc/dog/1.tmp /etc/dog/2.tmp`;do echo destroy $name;done > /etc/dog/ipset_cleanup.txt; cat /etc/dog/ipset_cleanup.txt | /home/dog/bin/ipset restore; rm /etc/dog/1.tmp /etc/dog/2.tmp"
                              ], dog_os_cmd_history())

              % ==================================================================
              % NORMALIZATION
              % ==================================================================
              % noop
              , ?_assertMatch(   "add A 127.0.0.1\nadd B 127.0.0.1",
                              ?N("add A 127.0.0.1\nadd B 127.0.0.1"))

              % sort
              , ?_assertMatch(   "add A 127.0.0.1\nadd B 127.0.0.1",
                              ?N("add B 127.0.0.1\nadd A 127.0.0.1"))

              % remove any line that does not start with "add "
              , ?_assertMatch(   "add A 127.0.0.1\nadd B 127.0.0.1",
                              ?N("add A 127.0.0.1\nadd B 127.0.0.1\ninvalid"))

              % remove trailing 'n' character: a Dog convention that related to tracking the lifecycles of ipsets
              % (more information needed)
              , ?_assertMatch(   "add A 127.0.0.1\nadd B 127.0.0.1",
                              ?N("add An 127.0.0.1\nadd B 127.0.0.1"))

              % remove trailing '/32' from addresses
              , ?_assertMatch(   "add A 127.0.0.1\nadd B 127.0.0.1",
                              ?N("add A 127.0.0.1/32\nadd B 127.0.0.1/32"))

              % remove trailing '/128' from addresses
              , ?_assertMatch(   "add A ::1\nadd B ::1",
                              ?N("add A ::1/128\nadd B ::1/128"))

              % remove trailing whitespace
              , ?_assertMatch(   "add A 127.0.0.1\nadd B 127.0.0.1",
                              ?N("add A 127.0.0.1   \nadd B 127.0.0.1   "))

              % ==================================================================
              % READING
              % ==================================================================
              , ?_assertMatch("add A 127.0.0.1/32", dog_ipset:read_current_ipset())
              , ?_assertMatch(["/home/dog/bin/ipset save"], dog_os_cmd_history())
              , ?_assertMatch(<<"3e75aaf2416a450fb747b9ef46b0bb31b2b8c016c809ad248cf726054e25fc0c">>, dog_ipset:read_hash())
              , ?_assertMatch(["/home/dog/bin/ipset save"], dog_os_cmd_history())

              % ==================================================================
              % CLEANUP
              % ==================================================================
              , ?_assertMatch(ok, dog_ipset:cleanup_ipset())
              , ?_assertMatch([
                  "grep create /etc/dog/ipset.txt | awk '{print $2}' | sort | uniq > /etc/dog/1.tmp",
                  "/home/dog/bin/ipset list -name | sort | uniq > /etc/dog/2.tmp",
                  "for name in `comm -1 -3 /etc/dog/1.tmp /etc/dog/2.tmp`;do echo destroy $name;done > /etc/dog/ipset_cleanup.txt; cat /etc/dog/ipset_cleanup.txt | /home/dog/bin/ipset restore; rm /etc/dog/1.tmp /etc/dog/2.tmp"
                              ], dog_os_cmd_history())
             ]
     end}.

dog_os_cmd_history() ->
    History = [ Cmd || {_, {dog_os, cmd, [Cmd]}, _} <- meck:history(dog_os) ],
    meck:reset(dog_os),
    History.

setup() ->
    dog_fixture:setup([file_write_nothing, dog_os, timer_nosleep]).

teardown(Context) ->
    dog_fixture:teardown(Context).
