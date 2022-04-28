-module(dog_iptables_tests).

-include_lib("eunit/include/eunit.hrl").

-define(N(X), dog_iptables:normalize_ruleset(X)).

dog_iptables_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_C) ->
             [
                ?_assertMatch([], dog_iptables:read_current_ipv4_ipsets())
              , ?_assertMatch(["echo \"`/home/dog/bin/iptables-save -t filter`\""], dog_os_cmd_history())

              , ?_assertMatch([], dog_iptables:read_current_ipv4_iptables())
              , ?_assertMatch(["cat /etc/dog/ip4tables_iptables.txt"], dog_os_cmd_history())

              , ?_assertMatch([], dog_iptables:read_current_ipv6_ipsets())
              , ?_assertMatch(["echo \"`/home/dog/bin/ip6tables-save -t filter`\""], dog_os_cmd_history())

              , ?_assertMatch([], dog_iptables:read_current_ipv6_iptables())
              , ?_assertMatch(["cat /etc/dog/ip6tables_iptables.txt"], dog_os_cmd_history())


              , ?_assertMatch(<<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>, dog_iptables:create_hash(dog_iptables:read_current_ipv4_iptables()))
              , ?_assertMatch(["cat"++_], dog_os_cmd_history())

              , ?_assertMatch(ok, dog_iptables:ensure_iptables_consumer(<<>>))
              , ?_assertMatch([{dog_thumper_sup,ensure_consumer,[down,'iptables.']},
                               {dog_thumper_sup,ensure_consumer,
                                [up,'iptables.'|_]}], get_ensure_consumer_calls())

              , ?_assertMatch("",         ?N("# a comment"))               % remove comments
              , ?_assertMatch(":A [0:0]", ?N(":A [1:2]"))                  % zero counters
              , ?_assertMatch("",         ?N("\"''\""))                    % remove quotes
              , ?_assertMatch(":A",       ?N(":A \n"))                     % trim trailing whitespace
              , ?_assertMatch(":A",       ?N(":A\n\n"))                    % remove empty

              , ?_assertMatch("",         ?N("-A INPUT -i lxdbr0 eunit"))       % remove lxd
              , ?_assertMatch("",         ?N("-A FORWARD -o lxdbr0 eunit"))     % remove lxd
              , ?_assertMatch("",         ?N("-A FORWARD -i lxdbr0 eunit"))     % remove lxd
              , ?_assertMatch("",         ?N("-A POSTROUTING -o lxdbr0 eunit")) % remove lxd

              , ?_assertMatch(":A", ?N("-A DOCKER\n:DOCKER\n:A\n-A FORWARD -j DROP"))         % remove docker
              , ?_assertMatch("-A FORWARD -j REJECT --reject-with icmp-port-unreachable",
                              ?N("-A FORWARD -j REJECT --reject-with icmp-port-unreachable")) % remove docker

              , ?_assertMatch("",   dog_iptables:remove_comments("# a comment"))
              , ?_assertMatch("",   dog_iptables:remove_quotes("\"''\""))

              , ?_assertMatch(["","",":A",""], dog_iptables:remove_docker(["-A DOCKER",":DOCKER",":A","-A FORWARD -j DROP"]))

              , ?_assertMatch(1, dog_iptables:rule_count("-A eunit\n:A eunit"))
              , ?_assertMatch(ok, dog_iptables:subscribe_to_iptables_updates(#{broker => default, name => 'queue', queue => "queue"}))
              , ?_assertMatch([{dog_thumper_sup,ensure_consumer,
                                   [up,queue,default,<<"queue">>|_]}], get_ensure_consumer_calls())

              , ?_assertMatch(ok, dog_iptables:unsubscribe_to_iptables_updates(#{broker => default, name => 'queue', queue => "queue"}))
              , ?_assertMatch([{dog_thumper_sup,ensure_consumer,[down,queue]}], get_ensure_consumer_calls())

              , ?_assertMatch({ok, _}, dog_iptables:write_ipv4_ruleset("-A"))
              , ?_assertMatch({ok, _}, dog_iptables:write_ipv6_ruleset("-A"))

              , ?_assertMatch(ok, dog_iptables:persist_ipv4_tables())
              , ?_assertMatch(["echo \"`/home/dog/bin/iptables-save -t filter`\" > /etc/iptables/rules.v4"], dog_os_cmd_history())

              %% reset side-effect tracking for upcoming 'handle_callback' tests. We'll only check
              %% for non-empty side-effects. See other test files for testing the content of these
              %% side-effects.
              , ?_assertMatch(_,     dog_os_cmd_history())
              , ?_assertMatch(_,     get_file_history())

              %% no-op
              , ?_assertMatch(pass,  dog_iptables:handle_callback([], false, false, false, false))
              , ?_assertMatch([],    dog_os_cmd_history())
              , ?_assertMatch([],    get_file_history())

              %% ipsets update
              , ?_assertMatch(pass,  dog_iptables:handle_callback(["eunit"], false, false, false, false))
              , ?_assertMatch([_|_], dog_os_cmd_history())
              , ?_assertMatch([_|_], get_file_history())

              %% ipv4sets ruleset update
              , ?_assertMatch(pass,  dog_iptables:handle_callback([], "eunit", false, false, false))
              , ?_assertMatch([_|_], dog_os_cmd_history())
              , ?_assertMatch([_|_], get_file_history())

              %% ip6ipsets ruleset update
              , ?_assertMatch(ok,    dog_iptables:handle_callback([], false, "eunit", false, false))
              , ?_assertMatch([_|_], dog_os_cmd_history())
              , ?_assertMatch([_|_], get_file_history())

              %% ip4tables ruleset
              , ?_assertMatch(pass,  dog_iptables:handle_callback([], false, false, "eunit", false))
              , ?_assertMatch([],    dog_os_cmd_history())
              , ?_assertMatch([_|_], get_file_history())

              %% ip6tables ruleset
              , ?_assertMatch(pass,  dog_iptables:handle_callback([], false, false, false, "eunit"))
              , ?_assertMatch([],    dog_os_cmd_history())
              , ?_assertMatch([_|_], get_file_history())
             ]
     end}.

get_ensure_consumer_calls() ->
    Result = [ {dog_thumper_sup, ensure_consumer, X} || {_, {dog_thumper_sup, ensure_consumer, X}, _} <- meck:history(dog_thumper_sup) ],
    meck:reset(dog_thumper_sup),
    Result.

get_publishes() ->
    Result = [ {binary_to_term(Payload), Exchange, RoutingKey} || {_, {thumper, publish, [Payload, Exchange, RoutingKey]}, _} <- meck:history(thumper) ],
    meck:reset(thumper),
    Result.

dog_os_cmd_history() ->
    History = [ Cmd || {_, {dog_os, cmd, [Cmd]}, _} <- meck:history(dog_os) ],
    meck:reset(dog_os),
    History.

get_file_history() ->
    History = [ X || {_, X={file, _, _}, _} <- meck:history(file) ],
    meck:reset(file),
    History.

setup() ->
    dog_fixture:setup([dog_os, dog_thumper_allow, thumper_publish, file_write_nothing, timer_nosleep, hackney_ec2, lager_app, dog_ips_agent_create_ipsets]).

teardown(Context) ->
    dog_fixture:teardown(Context).
