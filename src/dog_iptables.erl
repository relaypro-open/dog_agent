-module(dog_iptables).
-include_lib("amqp_client/include/amqp_client.hrl").

-include("dog.hrl").

-export([
         create_hash/1,
         handle_callback/5,
         normalize_ruleset/1,
         read_current_ipv4_ipsets/0,
         read_current_ipv4_iptables/0,
         read_current_ipv6_ipsets/0,
         read_current_ipv6_iptables/0,
         remove_comments/1,
         remove_docker/1,
         remove_quotes/1,
         rule_count/1,
         subscriber_loop/4,
         write_ipv4_ruleset/1,
         write_ipv6_ruleset/1,
         persist_ipv4_tables/0
        ]).

-define(IP4TABLES_SAVE_COMMAND, "echo \"\`/home/dog/bin/iptables-save -t filter\`\"").
-define(IP6TABLES_SAVE_COMMAND, "echo \"\`/home/dog/bin/ip6tables-save -t filter\`\"").
-define(IP4TABLES_RESTORE_COMMAND, "/home/dog/bin/iptables-restore").
-define(IP6TABLES_RESTORE_COMMAND, "/home/dog/bin/ip6tables-restore").

write_ipv4_docker_nat_ruleset(Ruleset) ->
    Tempfile = (?RUNDIR) ++ "/iptables-docker-nat.txt",
    ok = file:write_file(Tempfile, Ruleset),
    {ok, Tempfile}.

write_ipv4_docker_filter_ruleset(Ruleset) ->
    Tempfile = (?RUNDIR) ++ "/iptables-docker-filter.txt",
    ok = file:write_file(Tempfile, Ruleset),
    {ok, Tempfile}.

-spec write_ipv4_ruleset(binary() |
             maybe_improper_list(binary() |
                         maybe_improper_list(any(),
                                 binary() |
                                 []) |
                         byte(),
                         binary() | [])) -> {ok, string()}.

write_ipv4_ruleset(Ruleset) ->
    Tempfile = (?RUNDIR) ++ "/iptables.txt",
    ok = file:write_file(Tempfile, Ruleset),
    {ok, Tempfile}.

-spec write_ipv6_ruleset(binary() |
             maybe_improper_list(binary() |
                         maybe_improper_list(any(),
                                 binary() |
                                 []) |
                         byte(),
                         binary() | [])) -> {ok, string()}.

write_ipv6_ruleset(Ruleset) ->
    Tempfile = (?RUNDIR) ++ "/ip6tables.txt",
    ok = file:write_file(Tempfile, Ruleset),
    {ok, Tempfile}.

-spec write_ipv4_iptables_ruleset(binary() |
                  maybe_improper_list(binary() |
                              maybe_improper_list(any(),
                                      binary() |
                                      []) |
                              byte(),
                              binary() | [])) -> ok.

write_ipv4_iptables_ruleset(Ruleset) ->
    Tempfile = (?RUNDIR) ++ "/ip4tables_iptables.txt",
    ok = file:write_file(Tempfile, Ruleset),
    ok.

-spec write_ipv6_iptables_ruleset(binary() |
                  maybe_improper_list(binary() |
                              maybe_improper_list(any(),
                                      binary() |
                                      []) |
                              byte(),
                              binary() | [])) -> ok.

write_ipv6_iptables_ruleset(Ruleset) ->
    Tempfile = (?RUNDIR) ++ "/ip6tables_iptables.txt",
    ok = file:write_file(Tempfile, Ruleset),
    ok.

-spec write_ipv4_ipsets_ruleset(binary() |
                maybe_improper_list(binary() |
                            maybe_improper_list(any(),
                                    binary() |
                                    []) |
                            byte(),
                            binary() | [])) -> ok.

write_ipv4_ipsets_ruleset(Ruleset) ->
    Tempfile = (?RUNDIR) ++ "/ip4tables_ipsets.txt",
    ok = file:write_file(Tempfile, Ruleset),
    ok.

-spec write_ipv6_ipsets_ruleset(binary() |
                maybe_improper_list(binary() |
                            maybe_improper_list(any(),
                                    binary() |
                                    []) |
                            byte(),
                            binary() | [])) -> ok.

write_ipv6_ipsets_ruleset(Ruleset) ->
    Tempfile = (?RUNDIR) ++ "/ip6tables_ipsets.txt",
    ok = file:write_file(Tempfile, Ruleset),
    ok.

-spec persist_ipv4_tables() -> error | ok.

persist_ipv4_tables() ->
    Enforcing = application:get_env(dog, enforcing, true),
    Cmd = case Enforcing of
        false ->
          ?IP4TABLES_SAVE_COMMAND ++ " > " ++ ?RUNDIR ++ "/rules.v4";
        true ->
          ?IP4TABLES_SAVE_COMMAND ++ " > /etc/iptables/rules.v4"
      end,
    Result = dog_os:cmd(Cmd),
    case Result of
      [] -> ok;
      _ ->
        ?LOG_ERROR("Result: ~p", [Result]),
        error
    end.

-spec rm_nat() -> string().
rm_nat() ->
"*nat
COMMIT
".

-spec apply_ipv4_ruleset(string()) -> error | ok.
apply_ipv4_ruleset(TrainerFilterFile) ->
  Enforcing = application:get_env(dog, enforcing, true),
  case Enforcing of
    false ->
      ok;
    true ->
      case dog_docker:is_docker_instance() of
        true ->
          {DockerNatRuleset,DockerFilterRuleset} = dog_docker:iptables(),
          write_ipv4_docker_nat_ruleset(DockerNatRuleset),
          write_ipv4_docker_filter_ruleset(DockerFilterRuleset),
          {ok, TrainerFilter} = file:read_file(TrainerFilterFile),
          TrainerFilterWithoutCommit = string:join(lists:subtract(dog_string:split(binary_to_list(TrainerFilter),"\n",all),["COMMIT"]),"\n"),
          DockerTrainerFilterFile = (?RUNDIR) ++ "/iptables-docker-trainer-filter.txt",
          file:write_file(DockerTrainerFilterFile,TrainerFilterWithoutCommit),
          DockerNatFile = (?RUNDIR) ++ "/iptables-docker-nat.txt",
          DockerFilterFile = (?RUNDIR) ++ "/iptables-docker-filter.txt",
          DockerIptablesFile = (?RUNDIR) ++ "/iptables-docker.txt",
          ConcatCmd = "cat " ++ DockerTrainerFilterFile ++ " " ++ DockerFilterFile ++ " " ++ DockerNatFile ++ " > " ++ DockerIptablesFile,
          dog_os:cmd(ConcatCmd),
          Cmd = ?IP4TABLES_RESTORE_COMMAND ++ " " ++ DockerIptablesFile, 
          Result = dog_os:cmd(Cmd),
          case Result of
            [] ->
              ?LOG_INFO("applied ipv4 ruleset"),
              ok;
            _ ->
              ?LOG_ERROR("validate_ipv4_ruleset Result: ~p", [Result]),
              error
          end;
        false ->
          RemoveNatFile = "/etc/dog/rm-nat.txt",
          file:write_file(RemoveNatFile,rm_nat()),
          Cmd = "cat " ++ TrainerFilterFile ++ " " ++ RemoveNatFile ++ " | " ++ ?IP4TABLES_RESTORE_COMMAND, 
          Result = dog_os:cmd(Cmd),
          case Result of
            [] ->
              ?LOG_INFO("applied ipv4 ruleset"),
              ok;
            _ ->
              ?LOG_ERROR("validate_ipv4_ruleset Result: ~p", [Result]),
              error
          end
      end
  end.

-spec persist_ipv6_tables() -> error | ok.

persist_ipv6_tables() ->
    Enforcing = application:get_env(dog, enforcing, true),
    Cmd = case Enforcing of
        false ->
        ?IP6TABLES_SAVE_COMMAND ++ " > " ++ (?RUNDIR) ++ "/rules.v6";
        true ->
        ?IP6TABLES_SAVE_COMMAND ++ " > /etc/iptables/rules.v6"
      end,
    Result = dog_os:cmd(Cmd),
    case Result of
      [] -> ok;
      _ -> error
    end.

-spec apply_ipv6_ruleset(string()) -> error | ok.

apply_ipv6_ruleset(TempFile) ->
    Enforcing = application:get_env(dog, enforcing, true),
    case Enforcing of
      false -> ok;
      true ->
      Cmd = ?IP6TABLES_RESTORE_COMMAND ++ " " ++ TempFile,
      Result = dog_os:cmd(Cmd),
      case Result of
        [] -> ?LOG_INFO("applied ipv6 ruleset"), ok;
        _ ->
        ?LOG_ERROR("validate_ipv6_ruleset Result: ~p",
                [Result]),
        error
      end
    end.

-spec read_current_ipv4_iptables() -> string().

read_current_ipv4_iptables() ->
    UseIpsets = application:get_env(dog, use_ipsets, true),
    Enforcing = application:get_env(dog, enforcing, true),
    Cmd = case Enforcing of
        false ->
        "cat " ++ (?RUNDIR) ++ "/ip4tables_iptables.txt";
        true ->
        case UseIpsets of
          false ->
            ?IP4TABLES_SAVE_COMMAND;
          true ->
            "cat " ++ (?RUNDIR) ++ "/ip4tables_iptables.txt"
        end
      end,
    Result = dog_os:cmd(Cmd),
    lists:flatten(Result).

-spec read_current_ipv6_iptables() -> string().

read_current_ipv6_iptables() ->
    UseIpsets = application:get_env(dog, use_ipsets, true),
    Enforcing = application:get_env(dog, enforcing, true),
    Cmd = case Enforcing of
        false ->
        "cat " ++ (?RUNDIR) ++ "/ip6tables_iptables.txt";
        true ->
        case UseIpsets of
          false ->
            ?IP6TABLES_SAVE_COMMAND;
          true ->
            "cat " ++ (?RUNDIR) ++ "/ip6tables_iptables.txt"
        end
      end,
    Result = dog_os:cmd(Cmd),
    Result.

-spec read_current_ipv4_ipsets() -> string().

read_current_ipv4_ipsets() ->
    UseIpsets = application:get_env(dog, use_ipsets, true),
    Enforcing = application:get_env(dog, enforcing, true),
    Cmd = case Enforcing of
        false -> "cat " ++ (?RUNDIR) ++ "/ip4tables_ipsets.txt";
        true ->
        case UseIpsets of
          false ->
            "cat " ++ (?RUNDIR) ++ "/ip4tables_ipsets.txt";
          true ->
            ?IP4TABLES_SAVE_COMMAND
        end
      end,
    Result = dog_os:cmd(Cmd),
    lists:flatten(Result).

-spec read_current_ipv6_ipsets() -> string().

read_current_ipv6_ipsets() ->
    UseIpsets = application:get_env(dog, use_ipsets, true),
    Enforcing = application:get_env(dog, enforcing, true),
    Cmd = case Enforcing of
        false -> "cat " ++ (?RUNDIR) ++ "/ip6tables_ipsets.txt";
        true ->
        case UseIpsets of
          false -> "cat " ++ (?RUNDIR) ++ "/ip6tables_ipsets.txt";
          true ->
            ?IP6TABLES_SAVE_COMMAND
        end
      end,
    Result = dog_os:cmd(Cmd),
    Result.

-spec backup_current_ipv4_iptables() -> error | ok.

backup_current_ipv4_iptables() ->
    Cmd = ?IP4TABLES_SAVE_COMMAND ++ " > " ++ (?RUNDIR) ++ "/iptables.back",
    Result = dog_os:cmd(Cmd),
    ?LOG_DEBUG("backup_ipv4_iptables Result: ~p",
        [Result]),
    case Result of
      [] -> ok;
      _ -> error
    end.

-spec backup_current_ipv6_iptables() -> error | ok.

backup_current_ipv6_iptables() ->
    Cmd = ?IP6TABLES_SAVE_COMMAND ++ " > " ++ (?RUNDIR) ++ "/ip6tables.back",
    Result = dog_os:cmd(Cmd),
    ?LOG_DEBUG("backup_ipv6_iptables Result: ~p",
        [Result]),
    case Result of
      [] -> ok;
      _ -> error
    end.

-spec update_iptables4(binary() |
               maybe_improper_list(binary() |
                       maybe_improper_list(any(),
                                   binary() | []) |
                       byte(),
                       binary() | [])) -> ok.

update_iptables4(Ruleset) ->
    update_iptables4(Ruleset, 0).

-spec update_iptables4(binary() |
               maybe_improper_list(binary() |
                       maybe_improper_list(any(),
                                   binary() | []) |
                       byte(),
                       binary() | []),
               integer()) -> ok.

update_iptables4(Ruleset, Retry) ->
    IptablesRestoreRetryLimit = application:get_env(dog,
                            iptables_restore_retry_limit,
                            5),
    IptablesRestoreRetryWaitSeconds =
    application:get_env(dog,
                iptables_restore_retry_wait_seconds, 3),
    ?LOG_DEBUG("update_iptables4"),
    ok = backup_current_ipv4_iptables(),
    {ok, TempFile} = write_ipv4_ruleset(Ruleset),
    case apply_ipv4_ruleset(TempFile) of
      ok -> ok = persist_ipv4_tables();
      error ->
      case Retry of
        R when R =< IptablesRestoreRetryLimit ->
        timer:sleep(IptablesRestoreRetryWaitSeconds * 1000),
        ?LOG_INFO("Retry count updating IPv4 iptables: ~p",
               [R + 1]),
        update_iptables4(Ruleset, R + 1);
        R when R > IptablesRestoreRetryLimit ->
        ?LOG_ERROR("Unable to restore iptables after retry "
                "number: ~p",
                [R])
      end
    end.

-spec update_iptables6(binary() |
               maybe_improper_list(binary() |
                       maybe_improper_list(any(),
                                   binary() | []) |
                       byte(),
                       binary() | [])) -> ok.

update_iptables6(Ruleset) ->
    update_iptables6(Ruleset, 0).

-spec update_iptables6(binary() |
               maybe_improper_list(binary() |
                       maybe_improper_list(any(),
                                   binary() | []) |
                       byte(),
                       binary() | []),
               integer()) -> ok.

update_iptables6(Ruleset, Retry) ->
    IptablesRestoreRetryLimit = application:get_env(dog,
                            iptables_restore_retry_limit,
                            5),
    IptablesRestoreRetryWaitSeconds =
    application:get_env(dog,
                iptables_restore_retry_wait_seconds, 3),
    ?LOG_DEBUG("update_iptables6"),
    ok = backup_current_ipv6_iptables(),
    {ok, TempFile} = write_ipv6_ruleset(Ruleset),
    case apply_ipv6_ruleset(TempFile) of
      ok -> ok = persist_ipv6_tables();
      error ->
      case Retry of
        R when R =< IptablesRestoreRetryLimit ->
        timer:sleep(IptablesRestoreRetryWaitSeconds * 1000),
        ?LOG_INFO("Retry count updating IPv6 iptables: ~p",
               [R + 1]),
        update_iptables6(Ruleset, R + 1);
        R when R > IptablesRestoreRetryLimit ->
        ?LOG_ERROR("Unable to restore iptables after retry "
                "number: ~p",
                [R])
      end
    end.

subscriber_loop(_RoutingKey, _CType, Payload, State) -> 
    ?LOG_DEBUG("Payload: ~p", [Payload]),
    Proplist = binary_to_term(Payload),
    ?LOG_DEBUG("Proplist: ~p", [Proplist]),
    UserData = proplists:get_value(user_data, Proplist),
    ?LOG_DEBUG("UserData: ~p", [UserData]),
    R4IpsetsRuleset = maps:get(ruleset4_ipset, UserData,
                   false),
    R6IpsetsRuleset = maps:get(ruleset6_ipset, UserData,
                   false),
    R4IptablesRuleset = maps:get(ruleset4_iptables,
                 UserData, false),
    R6IptablesRuleset = maps:get(ruleset6_iptables,
                 UserData, false),
    Ipsets = maps:get(ipsets, UserData, false),
    handle_callback(Ipsets, R4IpsetsRuleset,    
                    R6IpsetsRuleset, R4IptablesRuleset,
                          R6IptablesRuleset),
    {ack, State }.

handle_callback(Ipsets, R4IpsetsRuleset,    
                    R6IpsetsRuleset, R4IptablesRuleset,
                          R6IptablesRuleset) ->
    case Ipsets of
      [] -> pass;
      _ ->
      case application:get_env(dog, use_ipsets, true) of
        true -> ok = dog_ips_agent:create_ipsets(Ipsets);
        false -> pass
      end
    end,
    case R4IpsetsRuleset of
      false -> pass;
      _ ->
      ?LOG_DEBUG("R4IpsetsRuleset: ~p", [R4IpsetsRuleset]),
      ok = write_ipv4_ipsets_ruleset(R4IpsetsRuleset)
    end,
    case R6IpsetsRuleset of
      false -> pass;
      _ ->
      ?LOG_DEBUG("R6IpsetsRuleset: ~p", [R6IpsetsRuleset]),
      ok = write_ipv6_ipsets_ruleset(R6IpsetsRuleset)
    end,
    case R4IptablesRuleset of
      false -> pass;
      _ ->
      ?LOG_DEBUG("R4IptablesRuleset: ~p",
              [R4IptablesRuleset]),
      ok = write_ipv4_iptables_ruleset(R4IptablesRuleset)
    end,
    case R6IptablesRuleset of
      false -> pass;
      _ ->
      ?LOG_DEBUG("R6IptablesRuleset: ~p",
              [R6IptablesRuleset]),
      ok = write_ipv6_iptables_ruleset(R6IptablesRuleset)
    end,
    case application:get_env(dog, use_ipsets, true) of
      true ->
      case R4IpsetsRuleset of
        false ->
        ?LOG_INFO("No v4 ipset ruleset to apply"), pass;
        _ -> ok = update_iptables4(R4IpsetsRuleset)
      end,
      case R6IpsetsRuleset of
        false ->
        ?LOG_INFO("No v6 ipset ruleset to apply"), pass;
        _ -> ok = update_iptables6(R6IpsetsRuleset)
      end;
      false ->
      case R4IptablesRuleset of
        false ->
        ?LOG_INFO("No v4 iptables ruleset to apply"), pass;
        _ -> ok = update_iptables4(R4IptablesRuleset)
      end,
      case R6IptablesRuleset of
        false ->
        ?LOG_INFO("No v6 iptables ruleset to apply"), pass;
        _ -> ok = update_iptables6(R6IptablesRuleset)
      end
    end.

remove_comments(Ruleset) ->
  NoCommentRulesList = lists:filter(fun(X) -> case re:run(X,"^#") of nomatch -> true; _ -> false end end, split(Ruleset,"\n", all) ),
  NoCommentRules = lists:flatten(lists:join("\n",
                         NoCommentRulesList)),
  NoCommentRules.

remove_docker(Ruleset) ->
    lists:map(fun(Line0) ->
        Line1 = re:replace(Line0, "^-A DOCKER(.*)","",[{return,list}]),
        Line2 = re:replace(Line1, "^:DOCKER(.*)","",[{return,list}]),
        Line3 = case Line2 of
            "-A FORWARD -j REJECT --reject-with icmp-port-unreachable" ->
                Line2;
            _ ->
                re:replace(Line2, "^-A FORWARD(.*)","",[{return,list}])
        end,
        Line3
              end, Ruleset).

remove_lxd(Ruleset) ->
    lists:map(fun(Line0) ->
        Line1 = re:replace(Line0, "^-A INPUT -i lxdbr0 (.*)","",[{return,list}]),
        Line2 = re:replace(Line1, "^-A FORWARD -o lxdbr0 (.*)","",[{return,list}]),
        Line3 = re:replace(Line2, "^-A FORWARD -i lxdbr0 (.*)","",[{return,list}]),
        Line4 = re:replace(Line3, "^-A POSTROUTING -o lxdbr0 (.*)","",[{return,list}]),
        Line4
              end, Ruleset).

remove_empty_lists(List) ->
  [L || L <- List, L =/= []].

remove_quotes(Line0) ->
    Line1 = re:replace(Line0, "\"", "", [{return, list},global]),
    Line2 = re:replace(Line1, "\'", "", [{return, list},global]),
    Line2.

-spec zero_counters(Ruleset :: iolist()) -> iolist().
zero_counters(Ruleset) ->
    re:replace(Ruleset, "(:.*) \\[.*\\]", "\\1 [0:0]",
           [{return, list}, global]).

-spec normalize_ruleset(Ruleset ::
                iolist()) -> iolist().
normalize_ruleset(Ruleset) ->
    RulesetNoComments = remove_comments(Ruleset),
    RulesetZeroed = zero_counters(RulesetNoComments),
    RulesetSplit = split(RulesetZeroed, "\n", all),
    RulesetNoQuotes = [remove_quotes(Line) || Line <- RulesetSplit],
    RulesetTrimmed = [trim(Line, trailing, " ") || Line <- RulesetNoQuotes],
    RulesetNoDocker = remove_docker(RulesetTrimmed),
    RulesetNoLxd = remove_lxd(RulesetNoDocker),
    RulesetNoBlankLines = remove_empty_lists(RulesetNoLxd),
    RulesetNormalized = lists:flatten(lists:join("\n",RulesetNoBlankLines)),
    RulesetNormalized.

-spec create_hash(Ruleset :: string()) -> any().

create_hash(Ruleset) ->
    RulesetTrimmed = normalize_ruleset(Ruleset),
    ?LOG_DEBUG("RulesetTrimmed: ~p", [RulesetTrimmed]),
    encode(crypto:hash(sha256, RulesetTrimmed)).

-spec rule_count(Ruleset :: string()) -> number().
rule_count(Ruleset) ->
  Rules = lists:filter(fun(X) -> case re:run(X,"^-A\s") of nomatch -> false; _ -> true end end, dog_string:split(Ruleset,"\n", all) ),
  length(Rules).

encode(Data) ->
    << <<(hex(N div 16)), (hex(N rem 16))>> || <<N>> <= Data >>.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

hex(N) when N < 10 ->
    N + $0;
hex(N) when N < 16 ->
    N - 10 + $a.

%For pre-2X Erlang:
trim(String, trailing, " ") ->
    re:replace(re:replace(String, "\\s+$", "",
              [global, {return, list}]),
           "^\\s+", "", [global, {return, list}]).

split(String, Delimiter, all) ->
    split(String, Delimiter).
split(String, Delimiter) ->
    re:split(String, Delimiter, [{return, list}]).
