-module(dog_ipset).

-include("dog.hrl").

-export([create_hash/1, create_ipsets/1,
     normalize_ipset/1, read_current_ipset/0, read_hash/0,
	cleanup_ipset/0]).

-spec create_ipsets(IpsetConf :: iolist()) -> ok.

create_ipsets(IpsetConf) ->
    lager:debug("IpsetConf: ~p", [IpsetConf]),
    ok = write_temp_file(IpsetConf),
    case restore_ipset() of
      ok -> lager:info("Successfully restored ipset"), ok;
      {error,
       [{_RestoreError, _RestoreCode}, {stderr, _CmdError}]} ->
      lager:error("Error restoring ipset")
    end,
    ok = persist_ipset(),
    cleanup_ipset(),
    ok.

-spec write_temp_file(IpsetConf :: iolist()) -> ok |
                        {error, iolist()}.

write_temp_file(IpsetConf) ->
    lager:debug("IpsetConf: ~p", [IpsetConf]),
    {ok, TmpFile} = file:open((?RUNDIR) ++ "/ipset.txt",
                  [write]),
    Result = file:write(TmpFile, IpsetConf),
    file:close(TmpFile),
    case Result of
      ok -> lager:info("wrote ipset.txt"), ok;
      {error, Error} ->
      lager:error("error: ~p", [Error]), {error, Error}
    end.

-spec restore_ipset() -> ok | {error, list()}.

restore_ipset() -> restore_ipset(0).

-spec restore_ipset(ErrorCount :: integer()) -> ok |
                        {error, list()}.

restore_ipset(ErrorCount) ->
  IpsetRestoreRetryLimit = application:get_env(dog, ipset_restore_retry_limit, 2),
  IpsetRestoreRetryWaitSeconds = application:get_env(dog, ipset_restore_retry_wait_seconds, 2),
  IpsetRestoreWaitSeconds = application:get_env(dog, ipset_restore_wait_seconds, 3),
  RestoreCmd = "cat " ++ ?RUNDIR ++ "/ipset.txt | /home/dog/bin/ipset restore -exist", % no '-f' in old ipset on centos 6
  lager:debug("RestoreCmd: ~p", [RestoreCmd]),
  Result = os:cmd(RestoreCmd),
  timer:sleep(IpsetRestoreWaitSeconds * 1000),
  case Result of       % Run a shell command to sleep for 1000s.
    %dog_ipset:restore_ipset:38 gen_server dog_ipset_agent terminated with reason: no case clause matching {error,[{exit_status,256},{stderr,[<<"ipset v6.29: Error in line 4396: Kernel error received: Device or resource busy n">>]}]} 
    [] ->
      ok;
    RestoreError ->
      lager:error("RestoreError: ~p",[RestoreError]),       
      NextErrorCount = ErrorCount + 1,
      case ErrorCount of
        ErrorCount when ErrorCount =< IpsetRestoreRetryLimit ->
          timer:sleep(IpsetRestoreRetryWaitSeconds * 1000),
          lager:debug("Restore Retry number: ~p",[NextErrorCount]),
          restore_ipset(NextErrorCount),
          ok;
        ErrorCount when ErrorCount > IpsetRestoreRetryLimit  ->
          lager:error("Unable to restore after retry number: ~p",[ErrorCount]),
          {error, RestoreError}
      end
  end.

-spec persist_ipset() -> ok | {error, list()}.

persist_ipset() ->
    PersistCmd =
    "/home/dog/bin/ipset save | tee /etc/iptable"
    "s/rules.ipset",
    lager:debug("PersistCmd: ~p", [PersistCmd]),
    os:cmd(PersistCmd),
    ok.

-spec read_current_ipset() -> list() |
                  {error, list(), {stderr, iolist()}}.

read_current_ipset() ->
    ReadCmd = "/home/dog/bin/ipset save",
    lager:debug("ReadCmd: ~p", [ReadCmd]),
    case os:cmd(ReadCmd) of
        [] ->
            [];
        ReadCmdResult ->
            lager:debug("ReadCmdResult: ~p", [ReadCmdResult]),
            %io_lib:format("~s",[lists:flatten(lists:join("",ReadCmdResult))])
            L = io_lib:format("~s",[lists:flatten(ReadCmdResult)]),
            lists:flatten(L)
    end.

-spec create_hash(Ipset :: string()) -> any().

create_hash(Ipset) ->
    base16:encode(crypto:hash(sha256, Ipset)).

-spec read_hash() -> binary().

read_hash() ->
    NormalizedIpset = normalize_ipset(read_current_ipset()),
    IpsetHash = create_hash(NormalizedIpset),
    lager:info("ipset hash: ~p",[IpsetHash]),
    IpsetHash.

-spec match_only_add(Line :: iolist()) -> boolean().

match_only_add(Line) ->
    case re:run(Line, "^add (.*)") of
      {match, _} -> true;
      nomatch -> false
    end.

-spec normalize_ipset(Ipset :: iolist()) -> iolist().
normalize_ipset(Ipset) ->
    IpsetSplit = dog_string:split(Ipset,"\n",all),
    IpsetSorted = lists:sort(IpsetSplit),
    IpsetAddOnly = lists:filter(fun(X) -> match_only_add(X) end, IpsetSorted),
    IpsetNotNew = [lists:flatten(dog_string:replace(X,"n "," ",all)) || X <- IpsetAddOnly],
    IpsetNot32 = [lists:flatten(dog_string:replace(X,"/32","",all)) || X <- IpsetNotNew],
    IpsetNot128 = [lists:flatten(dog_string:replace(X,"/128","",all)) || X <- IpsetNot32],
    IpsetTrimmed = [dog_string:trim(Line,trailing," ") || Line <- IpsetNot128],
    IpsetNormalized = lists:flatten(lists:join("\n",IpsetTrimmed)),
    IpsetNormalized.

-spec cleanup_ipset() -> ok.
cleanup_ipset() ->
    lager:info("cleanup_ipset()"),
    _One = os:cmd("grep create /etc/dog/ipset.txt | awk '{print $2}' | sort | uniq > /etc/dog/1.tmp"),
    _Two = os:cmd("/home/dog/bin/ipset list -name | sort | uniq > /etc/dog/2.tmp"),
    Cmd = "for name in `comm -1 -3 /etc/dog/1.tmp /etc/dog/2.tmp`;do echo destroy $name;done > /etc/dog/ipset_cleanup.txt; cat /etc/dog/ipset_cleanup.txt | /home/dog/bin/ipset restore; rm /etc/dog/1.tmp /etc/dog/2.tmp",
    lager:debug("Cmd: ~p", [Cmd]),
    case os:cmd(Cmd) of
      [] ->
            ok;
      CmdError ->
          lager:error("Cmd: ~p", [Cmd]),
          lager:error("CmdError: ~p", [CmdError]),
          {error, CmdError}
    end.

