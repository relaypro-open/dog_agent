#!/usr/bin/env escript
%%! -noshell -noinput 

main([]) ->
    TmpDir="/etc/dog",
    Hash4Iptables = create_iptables_hash(element(2,file:read_file(io_lib:format("~s/ip4tables_iptables.txt",[TmpDir])))),
    %Hash4Ipsets = create_iptables_hash(element(2,file:read_file(io_lib:format("~s/ip4tables_ipsets.txt",[TmpDir])))),
    Ip4TablesIpsets = element(2,file:read_file(io_lib:format("~s/ip4tables_ipsets.txt",[TmpDir]))),
    %io:format(Ip4TablesIpsets,[]),
    Hash4Ipsets = create_iptables_hash(Ip4TablesIpsets),
    Hash6Iptables = create_iptables_hash(element(2,file:read_file(io_lib:format("~s/ip6tables_iptables.txt",[TmpDir])))),
    Hash6Ipsets = create_iptables_hash(element(2,file:read_file(io_lib:format("~s/ip6tables_ipsets.txt",[TmpDir])))),
    Ip4Tables = os:cmd("sudo /sbin/iptables-save -t filter"),
    Ip4TablesHash = create_iptables_hash(Ip4Tables),
    Ip6Tables = os:cmd("sudo /sbin/ip6tables-save -t filter"),
    Ip6TablesHash = create_iptables_hash(Ip6Tables),
    {ok,IpsetFile} = file:read_file(io_lib:format("~s/ipset.txt",[TmpDir])),
    Ipset = read_current_ipset(),
    IpsetNormalized = normalize_ipset(Ipset), 
    IpsetFileHash = create_hash(normalize_ipset(binary:bin_to_list(IpsetFile))),
    IpsetHash = create_hash(IpsetNormalized),
    {ok,GroupJson} = file:read_file("/etc/dog/config.json"),
  
    io:format("config.json:~n"),
    io:format("~s~n",[GroupJson]),
    io:format("~n",[]),
    io:format("Source:                  Hash:~n",[]),
    io:format("iptables-save:           ~s~n",[Ip4TablesHash]),
    io:format("ipv4tables_ipsets.txt:   ~s~n",[Hash4Ipsets]),
    io:format("ipv4tables_iptables.txt: ~s~n",[Hash4Iptables]),
    io:format("~n",[]),
    io:format("ip6tables-save:          ~s~n",[Ip6TablesHash]),
    io:format("ipv6tables_ipsets.txt:   ~s~n",[Hash6Ipsets]),
    io:format("ipv6tables_iptables.txt: ~s~n",[Hash6Iptables]),
    io:format("~n",[]),
    io:format("ipset save:              ~s~n",[IpsetHash]),
    io:format("ipset.txt:               ~s~n",[IpsetFileHash]).

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
    RulesetNoBlankLines = remove_empty_lists(RulesetNoDocker),
    RulesetNormalized = lists:flatten(lists:join("\n",RulesetNoBlankLines)),
    RulesetNormalized.

read_current_ipset() ->
    ReadCmd = "sudo /sbin/ipset save",
    case os:cmd(ReadCmd) of
        [] ->
            [];
        ReadCmdResult ->
            L = io_lib:format("~s",[lists:flatten(ReadCmdResult)]),
            lists:flatten(L)
    end.

match_only_add(Line) ->
    case re:run(Line, "^add (.*)") of
      {match, _} -> true;
      nomatch -> false
    end.

-spec normalize_ipset(Ipset :: iolist()) -> iolist().
normalize_ipset(Ipset) ->
    IpsetSplit = split(Ipset,"\n",all),
    IpsetSorted = lists:sort(IpsetSplit),
    IpsetAddOnly = lists:filter(fun(X) -> match_only_add(X) end, IpsetSorted),
    IpsetNotNew = [lists:flatten(replace(X,"n "," ",all)) || X <- IpsetAddOnly],
    IpsetNot32 = [lists:flatten(replace(X,"/32","",all)) || X <- IpsetNotNew],
    IpsetNot128 = [lists:flatten(replace(X,"/128","",all)) || X <- IpsetNot32],
    IpsetTrimmed = [trim(Line,trailing," ") || Line <- IpsetNot128],
    IpsetNormalized = lists:flatten(lists:join("\n",IpsetTrimmed)),
    IpsetNormalized.

create_iptables_hash(Ruleset) ->
    RulesetTrimmed = normalize_ruleset(Ruleset),
    create_hash(RulesetTrimmed).

create_hash(String) ->    
    BitString = encode(crypto:hash(sha256, String)),
    binary:list_to_bin(erlang:bitstring_to_list(BitString)).

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
replace(String,SearchPattern,Replacement,all) ->
    Replaced = re:replace(String,SearchPattern,Replacement,[global,{return,list}]),
    [Replaced].
