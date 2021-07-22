-module(dog_lxd).

-export([
         any_lxd_containers/0,
         containers/0,
         get_lxd_url/0,
         init/0,
         is_lxd_instance/0,
         iptables/0
        ]).

%snap: /var/snap/lxd/common/lxd/unix.socket
%ubuntu apt: /var/lib/lxd/unix.socket
init() ->
    case check_lxd_firewall_rules() of
        true ->
            application:set_env(dog, lxd_instance, true),
            {ok,Url} = get_lxd_url(),
            application:set_env(dog, lxd_url, Url);
        false ->
            application:set_env(dog, lxd_instance, false)
    end.

-spec check_lxd_firewall_rules() -> boolean().
check_lxd_firewall_rules() ->
    Ipv4Tables = dog_iptables:read_current_ipv4_ipsets(complete),
    case re:run(Ipv4Tables,"lxdbr0") of
        {match,_} ->
            true;
        nomatch ->
            false
    end.

is_lxd_instance() ->
    application:get_env(dog, lxd_instance, false).

get_lxd_url() ->
    Urls = application:get_env(dog, lxd_urls, 
                               [<<"http+unix://%2Fvar%2Flib%2Flxd%2Funix.socket">>,
                                <<"http+unix://%2Fvar%2Fsnap%2Flxd%2Fcommon%2Flxd%2Funix.socket">>]),
    get_lxd_url(Urls).

-spec get_lxd_url(UrlList :: list()) -> {ok, Url :: binary()} | error.
get_lxd_url([]) -> error; 
get_lxd_url(UrlList) -> 
    [Url|H] = UrlList, 
    Response = hackney:request(get, 
                         Url, 
                         [], 
                         <<>>, 
                         [{pool, default},{connect_timeout, 1000}]),
    case Response of
       {ok,_,_,_} -> 
         {ok,Url};
       {error, _} ->
          get_lxd_url(H)
     end.

containers() ->
    BaseUrl = application:get_env(dog, lxd_url,<<"http+unix://%2Fvar%2Flib%2Flxd%2Funix.socket">>),
    Url = <<BaseUrl/binary, <<"/1.0/containers">>/binary>>,
    Method = get,
    Headers = [],
    Payload = <<>>,
    Options = [{connect_timeout,1000}],
    % -> {ok, integer(), list(), client_ref()} | {ok, integer(), list()} | {error, term()}
    case hackney:request(Method, Url, Headers, Payload, Options) of
        {error, _Error} ->
            lager:error("Error checking if softlayer instance"),
            false;
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            case StatusCode of
                200 -> 
                    {ok,Body} = hackney:body(ClientRef),
                    ResponseMap = jsx:decode(Body,[return_maps]),
                    List = maps:get(<<"metadata">>,ResponseMap),
                    List; 
                _ -> 
                    false
            end
    end.


-spec any_lxd_containers() -> boolean().
any_lxd_containers() ->
  {ok,Containers} = containers(),
  case Containers of
    [] ->
      false;
    _ ->
      true
  end.

-spec iptables() -> {IptablesNat :: iolist(), IptablesFilter :: iolist()}.
iptables() ->
  case is_lxd_instance() of
     false ->
         pass;
     true ->
        os:cmd("sudo systemctl restart lxd-bridge.service")
  end.
