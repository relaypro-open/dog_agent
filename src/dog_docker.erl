-module(dog_docker).

-include("dog.hrl").

-export([
         any_docker_containers/0,
         container_nets/3,
         container_networks/0,
         default_network/0,
         do_watch_docker/1,
         for_default_network/2,
         get_container_ids/0,
         get_interfaces/2,
         iptables/0,
         is_docker_instance/0,
         nets/0,
         per_container/3,
         per_network/2
        ]).

-spec is_docker_instance() -> boolean().
is_docker_instance() ->
 DockerSocketUrl = maps:get(docker_http,maps:from_list(application:get_all_env(erldocker)),<<"http+unix://%2Fvar%2Frun%2Fdocker.sock">>),
 case hackney:get(DockerSocketUrl) of
   {ok,_,_,_} -> 
     ?LOG_INFO("is_docker_instance: true"),
     true;
   {error, _} -> 
     ?LOG_INFO("is_docker_instance: false"),
     false
 end.

-spec any_docker_containers() -> boolean().
any_docker_containers() ->
  case is_docker_instance() of
    true ->
      {ok,Containers} = docker_container:containers(),
      case Containers of
        [] ->
          false;
        _ ->
         ?LOG_DEBUG("containers: ~p",[Containers]),
          true
      end;
    false ->
      false
  end.

-spec get_container_ids() -> list().
get_container_ids() ->
    {ok, Containers} = docker_container:containers(),
    [maps:get(<<"Id">>,jsn:new(Container)) || Container <- Containers].

-spec do_watch_docker(State :: map()) -> NewState :: map().
do_watch_docker(State) ->
  case is_docker_instance() of
    true ->
        OldDockerContainerIds = dog_state:get_docker_container_ids(State),
        NewDockerContainerIds = get_container_ids(),
        case ordsets:from_list(OldDockerContainerIds) =/= ordsets:from_list(NewDockerContainerIds) of
            true ->
                ?LOG_DEBUG("NewDockerContainerIds: ~p",[NewDockerContainerIds]),
                NewState = dog_state:set_docker_container_ids(State,NewDockerContainerIds),
                dog_iptables:recreate_ruleset(),
                {ok, NewState};
            false ->
                {ok, State}
        end;
    false ->
      false
  end.

-spec iptables() -> {IptablesNat :: iolist(), IptablesFilter :: iolist()}.
iptables() ->
  DefaultNetwork = default_network(),
  ContainerNetworks = container_networks(),
  Networks = nets(),
%  case ContainerNetworks of 
%    [] ->
%      {[],[]};
%    _ -> 
    D = fun(Template) ->
            for_default_network(DefaultNetwork,Template)
        end,
    C = fun(Template) ->
             per_container(ContainerNetworks,DefaultNetwork,Template)
         end,
    CP = fun(Template) ->
             per_public_container(ContainerNetworks,DefaultNetwork,Template)
         end,
    N = fun(Template) ->
             per_network(Networks,Template)
         end,
    P = fun(String) ->
             io_lib:format("~s",[String])
         end,
    ?LOG_DEBUG("~p~n",[DefaultNetwork]),
    ?LOG_DEBUG("~p~n~p~n",[ContainerNetworks,DefaultNetwork]),
    IptablesNat = [ 
                D(<<"*nat
:PREROUTING ACCEPT [0:0]
:INPUT ACCEPT [0:0]
:OUTPUT ACCEPT [0:0]
:POSTROUTING ACCEPT [0:0]
:DOCKER - [0:0]
-A PREROUTING -m addrtype --dst-type LOCAL -j DOCKER
-A OUTPUT ! -d 127.0.0.0/8 -m addrtype --dst-type LOCAL -j DOCKER">>),
                D(<<"-A POSTROUTING -s {{container_network}} ! -o {{bridge_interface}} -j MASQUERADE">> ),
                C(<<"-A POSTROUTING -s {{container_ip}}/32 -d {{container_ip}}/32 -p {{protocol}} -m {{protocol}} --dport {{private_port}} -j MASQUERADE">> ),
                N(<<"-A POSTROUTING -s {{subnet}} ! -o {{bridge_interface}} -j MASQUERADE">>),
                D(<<"-A DOCKER -i {{bridge_interface}} -j RETURN">>),
                C(<<"-A DOCKER -i {{bridge_interface}} -j RETURN">>),
                CP(<<"-A DOCKER ! -i {{bridge_interface}} -p {{protocol}} -m {{protocol}} --dport {{public_port}} -j DNAT --to-destination {{container_ip}}:{{private_port}}">> ),
                P(<<"COMMIT">>),
                P(<<"">>)
               ],

    IptablesFilter = [ 
                P(<<":DOCKER - [0:0]
:DOCKER-ISOLATION-STAGE-1 - [0:0]
:DOCKER-ISOLATION-STAGE-2 - [0:0]
:DOCKER-USER - [0:0]">>),
                %Reversed order of FORWARD rules to insert before 'reject-with' rule from dog_trainer
%                D(<<"-I FORWARD 1 -i {{bridge_interface}} -o {{bridge_interface}} -j ACCEPT
%-I FORWARD 1 -i {{bridge_interface}} ! -o {{bridge_interface}} -j ACCEPT
%-I FORWARD 1 -o {{bridge_interface}} -j DOCKER
%-I FORWARD 1 -o {{bridge_interface}} -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT">> ),
                N(<<"-I FORWARD 1 -i {{bridge_interface}} -o {{bridge_interface}} -j ACCEPT
-I FORWARD 1 -i {{bridge_interface}} ! -o {{bridge_interface}} -j ACCEPT
-I FORWARD 1 -o {{bridge_interface}} -j DOCKER
-I FORWARD 1 -o {{bridge_interface}} -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT">> ),
                P(<<"-I FORWARD 1 -j DOCKER-ISOLATION-STAGE-1
-I FORWARD 1 -j DOCKER-USER">>),
                %Adding REJECT rule will result in duplicate rules.
                %P(<<"-A FORWARD -j REJECT --reject-with icmp-port-unreachable">>),
                C(<<"-A DOCKER -d {{container_ip}}/32 ! -i {{bridge_interface}} -o {{bridge_interface}} -p tcp -m tcp --dport {{private_port}} -j ACCEPT">> ),
                D(<<"-A DOCKER-ISOLATION-STAGE-1 -i {{bridge_interface}} ! -o {{bridge_interface}} -j DOCKER-ISOLATION-STAGE-2">> ),
                N(<<"-A DOCKER-ISOLATION-STAGE-1 -i {{bridge_interface}} ! -o {{bridge_interface}} -j DOCKER-ISOLATION-STAGE-2">> ),
                P(<<"-A DOCKER-ISOLATION-STAGE-1 -j RETURN">>),
                D(<<"-A DOCKER-ISOLATION-STAGE-2 -o {{bridge_interface}} -j DROP">> ),
                N(<<"-A DOCKER-ISOLATION-STAGE-2 -o {{bridge_interface}} -j DROP">> ),
                P(<<"-A DOCKER-ISOLATION-STAGE-2 -j RETURN">>),
                P(<<"-A DOCKER-USER -j RETURN">>),
                P(<<"COMMIT">>),
                P(<<"">>)
               ],
  IptablesNatString = string:join([L || L <- IptablesNat, L =/= []],"\n"),
  IptablesFilterString = string:join([L || L <- IptablesFilter, L =/= []],"\n"),
  {IptablesNatString,IptablesFilterString}.
%  end.

%docker_running_no_containers() -> 
%"# Generated by iptables-save v1.6.0 on Thu Mar 11 22:45:58 2021
%*nat
%:PREROUTING ACCEPT [4:800]
%:INPUT ACCEPT [0:0]
%:OUTPUT ACCEPT [0:0]
%:POSTROUTING ACCEPT [0:0]
%:DOCKER - [0:0]
%-A PREROUTING -m addrtype --dst-type LOCAL -j DOCKER
%-A OUTPUT ! -d 127.0.0.0/8 -m addrtype --dst-type LOCAL -j DOCKER
%-A POSTROUTING -s 172.17.0.0/16 ! -o docker0 -j MASQUERADE
%-A DOCKER -i docker0 -j RETURN
%COMMIT
%# Completed on Thu Mar 11 22:45:58 2021
%# Generated by iptables-save v1.6.0 on Thu Mar 11 22:45:58 2021
%*filter
%:INPUT DROP [0:0]
%:FORWARD DROP [0:0]
%:OUTPUT ACCEPT [0:0]
%:DOCKER - [0:0]
%:DOCKER-ISOLATION-STAGE-1 - [0:0]
%:DOCKER-ISOLATION-STAGE-2 - [0:0]
%:DOCKER-USER - [0:0]
%-A FORWARD -j DOCKER-USER
%-A FORWARD -j DOCKER-ISOLATION-STAGE-1
%-A FORWARD -o docker0 -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
%-A FORWARD -o docker0 -j DOCKER
%-A FORWARD -i docker0 ! -o docker0 -j ACCEPT
%-A FORWARD -i docker0 -o docker0 -j ACCEPT
%-A DOCKER-ISOLATION-STAGE-1 -i docker0 ! -o docker0 -j DOCKER-ISOLATION-STAGE-2
%-A DOCKER-ISOLATION-STAGE-1 -j RETURN
%-A DOCKER-ISOLATION-STAGE-2 -o docker0 -j DROP
%-A DOCKER-ISOLATION-STAGE-2 -j RETURN
%-A DOCKER-USER -j RETURN
%COMMIT
%# Completed on Thu Mar 11 22:45:58 2021".
%
%docker_2_containers_one_network() ->
%  "# Generated by iptables-save v1.6.0 on Thu Mar 11 23:05:25 2021
%*nat
%:PREROUTING ACCEPT [2:400]
%:INPUT ACCEPT [0:0]
%:OUTPUT ACCEPT [0:0]
%:POSTROUTING ACCEPT [0:0]
%:DOCKER - [0:0]
%-A PREROUTING -m addrtype --dst-type LOCAL -j DOCKER
%-A OUTPUT ! -d 127.0.0.0/8 -m addrtype --dst-type LOCAL -j DOCKER
%-A POSTROUTING -s 172.17.0.0/16 ! -o docker0 -j MASQUERADE
%-A POSTROUTING -s 172.17.0.2/32 -d 172.17.0.2/32 -p tcp -m tcp --dport 90 -j MASQUERADE
%-A DOCKER -i docker0 -j RETURN
%-A DOCKER ! -i docker0 -p tcp -m tcp --dport 8090 -j DNAT --to-destination 172.17.0.2:90
%COMMIT
%# Completed on Thu Mar 11 23:05:25 2021
%# Generated by iptables-save v1.6.0 on Thu Mar 11 23:05:25 2021
%*filter
%:INPUT DROP [0:0]
%:FORWARD DROP [0:0]
%:OUTPUT ACCEPT [0:0]
%:DOCKER - [0:0]
%:DOCKER-ISOLATION-STAGE-1 - [0:0]
%:DOCKER-ISOLATION-STAGE-2 - [0:0]
%:DOCKER-USER - [0:0]
%-A FORWARD -j DOCKER-USER
%-A FORWARD -j DOCKER-ISOLATION-STAGE-1
%-A FORWARD -o docker0 -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
%-A FORWARD -o docker0 -j DOCKER
%-A FORWARD -i docker0 ! -o docker0 -j ACCEPT
%-A FORWARD -i docker0 -o docker0 -j ACCEPT
%-A DOCKER -d 172.17.0.2/32 ! -i docker0 -o docker0 -p tcp -m tcp --dport 90 -j ACCEPT
%-A DOCKER-ISOLATION-STAGE-1 -i docker0 ! -o docker0 -j DOCKER-ISOLATION-STAGE-2
%-A DOCKER-ISOLATION-STAGE-1 -j RETURN
%-A DOCKER-ISOLATION-STAGE-2 -o docker0 -j DROP
%-A DOCKER-ISOLATION-STAGE-2 -j RETURN
%-A DOCKER-USER -j RETURN
%COMMIT
%# Completed on Thu Mar 11 23:05:25 2021
%".

default_network() ->
  {ok, DefaultBridgeNetwork} = docker_container:network(<<"bridge">>),
  DefaultBridgeNetworkMap = jsn:new(DefaultBridgeNetwork),
  DefaultBridgeSubnet = maps:get(<<"Subnet">>,maps:from_list(lists:flatten(maps:get(<<"Config">>,maps:from_list(maps:get(<<"IPAM">>,DefaultBridgeNetworkMap)))))),
  DefaultBridgeName = maps:get(<<"com.docker.network.bridge.name">>,maps:from_list(maps:get(<<"Options">>,DefaultBridgeNetworkMap))),
  #{"container_network" => DefaultBridgeSubnet,
    "bridge_interface" => DefaultBridgeName}.

container_networks() ->
  {ok, Containers} = docker_container:containers(),
  ?LOG_DEBUG("Containers: ~p",[Containers]),
  ContainerNetworks = case Containers of
                        [] ->
                          [];
                        _ ->
                          lists:map(fun(Container) -> 
                                        ContainerMap = jsn:new(Container),
                                        NetworkMode = maps:get(<<"NetworkMode">>,maps:from_list(maps:get(<<"HostConfig">>,ContainerMap))),
                                        ContainerNetworks = maps:get(<<"Networks">>,maps:from_list(maps:get(<<"NetworkSettings">>,ContainerMap))),
                                        Nets = lists:map(fun(Network) ->
                                                             NetworkMap = jsn:new(Network),
                                                             Nmap = maps:from_list(hd(maps:values(NetworkMap))),
                                                             %io:format("NetworkMap: ~p~n",[Nmap]),
                                                             ContainerIp = maps:get(<<"IPAddress">>,Nmap),
                                                             ContainerNetmask= maps:get(<<"IPPrefixLen">>,Nmap),
                                                             NetworkId = binary:bin_to_list(maps:get(<<"NetworkID">>,Nmap)),
                                                             ShortNetworkId = binary:list_to_bin(string:slice(NetworkId,0,12)),
                                                             BridgePrefix = <<"br-">>,
                                                             BridgeInterface= <<BridgePrefix/binary,ShortNetworkId/binary>>,
                                                             {ok,ParsedContainerIp} = inet:parse_address(binary_to_list(ContainerIp)),
                                                             %io:format("ContainerIp: ~p~n",[ContainerIp]),
                                                             %io:format("ContainerNetmask: ~p~n",[ContainerNetmask]),
                                                             %io:format("ParsedContainerIp: ~p~n",[ParsedContainerIp]),
                                                             M = inet_utils:mask_address(ParsedContainerIp,ContainerNetmask),
                                                             ContainerNetwork = binary_to_list(inet_utils:inet_ntoa(M)) ++ "/" ++ integer_to_list(ContainerNetmask),
                                                             %print(f"{container_name} {container_ip}/{container_netmask} {bridge_interface}")
                                                             #{"bridge_interface" => BridgeInterface,
                                                               "container_ip" => ContainerIp,
                                                               "container_netmask" => ContainerNetmask,
                                                               "container_network" => ContainerNetwork}
                                                         end, ContainerNetworks),
                                        ContainerPorts = maps:get(<<"Ports">>,ContainerMap),
                                        %io:format("ContainerPorts: ~p~n",[ContainerPorts]),
                                        lists:map(fun(Port)-> 
                                                      PortMap = jsn:new(Port),
                                                      HostIP = maps:get(<<"IP">>,PortMap,[]),
                                                      PrivatePort = maps:get(<<"PrivatePort">>, PortMap),
                                                      Protocol = maps:get(<<"Type">>, PortMap),
                                                      PublicPort = maps:get(<<"PublicPort">>, PortMap,[]),
                                                      #{"network_mode" => NetworkMode, 
                                                        "host_ip" => HostIP, 
                                                        "public_port" => PublicPort, 
                                                        "private_port" => PrivatePort, 
                                                        "protocol" => Protocol, 
                                                        "nets" => Nets
                                                       }
                                                  end, ContainerPorts)
                                    end, Containers)
                      end,
  lists:reverse(lists:flatten(ContainerNetworks)).
%container_networks() ->
%  {ok, Containers} = docker_container:containers(),
%  ?LOG_DEBUG("Containers: ~p",[Containers]),
%  ContainerNetworks = case Containers of
%                        [] ->
%                          [];
%                        _ ->
%                          lists:map(fun(Container) -> 
%                                        ContainerMap = jsn:new(Container),
%                                        NetworkMode = maps:get(<<"NetworkMode">>,maps:from_list(maps:get(<<"HostConfig">>,ContainerMap))),
%                                        ContainerNetworks = maps:get(<<"Networks">>,maps:from_list(maps:get(<<"NetworkSettings">>,ContainerMap))),
%                                        Nets = container_nets(ContainerMap, NetworkMode, ContainerNetworks),
%                                        Nets
%                                    end, Containers)
%                      end,
%  lists:reverse(lists:flatten(ContainerNetworks)).

nets() ->
    {ok, Networks} = docker_container:networks(),
    Nets = lists:map(fun(Network) ->
                      NetworkMap = jsn:new(Network),
                      Name = maps:get(<<"Name">>, NetworkMap),
                      Driver = maps:get(<<"Driver">>, NetworkMap),
                      BridgeInterface = case Name of
                                 <<"bridge">> ->
                                     <<"docker0">>;
                                 _ ->
                                      NetworkId = binary:bin_to_list(maps:get(<<"Id">>,NetworkMap)),
                                      ShortNetworkId = binary:list_to_bin(string:slice(NetworkId,0,12)),
                                      BridgePrefix = <<"br-">>,
                                      <<BridgePrefix/binary,ShortNetworkId/binary>>
                             end,
                      Ipam = maps:from_list(maps:get(<<"IPAM">>, NetworkMap)),
                      Config = maps:get(<<"Config">>, Ipam),
                      case Config of
                          [] ->
                           Subnet = [],
                           Gateway = [];
                          _ ->
                           FirstConfig = maps:from_list(hd(Config)),
                           Subnet = maps:get(<<"Subnet">>,FirstConfig),
                           Gateway = maps:get(<<"Gateway">>,FirstConfig)
                      end,
                      #{"bridge_interface" => BridgeInterface,
                        "name" => Name,
                        "subnet" => Subnet,
                        "gateway" => Gateway,
                        "driver" => Driver}
              end, Networks),
    [Net || Net <- Nets, maps:get("subnet",Net) =/= []].

container_nets(ContainerMap, NetworkMode, ContainerNetworks) ->
    lists:map(fun(Network) ->
                      NetworkMap = jsn:new(Network),
                      Nmap = maps:from_list(hd(maps:values(NetworkMap))),
                      ?LOG_DEBUG("NetworkMap: ~p~n",[Nmap]),
                      ContainerIp = maps:get(<<"IPAddress">>,Nmap),
                      ContainerNetmask= maps:get(<<"IPPrefixLen">>,Nmap),
                      NetworkId = binary:bin_to_list(maps:get(<<"NetworkID">>,Nmap)),
                      ShortNetworkId = binary:list_to_bin(string:slice(NetworkId,0,12)),
                      BridgePrefix = <<"br-">>,
                      BridgeInterface= <<BridgePrefix/binary,ShortNetworkId/binary>>,
                      {ok,ParsedContainerIp} = inet:parse_address(binary_to_list(ContainerIp)),
                      ?LOG_DEBUG("ContainerIp: ~p~n",[ContainerIp]),
                      ?LOG_DEBUG("ContainerNetmask: ~p~n",[ContainerNetmask]),
                      ?LOG_DEBUG("ParsedContainerIp: ~p~n",[ParsedContainerIp]),
                      M = inet_utils:mask_address(ParsedContainerIp,ContainerNetmask),
                      ContainerNetwork = binary_to_list(inet_utils:inet_ntoa(M)) ++ "/" ++ integer_to_list(ContainerNetmask),
                      %print(f"{container_name} {container_ip}/{container_netmask} {bridge_interface}")
                      ContainerPorts = maps:get(<<"Ports">>,ContainerMap),
                      ContainerPortNetworks = container_port_networks(ContainerPorts),
                      %?LOG_DEBUG("ContainerPortNetworks: ~p~n",[ContainerPortNetworks]),
                      #{"bridge_interface" => BridgeInterface,
                        "container_ip" => ContainerIp,
                        "container_netmask" => ContainerNetmask,
                        "container_network" => ContainerNetwork,
                        "network_mode" => NetworkMode,
                        "ports" => ContainerPortNetworks}
              end, ContainerNetworks).

container_port_networks(ContainerPorts) ->
    lists:map(fun(Port)-> 
                      PortMap = jsn:new(Port),
                      HostIP = maps:get(<<"IP">>,PortMap,[]),
                      PrivatePort = maps:get(<<"PrivatePort">>, PortMap),
                      Protocol = maps:get(<<"Type">>, PortMap),
                      PublicPort = maps:get(<<"PublicPort">>, PortMap,[]),
                      #{ 
                        "host_ip" => HostIP, 
                        "public_port" => PublicPort, 
                        "private_port" => PrivatePort, 
                        "protocol" => Protocol
                       }
              end, ContainerPorts).

get_interfaces(ContainerNetworks,DefaultNetwork) ->
  BridgeInterfaces = lists:map(fun(Container) ->
                              maps:get("bridge_interface",Container)
            end, ContainerNetworks),
  DefaultBridgeInterface = maps:get("bridge_interface",DefaultNetwork),DefaultNetwork,
  lists:flatten([BridgeInterfaces,DefaultBridgeInterface]).

per_container(ContainerNetworks,DefaultNetwork,Template) ->
  string:join(lists:map(fun(Container) ->
                            ?LOG_DEBUG("Container: ~p",[Container]),
                            Nets = maps:get("nets",Container),
                            NetworkMode = maps:get("network_mode",Container),
                            lists:map(fun(Net) ->
                                          Net2 = case NetworkMode of
                                                   <<"default">> ->
                                                     maps:merge(maps:merge(Net,DefaultNetwork),Container);
                                                   _ ->
                                                     maps:merge(maps:merge(DefaultNetwork,Net),Container)
                                                 end,
                                          ?LOG_DEBUG("Net2: ~p~n",[Net2]),
                                          bbmustache:render(
                                            Template
                                            ,Net2)
                                      end, Nets)
                        end, ContainerNetworks),"\n").

per_public_container(ContainerNetworks,DefaultNetwork,Template) ->
  PublicContainerNetworks = [Net || Net <- ContainerNetworks, maps:get("public_port",Net) =/= []],                        
  Nets = lists:map(fun(Container) ->
                            ?LOG_DEBUG("Container: ~p",[Container]),
                            Nets = maps:get("nets",Container),
                            NetworkMode = maps:get("network_mode",Container),
                            lists:map(fun(Net) ->
                                          Net2 = case NetworkMode of
                                                   <<"default">> ->
                                                     maps:merge(maps:merge(Net,DefaultNetwork),Container);
                                                   _ ->
                                                     maps:merge(maps:merge(DefaultNetwork,Net),Container)
                                                 end,
                                          ?LOG_DEBUG("Net2: ~p~n",[Net2]),
                                          bbmustache:render(
                                            Template
                                            ,Net2)
                                      end, Nets)
                        end, PublicContainerNetworks),
    string:join(Nets,"\n").

per_network(Nets,Template) ->
 %?LOG_DEBUG("ContainerNetworks: ~p",[ContainerNetworks]),
  %Interfaces = get_interfaces(ContainerNetworks,DefaultNetwork),
  %?LOG_DEBUG("ContainerNets: ~p~n",[ContainerNets]),
  string:join(lists:map(fun(Net) ->
          ?LOG_DEBUG("Net: ~p~n",[Net]),
          R = binary_to_list(bbmustache:render(
            Template
            ,Net)),
          R
            end, Nets),"\n").

for_default_network(DefaultNetwork,Template) ->
  R = binary_to_list(bbmustache:render(
    Template
    ,DefaultNetwork)),
  R.
