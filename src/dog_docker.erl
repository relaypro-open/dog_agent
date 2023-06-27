-module(dog_docker).

-include("dog.hrl").

-export([
         any_docker_containers/0,
         container_networks/0,
         default_network/0,
         for_default_network/2,
         get_interfaces/2,
         is_docker_instance/0,
         iptables/0,
         per_container/3,
         per_network/3
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

-spec iptables() -> {IptablesNat :: iolist(), IptablesFilter :: iolist()}.
iptables() ->
  DefaultNetwork = default_network(),
  ContainerNetworks = container_networks(),
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
    N = fun(Template) ->
             per_network(ContainerNetworks,DefaultNetwork,Template)
         end,
    P = fun(String) ->
             io_lib:format("~s",[String])
         end,
    %io:format("~p~n",[DefaultNetwork]),
    %io:format("~p~n~p~n",[ContainerNetworks,DefaultNetwork]),
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
                N(<<"-A POSTROUTING -s {{container_network}}/{{container_netmask}} ! -o {{bridge_interface}} -j MASQUERADE">>),
                D(<<"-A DOCKER -i {{bridge_interface}} -j RETURN">>),
                C(<<"-A DOCKER -i {{bridge_interface}} -j RETURN">>),
                C(<<"-A DOCKER ! -i {{bridge_interface}} -p {{protocol}} -m {{protocol}} --dport {{public_port}} -j DNAT --to-destination {{container_ip}}:{{private_port}}">> ),
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
                                                      case PublicPort of
                                                        [] ->
                                                          [];
                                                        _ ->
                                                          #{"network_mode" => NetworkMode, 
                                                            "host_ip" => HostIP, 
                                                            "public_port" => PublicPort, 
                                                            "private_port" => PrivatePort, 
                                                            "protocol" => Protocol, 
                                                            "nets" => Nets
                                                           }
                                                      end
                                                  end, ContainerPorts)
                                    end, Containers)
                      end,
  lists:reverse(lists:flatten(ContainerNetworks)).

get_interfaces(ContainerNetworks,DefaultNetwork) ->
  BridgeInterfaces = lists:map(fun(Container) ->
                Nets = maps:get("nets",Container),
                lists:map(fun(Net) ->
                              {maps:get("bridge_interface",Net),Net}
                          end, Nets)
            end, ContainerNetworks),
  DefaultBridgeInterface = {maps:get("bridge_interface",DefaultNetwork),DefaultNetwork},
  maps:values(maps:from_list(lists:flatten([BridgeInterfaces,DefaultBridgeInterface]))).

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
                                          %io:format("Net2: ~p~n",[Net2]),
                                          bbmustache:render(
                                            Template
                                            ,Net2)
                                      end, Nets)
                        end, ContainerNetworks),"\n").


per_network(ContainerNetworks,DefaultNetwork,Template) ->
 ?LOG_DEBUG("ContainerNetworks: ~p",[ContainerNetworks]),
  Interfaces = get_interfaces(ContainerNetworks,DefaultNetwork),
  %io:format("ContainerNets: ~p~n",[ContainerNets]),
  string:join(lists:map(fun(Net) ->
      %io:format("Net: ~p~n",[Net]),
      R = binary_to_list(bbmustache:render(
        Template
        ,Net)),
      R
            end, Interfaces),"\n").

for_default_network(DefaultNetwork,Template) ->
  R = binary_to_list(bbmustache:render(
    Template
    ,DefaultNetwork)),
  R.
