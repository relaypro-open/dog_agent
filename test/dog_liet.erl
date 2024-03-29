-module(dog_liet).
-compile({parse_transform, liet_resource_graph}).

-include_lib("kernel/include/inet.hrl").
-include_lib("eunit/include/eunit.hrl").

%% APPS
dog_envload() ->
    application:load(dog).

dog_envload(destroy) ->
    application:unload(dog).

dog_version() ->
    Version = "0.0",
    _ = dog_envload(),
    application:set_env(dog, version, Version),
    Version.

dog_version(destroy) ->
    application:unset_env(dog, version).

dog_envconfig() ->
    _ = dog_envload(),
    application:set_env(dog, environment, binary_to_list(environment())),
    application:set_env(dog, location, binary_to_list(location())),
    application:set_env(dog, hostkey, binary_to_list(hostkey())),
    application:set_env(dog, group, binary_to_list(group())).

dog_envconfig(destroy) ->
    application:unset_env(dog, environment),
    application:unset_env(dog, location),
    application:unset_env(dog, hostkey),
    application:unset_env(dog, group).

dog_app() ->
    _ = [
         dog_os(),
         timer_nosleep(),
         file_write_nothing(),
         file_read_config_map(),
         inet_ifs(),
         dog_turtle_allow(),         
         dog_turtle_service_allow(),
         turtle_publish(),
         hackney_deny(),
         lager_none(),                
         dog_version(),
         turtle_noconnect()
        ],                             

    {ok, Apps} = application:ensure_all_started(dog),
    %Apps = [sasl,syntax_tools,compiler,goldrush,lager,unicode_util_compat,idna,mimerl,
    %  certifi,parse_trans,ssl_verify_fun,metrics,hackney,jsx,base16,quickrand,uuid,
    %   fn,erldocker,bbmustache,jsn,xmerl,ranch,recon,rabbit_common,amqp_client,hut,
    %    setup,bear,folsom,exometer_core,gproc,turtle,dog],
    %lists:foreach(fun(App) ->
    %                      ?debugFmt("start App: ~p~n",[App]),
    %                      application:start(App)
    %              end, Apps),
    meck:reset(dog_os),
    %?debugFmt("Apps: ~p~n",[Apps]),
    Apps.

dog_app(destroy) ->
    %Apps = [sasl,syntax_tools,compiler,goldrush,lager,unicode_util_compat,idna,mimerl,
    %  certifi,parse_trans,ssl_verify_fun,metrics,hackney,jsx,base16,quickrand,uuid,
    %   fn,erldocker,bbmustache,jsn,xmerl,ranch,recon,rabbit_common,amqp_client,hut,
    %    setup,bear,folsom,exometer_core,gproc,turtle,dog],
    %lists:foreach(fun(App) ->
    %                      ?debugFmt("stop App: ~p~n",[App]),
    %                      application:stop(App)
    %              end, lists:reverse(Apps)).
    [ application:stop(X) || X <- lists:reverse(dog_app()) ].

dog_ec2_app() ->
    _ = [
         dog_os(),
         timer_nosleep(),
         file_write_nothing(),
         file_read_config_map(),
         inet_ifs(),
         dog_turtle_allow(),
         dog_turtle_service_allow(),
         turtle_publish(),
         hackney_ec2(),
         lager_none(),
         dog_version(),
         turtle_noconnect()
        ],

    {ok, Apps} = application:ensure_all_started(dog),
    meck:reset(dog_os),
    Apps.

dog_ec2_app(destroy) ->
    [ application:stop(X) || X <- lists:reverse(dog_ec2_app()) ].

lager_none() ->
    meck:new(lager,[passthrough]),
    meck:expect(lager, log, fun(_,_,_) -> ok end),
    meck:expect(lager, log, fun(_,_,_,_) -> ok end),
    meck:expect(lager, log, fun(_,_,_,_,_) -> ok end),
    meck:expect(lager, pr_stacktrace, fun(_,_) -> ok end),
    application:set_env(lager, handlers, [{lager_console_backend, [{level, none}]}]).

lager_none(destroy) ->
    application:unset_env(lager, handlers),
    meck:unload(lager).

lager_app() ->
    _ = lager_none(),
    application:set_env(lager, handlers, [{lager_console_backend, [{level, debug}]}]),
    {ok, Apps} = application:ensure_all_started(lager),
    Apps.

lager_app(destroy) ->
    [ application:stop(X) || X <- lager_app() ].

turtle_noconnect() ->
    application:load(turtle),
    application:set_env(turtle, connection_config, []).

turtle_noconnect(destroy) ->
    application:unset_env(turtle, connection_config),
    application:unload(turtle).

dog_ips_agent_create_ipsets() ->
    meck:new(dog_ips_agent, [passthrough]),
    meck:expect(dog_ips_agent, create_ipsets, fun(X) -> dog_ipset:create_ipsets(X) end).

dog_ips_agent_create_ipsets(destroy) ->
    meck:unload(dog_ips_agent).

%% MOCKS
dog_turtle_allow() ->
    meck:new(dog_turtle_sup, [passthrough]),
    meck:expect(dog_turtle_sup, file_transfer_service_spec, fun(_Environment, _Location, _Group, _Hostkey) ->
      {file_transfer_service,                                                                
		{turtle_service,start_link,                                                    
			[#{connection => default,                                                       
			   consume_queue =>                                                              
				   <<102,105,108,101,95,116,114,97,110,115,102,101,114,46,4>>,               
			   declarations =>                                                               
				   [{'queue.declare',0,                                                      
						<<102,105,108,101,95,116,114,97,110,115,102,101,114,46,4>>,          
						false,true,false,true,false,[]},                                     
					{'queue.bind',0,                                                         
						<<102,105,108,101,95,116,114,97,110,115,102,101,114,46,4>>,          
						<<"file_transfer">>,                                                 
						<<1,46,2,46,3,46,42>>,                                               
						false,[]},                                                           
					{'queue.bind',0,                                                         
						<<102,105,108,101,95,116,114,97,110,115,102,101,114,46,4>>,          
						<<"file_transfer">>,                                                 
						<<1,46,2,46,42,46,4>>,                                               
						false,[]}],                                                          
			   function => fun dog_file_transfer:subscriber_loop/4,                          
			   handle_info => fun dog_file_transfer:handle_info/2,                           
			   init_state => #{},name => file_transfer_service,                              
			   passive => false,prefetch_count => 1,                                         
			   subscriber_count => 1}]},                                                     
		permanent,infinity,supervisor,                                                       
		[turtle_service]} 
	end),
    meck:expect(dog_turtle_sup, config_service_spec, fun(_Hostkey) -> 
		{config_service,                                                         
			{turtle_service,start_link,                                          
				[#{connection => default,consume_queue => <<"config.hostkey">>,  
				   declarations =>                                               
					   [{'queue.declare',0,<<"config.hostkey">>,false,true,false,
							true,false,[]},                                      
						{'queue.bind',0,<<"config.hostkey">>,<<"config">>,       
							<<"hostkey">>,false,[]}],                            
				   function => fun dog_config:subscriber_loop/4,                 
				   handle_info => fun dog_config_agent:handle_info/2,            
				   init_state => #{},name => config_service,passive => false,    
				   prefetch_count => 1,subscriber_count => 1}]},                 
			permanent,infinity,supervisor,                                       
			[turtle_service]}                                                    
	 end),
    meck:expect(dog_turtle_sup, iptables_service_spec, fun(_Environment, _Location, _Group, _Hostkey) ->
     {iptables_service,                                                         
		{turtle_service,start_link,                                            
			[#{connection => default,                                          
			   consume_queue => <<"iptables.hostkey">>,                        
			   declarations =>                                                 
				   [{'queue.declare',0,<<"iptables.hostkey">>,false,true,false,
						true,false,[]},                                        
					{'queue.bind',0,<<"iptables.hostkey">>,<<"ipsets">>,       
						<<"fanout">>,false,[]},                                
					{'queue.bind',0,<<"iptables.hostkey">>,<<"iptables">>,     
						<<"environment.location.*.hostkey">>,false,[]},        
					{'queue.bind',0,<<"iptables.hostkey">>,<<"iptables">>,     
						<<"environment.location.group.*">>,false,[]}],         
			   function => fun dog_iptables:subscriber_loop/4,                 
			   handle_info => fun dog_iptables_agent:handle_info/2,            
			   init_state => #{},name => iptables_service,passive => false,    
			   prefetch_count => 1,subscriber_count => 1}]},                   
		permanent,infinity,supervisor,                                         
		[turtle_service]}                                                      
 
	 end),
    meck:expect(dog_turtle_sup, ips_publisher_spec, fun() -> 
      {ips_publisher,                                                      
		{turtle_publisher,start_link,                                    
			[ips_publisher,default,                                      
			 [{'queue.declare',0,<<"ips">>,false,true,false,false,false, 
				  []},                                                   
			  {'queue.bind',0,<<"ips">>,<<"ips">>,<<"#">>,false,[]}],    
			 #{confirms => true,passive => false,rpc => false}]},        
		permanent,5000,worker,                                           
		[turtle_publisher]}                                              
    end),
    meck:expect(dog_turtle_sup, stop_ips_agent, fun() -> ok end),
    meck:expect(dog_turtle_sup, restart_ips_agent, fun() -> ok end),
    meck:expect(dog_turtle_sup, restart_mq_services, fun(_,_,_,_) -> ok end),
    meck:new(turtle_publisher, [passthrough]),
    meck:expect(turtle_publisher, child_spec, fun(_Environment, _Location, _Group, _Hostkey) -> {} end).

dog_turtle_allow(destroy) ->
    meck:unload(dog_turtle_sup).

dog_turtle_service_allow() ->
    meck:new(turtle_service, [passthrough]),
    meck:expect(turtle_service, new, fun(_Dog_turtle_sup,_Turtle_config_service_spec_Hostkey) -> ok end), 
    meck:expect(turtle_service, stop, fun(_Dog_turtle_sup,_Turtle_config_service) -> ok end),
    meck:expect(turtle_service, child_spec, fun(_Config) -> {} end).

dog_turtle_service_allow(destroy) ->
    meck:unload(turtle_service).

turtle_publish() ->
    meck:new(turtle, [passthrough]),
    meck:expect(turtle, publish, fun(_Publisher,_Exchange,_RoutingKey,_FileType,_Message,_Metadata) -> ok end).

turtle_publish(destroy) ->
    meck:unload(turtle).

hackney() ->
    {ok, Apps} = application:ensure_all_started(hackney),
    meck:new(hackney, [passthrough]),
    Apps.

hackney(destroy) ->
    meck:unload(hackney),
    [ application:stop(X) || X <- lists:reverse(hackney()) ].

hackney_docker() ->
    _ = hackney(),
    meck:expect(hackney, get, fun(_) -> {ok, fake, data, here} end).

hackney_deny() ->
    _ = hackney(),
    meck:expect(hackney, request, fun(_, _, _, _, _) -> {error, meck} end).

hackney_ec2() ->
    _ = hackney(),
    Mac = mac(),
    PublicIp = public_ip(),
    application:unset_env(dog, is_ec2_instance),
    meck:expect(hackney, request,
                fun(get, "http://169.254.169.254/latest/meta-data/placement/region", _, _, _) ->
                        {ok, 200, [], region};
		   (get, "http://169.254.169.254/latest/meta-data/placement/availability-zone", _, _, _) ->
                        {ok, 200, [], availability_zone};
                   (get, "http://169.254.169.254/latest/meta-data/instance-id", _, _, _) ->
                        {ok, 200, [], instance_id};
                   (get, "http://169.254.169.254/latest/meta-data/", _, _, _) ->
                        {ok, 200, [], metadata};
                   (get, "http://169.254.169.254/latest/meta-data/network/interfaces/macs/", _, _, _) ->
                        {ok, 200, [], macs};
                   (get, "http://169.254.169.254/latest/meta-data/network/interfaces/macs/" ++ Path, _, _, _) ->
                        case re:split(Path, "/", [{return, list}]) of
                            [Mac, Key] ->
                                {ok, 200, [], list_to_atom(Key)}
                        end;
                   (_, _, _, _, _) ->
                        {error, meck}
                end),
    meck:expect(hackney, get, fun(<<"http+unix://%2Fvar%2Frun%2Fdocker.sock">>) -> {error, meck} end),
    meck:expect(hackney, body,
                fun(region) -> {ok, "region"};
		   (availability_zone) -> {ok, "availability-zone"};
                   (instance_id) -> {ok, "instance-id"};
                   (macs) -> {ok, Mac ++ "/"};
                   ('public-ipv4s') -> {ok, PublicIp};
                   ('security-group-ids') -> {ok, "security-group"};
                   ('owner-id') -> {ok, "owner-id"}
                end).

docker_container() ->
    % This data was contructed to fit the existing code paths in dog_docker.erl. It would be
    % helpful to confirm that the mock data here is consistent with the values returned by
    % docker_container.erl
    Network =[{'Network', [
                           {<<"IPAddress">>, <<"127.0.0.1">>},
                           {<<"IPPrefixLen">>, 0},
                           {<<"NetworkID">>, <<"lo">>}
                          ]}],
    Port = [{<<"IP">>, <<"127.0.0.1">>},
            {<<"PrivatePort">>, 8080},
            {<<"PublicPort">>, 80},
            {<<"Type">>, <<"http">>}],
    Container = [{'Id',<<"6274938616c28e82b49b3ab120f9e9b9d96592d3b5bb95b53e91f740aca76243">>},
      {'Image',<<"base:latest">>},
      {'Command',<<"/bin/bash -c while true; do sleep 1; date; done">>},
      {'Created',1378278965},
      {'Status',<<"Up About an hour">>},
      {'Ports',[Port]},
      {'SizeRw',0},
      {'SizeRootFs',0},
      {'HostConfig', [{<<"NetworkMode">>, <<"networkmode">>}]},
      {'NetworkSettings', [{<<"Networks">>, [Network]}]}
                ],
    meck:new(docker_container, [passthrough]),
    meck:expect(docker_container, containers, fun() -> {ok, [Container]} end),

    BridgeNetwork = [{'IPAM', [{<<"Config">>, [{<<"Subnet">>, <<"mock-subnet">>}]}]},
                     {'Options', [{<<"com.docker.network.bridge.name">>, <<"mock-bridge-name">>}]}
    ],
    meck:expect(docker_container, network, fun(<<"bridge">>) -> {ok, BridgeNetwork} end).

docker_container(destroy) ->
    meck:unload(docker_container).

file() ->
    meck:new(file, [unstick, passthrough]).

file(destroy) ->
    meck:unload(file).

file_write_nothing() ->
    _ = file(),
    meck:expect(file, write_file, fun(_, _) -> ok end),
    meck:expect(file, open, fun(_, _) -> {ok, erlang:self()} end),
    meck:expect(file, write, fun(_, _) -> ok end),
    meck:expect(file, close, fun(_) -> ok end).

file_read_config_map() ->
    _ = file_write_nothing(), % meck cannot concurrently load the same module,
                              % so we force serialization of resources
    ConfigMap = config_map(),
    meck:expect(file, read_file,  fun(_) -> {ok, jsx:encode(ConfigMap)} end).%,

dog_os() ->
    meck:new(dog_os, [passthrough]),
    meck:expect(dog_os, cmd,
                fun(Cmd) ->
                        case re:run(Cmd, "ipset save$", [{capture, none}]) of
                            match ->
                                ["add A 127.0.0.1/32"];
                            nomatch ->
                                case re:run(Cmd, "ipset", [{capture, none}]) of
                                    match ->
                                        [];
                                    nomatch ->
                                        case re:run(Cmd, "tables", [{capture, none}]) of
                                            match ->
                                                [];
                                            nomatch ->
                                                meck:passthrough([Cmd])
                                        end
                                end
                        end
                end).

dog_os(destroy) ->
    meck:unload(dog_os).

timer_nosleep() ->
    meck:new(timer, [unstick, passthrough]),
    meck:expect(timer, sleep, fun(_) -> ok end).

timer_nosleep(destroy) ->
    meck:unload(timer).

inet_ifs() ->
    Hostname = hostname(),
    Interfaces = interfaces(),
    meck:new(inet, [unstick, passthrough]),
    meck:expect(inet, gethostname, fun() -> {ok, binary_to_list(Hostname)} end),
    meck:expect(inet, gethostbyname, fun(_) -> {ok, #hostent{h_name="localhost", h_addrtype=inet}} end),
    meck:expect(inet, getifaddrs, fun() -> {ok, Interfaces} end).

inet_ifs(destroy) ->
    meck:unload(inet).

%% DATA
config_map() ->
    #{<<"group">> => group(),
      <<"location">> => location(),
      <<"environment">> => environment(),
      <<"hostkey">> => hostkey()}.

group() -> <<"">>.
location() -> <<"">>.
environment() -> <<"">>.
hostkey() -> <<"">>.
hostname() -> <<"">>.
interfaces() -> [].

mac() -> "".
public_ip() -> "".
