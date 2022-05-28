-module(dog_turtle_sup).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([
		 get_pid/1,
         %child_specs/0,
         config_service_spec/1,
         file_transfer_service_spec/4,
         init/1,
         ips_publisher_spec/0,
         iptables_service_spec/4,
         restart_ips_agent/0,
         restart_mq_services/4,
         start_config_service/1,
         start_file_transfer_service/4,
         start_ips_publisher/0,
         start_link/0,
         stop_config_service/0,
         stop_file_transfer_service/0,
         stop_ips_agent/0,
         stop_ips_publisher/0
       ]).

-behaviour(supervisor).

-define(SERVER, ?MODULE).

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    child_specs().

child_specs() ->
    ChildSpecs = [
         ips_publisher_spec()
                 ],
    {ok, { {one_for_all, 10, 60}, ChildSpecs} }.

ips_publisher_spec() ->
    PublisherName = ips_publisher,
    ConnName = default, 
    AMQPDecls = [
     #'queue.declare' {queue = <<"ips">>, auto_delete = false, durable = true},
     #'queue.bind' {queue = <<"ips">>, exchange = <<"ips">>, routing_key = <<"#">>}
    ],
    AMQPPoolChildSpec =
        turtle_publisher:child_spec(PublisherName, ConnName, AMQPDecls,
            #{ confirms => true, passive => false, rpc => false }),
	AMQPPoolChildSpec.

%ipset_service_spec() ->
%    Config = #{
%      name => ipset_service,
%      connection => default,
%      function => fun dog_config_agent:loop/4,
%      handle_info => fun dog_config_agent:handle_info/2,
%      init_state => #{ },
%      declarations =>
%		  [
%		], 
%      subscriber_count => 1,
%      prefetch_count => 1,
%      passive => false
%    },
%    ServiceSpec = turtle_service:child_spec(Config),
%        ServiceSpec.

iptables_service_spec(Environment, Location, Group, Hostkey) ->
    QueueName = erlang:iolist_to_binary([<<"iptables.">>, Hostkey]),
    GroupRoutingKey = erlang:iolist_to_binary([Environment,".",Location,".",Group,".*"]),
    HostRoutingKey = erlang:iolist_to_binary([Environment,".",Location,".*.",Hostkey]),
    Config = #{
      name => iptables_service,
      connection => default,
      function => fun dog_iptables:subscriber_loop/4,
      handle_info => fun dog_iptables_agent:handle_info/2,
      init_state => #{ },
      declarations =>
		  [
          #'queue.declare' {queue = QueueName , auto_delete = true, durable = true},
          #'queue.bind' {queue = QueueName, exchange = <<"ipsets">>, routing_key = <<"fanout">> },
          #'queue.bind' {queue = QueueName, exchange = <<"iptables">>, routing_key = HostRoutingKey },
          #'queue.bind' {queue = QueueName, exchange = <<"iptables">>, routing_key = GroupRoutingKey }
		], 
      subscriber_count => 1,
      prefetch_count => 1,
      consume_queue => QueueName,
      passive => false
    },
    ServiceSpec = turtle_service:child_spec(Config),
        ServiceSpec.

config_service_spec(Hostkey) ->
    QueueName = erlang:iolist_to_binary([<<"config.">>, Hostkey]),
    Config = #{
      name => dog_config_service,
      connection => default,
      function => fun dog_config:subscriber_loop/4,
      handle_info => fun dog_config_agent:handle_info/2,
      init_state => #{ },
      declarations =>
		  [
          #'queue.declare' {queue = QueueName , auto_delete = true, durable = true},
          #'queue.bind' {queue = QueueName, exchange = <<"config">>, routing_key = Hostkey }
		], 
      subscriber_count => 1,
      prefetch_count => 1,
      consume_queue => QueueName,
      passive => false
    },

    ServiceSpec = turtle_service:child_spec(Config),
        ServiceSpec.

file_transfer_service_spec(Environment, Location, Group, Hostkey) ->
    GroupRoutingKey = erlang:iolist_to_binary([Environment,".",Location,".",Group,".*"]),
    HostRoutingKey = erlang:iolist_to_binary([Environment,".",Location,".*.",Hostkey]),
    QueueName = erlang:iolist_to_binary([<<"file_transfer.">>, Hostkey]),
    Config = #{
      name => file_transfer_service,
      connection => default,
      function => fun dog_file_transfer:subscriber_loop/4,
      handle_info => fun dog_file_transfer:handle_info/2,
      init_state => #{ },
      declarations =>
		  [
            #'queue.declare' {queue = QueueName, auto_delete = true, durable = true},
            #'queue.bind' {queue = QueueName, exchange = <<"file_transfer">>, routing_key = GroupRoutingKey},
            #'queue.bind' {queue = QueueName, exchange = <<"file_transfer">>, routing_key = HostRoutingKey}
		], 
      subscriber_count => 1,
      prefetch_count => 1,
      consume_queue => QueueName,
      passive => false
    },

    ServiceSpec = turtle_service:child_spec(Config),
        ServiceSpec.

-spec get_pid(atom()) -> {'error','deleted' | 'terminated'} | {'ok',pid()}.
get_pid(Name) ->                                
    case whereis(Name) of                       
        Pid when is_pid(Pid) ->                 
            {ok, Pid};                          
        _ ->                                    
            Children = supervisor:which_children(?MODULE),
            case lists:keyfind(Name, 1, Children) of
                {_N, Pid, _, _} when is_pid(Pid) ->
                    {ok, Pid};                  
                {_N, _, _, _} ->              
                    {error, terminated};        
                false ->                        
                    {error, deleted}            
            end                                 
    end.   

restart_mq_services(Environment, Location, Group, Hostkey) ->
    stop_config_service(),
    start_config_service(Hostkey),
    stop_iptables_service(),
    start_iptables_service(Environment, Location, Group, Hostkey),
    stop_file_transfer_service(),
    start_file_transfer_service(Environment, Location, Group, Hostkey),
    stop_ips_publisher(),
    start_ips_publisher(),
    stop_ips_agent(),
    restart_ips_agent().

start_config_service(Hostkey) ->
    lager:debug("Hostkey: ~p",[Hostkey]),
    turtle_service:new(dog_turtle_sup,config_service_spec(Hostkey)).

stop_config_service() ->
    stop(dog_turtle_sup,dog_config_service).

start_iptables_service(Environment,Location,Group,Hostkey) ->
    turtle_service:new(dog_turtle_sup,iptables_service_spec(Environment,Location,Group,Hostkey)).

stop_iptables_service() ->
    stop(dog_turtle_sup,iptables_service).

start_file_transfer_service(Environment, Location, Group, Hostkey) ->
    turtle_service:new(dog_turtle_sup,
                       file_transfer_service_spec(Environment, Location, Group, Hostkey)).

stop_file_transfer_service() ->
    stop(dog_turtle_sup,file_transfer_service).

start_ips_publisher() ->
    turtle_publisher:new(dog_turtle_sup,ips_publisher_spec()).

stop_ips_publisher() ->
    stop(dog_turtle_sup,ips_publisher).

restart_ips_agent() ->
    supervisor:restart_child(dog_sup, dog_ips_agent),
    ok.

stop_ips_agent() ->
    supervisor:terminate_child(dog_sup, dog_ips_agent),
    ok.

stop(Supervisor,Name) ->
    supervisor:terminate_child(Supervisor,Name),
    supervisor:delete_child(Supervisor,Name).
