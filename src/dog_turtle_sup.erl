-module(dog_turtle_sup).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([
         config_service_spec/1,
		 get_pid/1,
         init/1,
         %ipset_service_spec/0,
         iptables_service_spec/4,
         start_link/0 
       ]).

-behaviour(supervisor).

-define(SERVER, ?MODULE).

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
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
      name => config_service,
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
