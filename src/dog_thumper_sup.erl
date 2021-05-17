-module(dog_thumper_sup).

-export([amqp_op/3, ensure_consumer/2,
     ensure_consumer/5, init/1, start_link/0]).

-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%-spec ensure_consumer(Action :: atom(), Name :: atom()) ->
%   {error, {already_up, pid()}} |
%   {ok, Child :: term()} |
%   {ok, Child :: term(), Info :: term()} |
%   {error, atom()}.
ensure_consumer(up, Name, Broker, Queue, Callback) ->
    case get_pid(Name) of
      {ok, Pid} -> {error, {already_up, Pid}};
      {error, terminated} ->
      supervisor:restart_child(?MODULE, Name);
      {error, deleted} ->
      supervisor:start_child(?MODULE,
                 spec(Name, Broker, Queue, Callback))
    end.

%-spec ensure_consumer(Action :: down, Name :: atom()) ->
%    {error, already_down} |
%    {error, already_down} |
%    ok |
%    {error, not_found} |
%    {error, simple_one_for_one}.
ensure_consumer(down, Name) ->
    case get_pid(Name) of
      {ok, _Pid} -> supervisor:terminate_child(?MODULE, Name);
      {error, terminated} -> {error, already_down};
      {error, deleted} -> {error, already_down}
    end.

-spec amqp_op(Broker :: any(), Consumer :: any(),
          Ops :: any()) -> ok | {ok, atom()} | {error, atom()}.

amqp_op(Broker, Consumer, Ops) ->
    case whereis(Consumer) of
      undefined ->
        Config = broker_connection:rabbitmq_config(Broker),
        Tx = [{rabbitmq_config, Config}, {tx, Ops}],
        thumper_tx:run(Tx);
      _Result ->
        State = case thumper_consumer:get_state(Consumer, 2000) of
          {error, Reason} ->
              {error, Reason};
          GoodValue ->
              GoodValue
        end,
        case proplists:get_value(channel, State) of
          {error, _} -> {error, no_channel};
          undefined -> {error, no_channel};
          Channel -> thumper_tx:run([{tx, Ops}], Channel)
        end
    end.

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10,
         period => 60},
    {ok, {SupFlags, []}}.

-spec spec(Name :: atom(), Broker :: binary(),
       Queue :: binary(), Callback :: atom()) -> map().

spec(Name, Broker, Queue, Callback) ->
    #{id => Name,
      start =>
      {thumper_consumer, start_link,
       [Name, Broker, Queue, Callback]}}.

-spec get_pid(Name :: atom()) -> {ok, pid()} |
                 {error, terminated} | {error, deleted}.

get_pid(Name) ->
    case whereis(Name) of
      Pid when is_pid(Pid) -> {ok, Pid};
      _ ->
      Children = supervisor:which_children(?MODULE),
      case lists:keyfind(Name, 1, Children) of
        {Name, Pid, _, _} when is_pid(Pid) -> {ok, Pid};
        {Name, _, _, _} -> {error, terminated};
        false -> {error, deleted}
      end
    end.
