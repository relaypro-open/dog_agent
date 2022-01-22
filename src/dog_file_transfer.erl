-module(dog_file_transfer).
-behaviour(gen_server).

-include("dog.hrl").
-include_lib("kernel/include/file.hrl").

-define(BLOCK_SIZE, 4096).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
         subscriber_loop/4,
         start_file_transfer_service/4,
         stop_file_transfer_service/0
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_file_transfer_service(Environment, Location, Group, Hostkey) ->
    turtle_service:new(dog_turtle_sup,
                       dog_turtle_sup:file_transfer_service_spec(Environment, Location, Group, Hostkey)).

stop_file_transfer_service() ->
    turtle_service:stop(dog_turtle_sup,dog_file_transfer_service).

subscriber_loop(_RoutingKey, _CType, Payload, State) -> 
    Message = binary_to_term(Payload),
    Filename = proplists:get_value(file_name, Message),
    Command = proplists:get_value(command, Message),
    UserData = proplists:get_value(user_data, Message),
    FilePath = "/tmp" ++ Filename,
    io:format("Command: ~p~n",[Command]),
    case Command of
        send_file ->
            FileTotalBlocks = proplists:get_value(total_blocks, Message),
            FileCurrentBlock = proplists:get_value(current_block, Message),
            FileBlock = maps:get(file_block, UserData),
            io:format("Filename: ~p, Block ~p of ~p~n",[Filename,FileCurrentBlock,FileTotalBlocks]),
            %{ok,IoDevice} = file:open(FilePath,[write,binary,read_ahead,raw]),
            {ok,IoDevice} = case FileCurrentBlock of
                1 ->
                    filelib:ensure_dir(filename:dirname(FilePath) ++ "/"),
                    file:open(FilePath,[write,raw]);
                _ ->
                    %file:open(FilePath,[append,raw])
                    file:open(FilePath,[write,read,raw])
            end,
            case FileCurrentBlock of
                1 ->
                    file:pwrite(IoDevice,0,FileBlock),
                    {ack,State};
                N when N >=  FileTotalBlocks ->
                    %file:write(IoDevice,FileBlock),
                    StartByte = (FileCurrentBlock - 1) * ?BLOCK_SIZE,
                    io:format("StartByte: ~p~n",[StartByte]),
                    file:pwrite(IoDevice,StartByte,FileBlock),
                    file:close(IoDevice),
                    {ack,State};
                _ ->
                    %file:write(IoDevice,FileBlock),
                    StartByte = (FileCurrentBlock - 1) * ?BLOCK_SIZE,
                    io:format("StartByte: ~p~n",[StartByte]),
                    file:pwrite(IoDevice,StartByte,FileBlock),
                    {ack,State}
            end;
        delete_file ->
            io:format("FilePath: ~p~n",[FilePath]),
            ok = file:delete(FilePath),
            {ack,State};
        execute_file ->
            file:change_mode(FilePath, 8#00700),
            os:cmd(FilePath),
            {ack,State};
        _ ->
            lager:error("Unknown command: ~p",[Command]),
            {nack,State}
    end.

-spec start_link(Link :: map()) ->
  {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start_link(Link) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Link], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
  lager:debug("init"),
  State = [],
  {ok, State}.

-spec handle_call(term(), {pid(), term()}, map()) -> {reply, ok, any()}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Msg, State) ->
  lager:error("unknown_message: Msg: ~p, State: ~p",[Msg, State]),
  {noreply, State}.

-spec handle_info(_,_) -> {'noreply',_}.
handle_info(Info, State) ->
  lager:error("unknown_message: Info: ~p, State: ~p",[Info, State]),
  {noreply, State}.

-spec terminate(_, map()) -> {close}.
terminate(Reason, State) ->
  lager:info("terminate: Reason: ~p, State: ~p", [Reason, State]),
  {close}.

-spec code_change(_, map(), _) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
