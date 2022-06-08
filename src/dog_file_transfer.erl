-module(dog_file_transfer).
-behaviour(gen_server).

-include("dog.hrl").
-include_lib("kernel/include/file.hrl").

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
         subscriber_loop/4
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
subscriber_loop(_RoutingKey, _CType, Payload, State) -> 
    Message = binary_to_term(Payload),
    lager:debug("Message: ~p",[Message]),
    Filename = dog_common:to_list(proplists:get_value(file_name, Message)),
    Command = proplists:get_value(command, Message),
    UserData = proplists:get_value(user_data, Message),
    lager:debug("Command: ~p",[Command]),
    case Command of
        send_file ->
            FilenameClean = filelib:safe_relative_path(string:trim(Filename,leading,"/"),[]),
            FilePath = "/tmp/dog/" ++ FilenameClean,
            case FilenameClean of
                unsafe ->
                    lager:debug("Unsafe FilePath"),
                    {reply, <<"text/json">>, jsx:encode(file_bad), State};
                _ ->
                    FileTotalBlocks = proplists:get_value(total_blocks, Message),
                    FileCurrentBlock = proplists:get_value(current_block, Message),
                    MaxBlockSizeBytes = proplists:get_value(max_block_size_bytes, Message),
                    FileBlock = maps:get(file_block, UserData),
                    lager:debug("FilePath: ~p",[FilePath]),
                    lager:debug("Filename: ~p, MaxBlockSizeBytes: ~p, Block ~p of ~p",[Filename,MaxBlockSizeBytes,FileCurrentBlock,FileTotalBlocks]),
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
                        1 when FileTotalBlocks =:= 1 ->
                            file:pwrite(IoDevice,0,FileBlock),
                            file:close(IoDevice),
                            {ack,State};
                            %{reply, <<"text/json">>, jsx:encode(block_ok), State};
                        1 when FileTotalBlocks > 1 ->
                            file:pwrite(IoDevice,0,FileBlock),
                            %{reply, <<"text/json">>, jsx:encode(block_ok), State};
                            {ack,State};
                        N when N >=  FileTotalBlocks ->
                            %file:write(IoDevice,FileBlock),
                            StartByte = (FileCurrentBlock - 1) * MaxBlockSizeBytes,
                            lager:debug("StartByte: ~p",[StartByte]),
                            file:pwrite(IoDevice,StartByte,FileBlock),
                            file:close(IoDevice),
                            {ack,State};
                            %{reply, <<"text/json">>, jsx:encode(block_ok), State};
                        _ ->
                            %file:write(IoDevice,FileBlock),
                            StartByte = (FileCurrentBlock - 1) * MaxBlockSizeBytes,
                            lager:debug("StartByte: ~p",[StartByte]),
                            file:pwrite(IoDevice,StartByte,FileBlock),
                            {ack,State}
                            %{reply, <<"text/json">>, jsx:encode(file_ok), State}
                end    
            end;
        delete_file ->
            FilenameClean = filelib:safe_relative_path(string:trim(Filename,leading,"/"),[]),
            FilePath = "/tmp/dog/" ++ FilenameClean,
            case FilenameClean of
                unsafe ->
                    lager:debug("Unsafe FilePath"),
                    {reply, <<"text/json">>, jsx:encode(file_bad), State};
                _ ->
                    lager:debug("FilePath: ~p",[FilePath]),
                    case filelib:is_dir(FilePath) of
                        true ->
                            case file:del_dir(FilePath) of
                                         {error,Reason} ->
                                            {reply, <<"text/json">>, jsx:encode([{error,Reason}])};
                                         ok ->
                                            %{ack,State};
                                            {reply, <<"text/json">>, jsx:encode([ok]), State}
                                     end;
                        false ->
                            case file:delete(FilePath) of
                                         {error,Reason} ->
                                            {reply, <<"text/json">>, jsx:encode([{error,Reason}])};
                                         ok ->
                                            %{ack,State};
                                            {reply, <<"text/json">>, jsx:encode([ok]), State}
                                     end
                    end
            end;
        fetch_file ->
            case file:read_file(Filename) of
                {ok,Bytes} ->
                    {reply, <<"application/octet-stream">>, Bytes, State};
                {error, Reason} ->
                    {reply, <<"text/json">>, jsx:encode({error, Reason}), State}
            end;
        execute_command ->
            ExecuteCommandRaw = proplists:get_value(execute_command, Message),
            UseShell = proplists:get_value(use_shell, Message, false),
            RunAsUser = proplists:get_value(user, Message, "dog"),
            ExecuteCommand = case UseShell of                                          
                                 true ->                                                 
                                     ExecuteCommandRaw;      
                                 false ->                                                
                                     string:split(ExecuteCommandRaw," ")
                            end,                                                    
            case exec:run(ExecuteCommand, [sync, stdout, stderr, {user, RunAsUser}]) of
                {ok,[{stdout,StdOut}]} ->
                    {reply, <<"text/json">>, jsx:encode([{ok, StdOut}]), State};
                {error,[{exit_status,_ExitStatus},{stderr,StdErr}]} ->
                    {reply, <<"text/json">>, jsx:encode([{error, StdErr}]), State}
            end;
        _ ->
            lager:error("Unknown command: ~p",[Command]),
            %{ack,State}
            {reply, <<"text/json">>, jsx:encode(error), State}
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
