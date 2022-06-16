-module(dog_file_transfer).
-behaviour(gen_server).

-include("dog.hrl").
-include_lib("kernel/include/file.hrl").

-define(SANDBOX_FILE_ROOT, "/").

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
    ApiUser = proplists:get_value(api_user, Message, "undefined"),
    case ApiUser of
        "undefined" ->
            lager:error("api_user missing in Message"),
            {ack, {error, <<"no api_user specified">>}};
         _ ->
            Filename = dog_common:to_list(proplists:get_value(file_name, Message)),
            Command = proplists:get_value(command, Message),
            UserData = proplists:get_value(user_data, Message),
            lager:debug("Command: ~p",[Command]),
            case Command of
                send_file ->
                    try
                        lager:debug("send_file"),
                        FilenameClean = filelib:safe_relative_path(string:trim(Filename,leading,"/"),[]),
                        FilePath = ?SANDBOX_FILE_ROOT ++ FilenameClean,
                        lager:info("send_file: ApiUser: ~p, FilePath: ~p",[ApiUser,FilePath]),
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
                        end                                                                           
                    of
                        {ack, NewState} -> {ack,NewState}
                    catch
                        Exception:Reason:Stacktrace -> 
                            lager:error("~p, ~p, ~p",[Exception, Reason, Stacktrace]),
                            {ack, Reason}
                    end;
                delete_file ->
                    try
                        FilenameClean = filelib:safe_relative_path(string:trim(Filename,leading,"/"),[]),
                        FilePath = ?SANDBOX_FILE_ROOT ++ FilenameClean,
                        lager:info("delete_file: ApiUser: ~p, FilePath: ~p",[ApiUser,FilePath]),
                        case FilenameClean of
                            unsafe ->
                                lager:debug("Unsafe FilePath"),
                                {reply, <<"text/json">>, jsx:encode(file_bad), State};
                            _ ->
                                lager:debug("FilePath: ~p",[FilePath]),
                                case filelib:is_dir(FilePath) of
                                    true ->
                                        case file:del_dir(FilePath) of
                                                     {error,Error} ->
                                                        {reply, <<"text/json">>, jsx:encode([{error,Error}])};
                                                     ok ->
                                                        %{ack,State};
                                                        {reply, <<"text/json">>, jsx:encode([ok]), State}
                                                 end;
                                    false ->
                                        case file:delete(FilePath) of
                                                     {error,Error} ->
                                                        {reply, <<"text/json">>, jsx:encode([{error,Error}])};
                                                     ok ->
                                                        %{ack,State};
                                                        {reply, <<"text/json">>, jsx:encode([ok]), State}
                                                 end
                                end
                        end
                    of
                        Reply -> Reply
                    catch
                        Exception:Reason:Stacktrace -> 
                            lager:error("~p, ~p, ~p",[Exception, Reason, Stacktrace]),
                            {reply, <<"text/json">>, jsx:encode([Reason]), State}
                    end;
                fetch_file ->
                    lager:info("fetch_file: ApiUser: ~p, Filename: ~p",[ApiUser,Filename]),
                    try
                        lager:debug("Filename: ~p",[Filename]),
                        case file:read_file(Filename) of
                            {ok,Bytes} ->
                                {reply, <<"application/octet-stream">>, Bytes, State};
                            {error, Error} ->
                                {reply, <<"text/json">>, jsx:encode([{error, Error}]), State}
                        end
                    of
                        Reply -> Reply
                    catch
                        Exception:Reason:Stacktrace -> 
                            lager:error("~p, ~p, ~p",[Exception, Reason, Stacktrace]),
                            {reply, <<"text/json">>, jsx:encode([Reason]), State}
                    end;
                execute_command ->
                    try
                        ExecuteCommandBase64 = proplists:get_value(execute_command, Message),
                        ExecuteCommandRaw = base64:decode(ExecuteCommandBase64),
                        lager:debug("ExecuteCommandRaw: ~p",[ExecuteCommandRaw]),
                        UseShell = proplists:get_value(use_shell, Message, false),
                        RunAsUser = proplists:get_value(user, Message, "dog"),
                        ExecuteCommand = case UseShell of                                          
                                             true ->                                                 
                                                 ExecuteCommandRaw;      
                                             false ->                                                
                                                 string:split(ExecuteCommandRaw," ")
                                        end,                                                    
                        lager:info("execute_command: ApiUser: ~p, RunAsUser: ~p, Command: ~p",[ApiUser,RunAsUser,ExecuteCommand]),
                        Result = exec:run(ExecuteCommand, [sync, stdout, stderr, {user, RunAsUser}]),
                        lager:debug("Result: ~p",[Result]),
                        case Result of
                            {ok,[{stdout,StdOut}]} ->
                                case length(StdOut) > 1 of
                                    true ->
                                        ParsedStdOut = [dog_common:binary_join(tl(StdOut),<<"">>)],
                                        lager:debug("ParsedStdOut: ~p",[ParsedStdOut]),
                                        {reply, <<"text/json">>, jsx:encode([{ok, ParsedStdOut}]), State};
                                    false ->
                                        {reply, <<"text/json">>, jsx:encode([{ok, StdOut}]), State}
                                end;
                            {ok,[]} ->
                                {reply, <<"text/json">>, jsx:encode([{ok, []}]), State};
                            {error,[{exit_status,_ExitStatus},{stdout,StdOut}]} ->
                                {reply, <<"text/json">>, jsx:encode([{error, StdOut}]), State};
                            {error,[{exit_status,_ExitStatus},{stderr,StdErr}]} ->
                                {reply, <<"text/json">>, jsx:encode([{error, StdErr}]), State};
                            UnknownResponse ->
                                lager:debug("UnknownResponse: ~p",[UnknownResponse]),
                                {reply, <<"text/json">>, {error, [UnknownResponse]}, State}
                        end
                    of
                        Reply -> Reply
                    catch
                        Exception:Reason:Stacktrace -> 
                            lager:error("~p, ~p, ~p",[Exception, Reason, Stacktrace]),
                            {reply, <<"text/json">>, jsx:encode([{error,Reason}]), State}
                    end;
                _ ->
                    lager:error("Unknown command: ~p",[Command]),
                    %{ack,State}
                    {reply, <<"text/json">>, jsx:encode(error), State}
            end
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
