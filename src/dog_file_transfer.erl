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
    ?LOG_DEBUG("Message: ~p",[Message]),
    ApiUser = proplists:get_value(api_user, Message, "undefined"),
    case ApiUser of
        "undefined" ->
            ?LOG_ERROR("api_user missing in Message"),
            {ack, {error, <<"no api_user specified">>}};
        _ ->
            Filename = dog_common:to_list(proplists:get_value(file_name, Message)),
            Command = proplists:get_value(command, Message),
            UserData = proplists:get_value(user_data, Message),
            ?LOG_DEBUG(#{ command => Command}),
            case Command of
                send_file ->
                    send_file(State, ApiUser, Message, Filename, UserData);
                delete_file ->
                    delete_file(State, ApiUser, Filename);
                fetch_file ->
                    fetch_file(State, ApiUser, Filename);
                execute_command ->
                    execute_command(State, ApiUser, Message)
            end
    end.

fetch_file(State,ApiUser,Filename) ->
    ?LOG_INFO(#{apiuser => ApiUser, filename => Filename}),
    try
        ?LOG_DEBUG(#{filename => Filename}),
        case file:read_file(Filename) of
            {ok,Bytes} ->
                {reply, <<"application/octet-stream">>, Bytes, State};
            {error, Error} ->
                {reply, <<"text/json">>, jsx:encode([{error, Error}]), State}
        end
    of
        Reply -> Reply
    catch %TODO: replace with 'after'
        Exception:Reason:Stacktrace ->
            ?LOG_ERROR(#{ exception => Exception, reason => Reason, stacktrace => Stacktrace}),
            {reply, <<"text/json">>, jsx:encode([Reason]), State}
    end.

delete_file(State,ApiUser,Filename) ->
    try
        FilenameClean = filelib:safe_relative_path(string:trim(Filename,leading,"/"),[]),
        FilePath = ?SANDBOX_FILE_ROOT ++ FilenameClean,
        ?LOG_DEBUG(#{ apiuser => ApiUser, filepath => FilePath}),
        case FilenameClean of
            unsafe ->
                ?LOG_ERROR("Unsafe FilePath"),
                {reply, <<"text/json">>, jsx:encode(file_bad), State};
            _ ->
                ?LOG_DEBUG(#{ filepath => FilePath}),
                case filelib:is_dir(FilePath) of
                    true ->
                        case file:del_dir(FilePath) of
                            {error,Error} ->
                                {reply, <<"text/json">>, jsx:encode([{error,Error}])};
                            ok ->
                                {reply, <<"text/json">>, jsx:encode([ok]), State}
                        end;
                    false ->
                        case file:delete(FilePath) of
                            {error,Error} ->
                                {reply, <<"text/json">>, jsx:encode([{error,Error}])};
                            ok ->
                                {reply, <<"text/json">>, jsx:encode([ok]), State}
                        end
                end
        end
    of
        Reply -> Reply
    catch %TODO: replace with 'after'
        Exception:Reason:Stacktrace ->
            ?LOG_ERROR(#{ exception => Exception, reason => Reason, stacktrace => Stacktrace}),
            {reply, <<"text/json">>, jsx:encode([Reason]), State}
    end.

send_file(State, ApiUser, Message, Filename, UserData) ->
    %timer:sleep(10000),
    %{ack, test_timeout}.
    try
        ?LOG_DEBUG("send_file"),
        FilenameClean = filelib:safe_relative_path(string:trim(Filename,leading,"/"),[]),
        FilePath = ?SANDBOX_FILE_ROOT ++ FilenameClean,
        ?LOG_INFO(#{ apiuser => ApiUser, filepath => FilePath}),
        case FilenameClean of
            unsafe ->
                ?LOG_DEBUG("Unsafe FilePath"),
                {reply, <<"text/json">>, jsx:encode(file_bad), State};
            _ ->
                FileTotalBlocks = proplists:get_value(total_blocks, Message),
                FileCurrentBlock = proplists:get_value(current_block, Message),
                MaxBlockSizeBytes = proplists:get_value(max_block_size_bytes, Message),
                FileBlock = maps:get(file_block, UserData),
                ?LOG_DEBUG(#{ filepath => FilePath}),
                ?LOG_DEBUG(#{ filename => Filename, maxblocksizebytes => MaxBlockSizeBytes, file_currenp_block => FileCurrentBlock,file_total_blocks => FileTotalBlocks}),
                %{ok,IoDevice} = file:open(FilePath,[write,binary,read_ahead,raw]),
                {ok,IoDevice} = case FileCurrentBlock of
                                    1 ->
                                        ok = filelib:ensure_dir(filename:dirname(FilePath) ++ "/"),
                                        {ok, _IODevice} = file:open(FilePath,[write,raw]);
                                    _ ->
                                        %file:open(FilePath,[append,raw])
                                        {ok, _IODevice} = file:open(FilePath,[write,read,raw])
                                end,
                case FileCurrentBlock of
                    1 when FileTotalBlocks =:= 1 ->
                        ok = file:pwrite(IoDevice,0,FileBlock),
                        ok = file:sync(IoDevice),
                        ok = file:close(IoDevice),
                        {ack,State};
                    %{reply, <<"text/json">>, jsx:encode(block_ok), State};
                    1 when FileTotalBlocks > 1 ->
                        ok = file:pwrite(IoDevice,0,FileBlock),
                        %{reply, <<"text/json">>, jsx:encode(block_ok), State};
                        {ack,State};
                    N when N >=  FileTotalBlocks ->
                        %file:write(IoDevice,FileBlock),
                        StartByte = (FileCurrentBlock - 1) * MaxBlockSizeBytes,
                        ?LOG_DEBUG(#{start_byte => StartByte}),
                        ok = file:pwrite(IoDevice,StartByte,FileBlock),
                        ok = file:sync(IoDevice),
                        ok = file:close(IoDevice),
                        {ack,State};
                    %{reply, <<"text/json">>, jsx:encode(block_ok), State};
                    _ ->
                        %file:write(IoDevice,FileBlock),
                        StartByte = (FileCurrentBlock - 1) * MaxBlockSizeBytes,
                        ?LOG_DEBUG(#{start_byte => StartByte}),
                        ok = file:pwrite(IoDevice,StartByte,FileBlock),
                        {ack,State}
                        %{reply, <<"text/json">>, jsx:encode(file_ok), State}
                end
        end
    of
        {ack, NewState} -> {ack,NewState}
    catch %TODO: replace with 'after'
        Exception:Reason:Stacktrace ->
            ?LOG_ERROR(#{ exception => Exception, reason => Reason, stacktrace => Stacktrace}),
            {ack, Reason}
    end.

execute_command(State, ApiUser, Message) ->
    ExecuteCommandBase64 = proplists:get_value(execute_command, Message),
    ExecuteCommandRaw = base64:decode(ExecuteCommandBase64),
    ?LOG_DEBUG(#{execute_command_raw => ExecuteCommandRaw}),
    UseShell = proplists:get_value(use_shell, Message, false),
    CmdUser = application:get_env(dog, cmd_user, "dog"),
    ExecuteCommand = case UseShell of
                         true ->
                             ExecuteCommandRaw;
                         false ->
                             string:split(ExecuteCommandRaw," ")
                     end,
    ?LOG_INFO(#{api_user => ApiUser, cmd_user => CmdUser, execute_command => ExecuteCommand}),
    try
        Result = exec:run(ExecuteCommand, [sync, stdout, stderr, {user, CmdUser}]),
        ?LOG_DEBUG(#{result => Result}),
        case Result of
            {ok,[{stdout,StdOut}]} ->
                case length(StdOut) > 1 of
                    true ->
                        ParsedStdOut = [erlang:iolist_to_binary(StdOut)],
                        ?LOG_INFO(#{parsed_stdout => ParsedStdOut}),
                        {reply, <<"text/json">>, jsx:encode([{ok, ParsedStdOut}]), State};
                    false ->
                        ?LOG_INFO(#{stdout => StdOut}),
                        %timer:sleep(5000),
                        {reply, <<"text/json">>, jsx:encode([{ok, StdOut}]), State}
                end;
            {ok,[{stdout,StdOut},{stderr,StdErr}]} ->
                StdCombined = [StdOut,StdErr],
                ParsedStdCombined = [erlang:iolist_to_binary(StdCombined)],
                ?LOG_INFO(#{parsed_stdout => ParsedStdCombined}),
                {reply, <<"text/json">>, jsx:encode([{ok, ParsedStdCombined}]), State};
            {ok,[]} ->
                {reply, <<"text/json">>, jsx:encode([{ok, []}]), State};
            {error,[{exit_status,_ExitStatus},{stdout,StdOut}]} ->
                {reply, <<"text/json">>, jsx:encode([{error, StdOut}]), State};
            {error,[{exit_status,_ExitStatus},{stderr,StdErr}]} ->
                {reply, <<"text/json">>, jsx:encode([{error, StdErr}]), State};
            {error, Reason} ->
                {reply, <<"text/json">>, jsx:encode([{error, Reason}]), State};
            UnknownResponse ->
                ?LOG_DEBUG(#{unknown_response => UnknownResponse}),
                {reply, <<"text/json">>, {error, [UnknownResponse]}, State}
        end
    after
        {reply, <<"text/json">>, {error, <<"exec error">>}, State}
    end.

-spec start_link(Link :: map()) ->
    {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start_link(Link) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Link], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
    ?LOG_DEBUG("init"),
    State = [],
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, map()) -> {reply, ok, any()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    ?LOG_ERROR(#{log => "unknown_message", msg => Msg, state => State}),
    {noreply, State}.

-spec handle_info(_,_) -> {'noreply',_}.
handle_info(Info, State) ->
    ?LOG_ERROR(#{ log => "unknown_message",  info => Info, state => State}),
    {noreply, State}.

-spec terminate(_, map()) -> {close}.
terminate(Reason, State) ->
    ?LOG_INFO(#{ log => "terminate",  reason => Reason, state => State}),
    {close}.

-spec code_change(_, map(), _) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
