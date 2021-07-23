-module(dog_exec).

-export([
	exec/1
	]).

exec(Command) ->
    Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
    Result = get_data(Port, []),
    Result.

get_data(Port, Sofar) ->
    receive
    {_, {data, Bytes}} ->
        get_data(Port, [Sofar|Bytes]);
    {_, eof} ->
        Port ! {self(), close},
        receive
        {_, closed} ->
            true
        end,
        receive
        {'EXIT',  _,  _} ->
            ok
        after 1 ->              % force context switch
            ok
        end,
        ExitCode =
            receive
            {_, {exit_status, Code}} ->
                Code
        end,
        {ExitCode, lists:flatten(Sofar)}
    end.
