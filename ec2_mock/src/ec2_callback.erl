-module(ec2_callback).

-export([
         handle/2, 
         handle_event/3,
         metadata/0
        ]).

-include_lib("elli/include/elli.hrl").

-behaviour(elli_handler).

metadata() ->
    {ok, File} = file:read_file("priv/metadata.json"),
    jsx:decode(File).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).


handle('GET', RequestList, _Req) ->
    case rand:uniform() of
        Rand when Rand > 0 -> % set to > N to simulate random 404s
            try serve_metadata(RequestList) of
                Response ->  Response
            catch
                _:_ -> {404, [], error404()}
            end;
        _ -> {404, [], error404()}
    end.

serve_metadata(RequestList) ->
    Key = lists:delete(<<"latest">>,RequestList),
    case nested:get(Key,metadata(),not_found) of
        not_found ->
            {404, [], error404()};
        Value ->
            case erlang:is_map(Value) of
                true ->
                     FormatedList = lists:map(fun({K,V}) -> 
                                           case is_map(V) of
                                               true ->
                                                   io_lib:format("~s/~n",[K]);
                                               false ->
                                                   io_lib:format("~s~n",[K])
                                           end
                                           end, maps:to_list(Value)),
                     LastEntry = lists:last(FormatedList),
                     LastEntryWithoutNewline = lists:reverse(lists:nthtail(1,lists:reverse(LastEntry))),
                     FormatedListWithoutLastEntry = lists:reverse(lists:nthtail(1,lists:reverse(FormatedList))),
                     FormatedListWithoutLastNewline = FormatedListWithoutLastEntry ++ LastEntryWithoutNewline,
                    {ok, [], FormatedListWithoutLastNewline };
                false ->
                    {ok, [], Value}
            end
    end.
    

error404() ->
    <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
    <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
     <head>
      <title>404 - Not Found</title>
     </head>
     <body>
      <h1>404 - Not Found</h1>
     </body
    </html>">>.

%% @doc Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return `ok'.
handle_event(_Event, _Data, _Args) -> ok.
