-module(ec2_callback).

-export([
         handle/2, 
         handle_event/3,
         lookup/2,
         metadata/0,
         serve_metadata/1
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
    ChanceOf404 = application:get_env(ec2_mock, chance_of_404, 0.0),
    case rand:uniform() of
        Rand when Rand > ChanceOf404 -> % set to > N to simulate random 404s
            try serve_metadata(RequestList) of
                Response ->  Response
            catch
                _:_ -> {404, [], error404()}
            end;
        _ -> {404, [], error404()}
    end.

lookup([],Value) ->
    Value;
lookup([Head | Tail],Map) ->
    case maps:get(Head,Map,not_found) of
        not_found ->
            Key = list_to_binary(io_lib:format("~s/",[Head])),
            %io:format("Key: ~p~n",[Key]),
            case maps:get(Key,Map,not_found) of
                not_found ->
                    not_found;
                Value ->
                    case is_map(Value) of
                        true ->
                            lookup(Tail,Value);
                        false ->
                            not_found
                    end
            end;
        Value ->
            lookup(Tail,Value)
    end.


serve_metadata(RequestList) ->
    Keys = lists:delete(<<"latest">>,RequestList),
    Lookup = lookup(Keys,metadata()),
    case Lookup of
        not_found ->
            {404, [], error404()};
        Value ->
            case erlang:is_map(Value) of
                true ->
                    FormatedList = lists:map(fun({K,V}) -> 
                                                     case is_map(V) of
                                                         true ->
                                                             io_lib:format("~s~n",[K]);
                                                         false ->
                                                             io_lib:format("~s~n",[K])
                                                     end
                                             end, maps:to_list(Value)),
                    LastEntry = lists:last(FormatedList),
                    LastEntryWithoutNewline = lists:reverse(lists:nthtail(1,lists:reverse(LastEntry))),
                    FormatedListWithoutLastEntry = lists:reverse(lists:nthtail(1,lists:reverse(FormatedList))),
                    FormatedListWithoutLastNewline = FormatedListWithoutLastEntry ++ LastEntryWithoutNewline,
                    {ok, [], lists:flatten(FormatedListWithoutLastNewline) };
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
