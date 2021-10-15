-module(ec2_callback).

-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").

-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"latest">>, <<"meta-data">>], _Req) ->
    %% Reply with a normal response. `ok' can be used instead of `200'
    %% to signal success.
    %%
    Response =
    <<"ami-id\nami-launch-index\nami-manifest-path\n"
      "block-device-mapping/\nevents/\nhostname\nide"
      "ntity-credentials/\ninstance-action\ninstance"
      "-id\ninstance-type\nkernel-id\nlocal-hostname\n"
      "local-ipv4\nmac\nmetrics/\nnetwork/\nplacemen"
      "t/\nprofile\npublic-hostname\npublic-ipv4\npu"
      "blic-keys/\nreservation-id\nsecurity-groups\n"
      "services/">>,
    {ok, [], Response};
handle('GET',
       [<<"latest">>, <<"meta-data">>, <<"network">>,
        <<"interfaces">>, <<"macs">>],
       _Req) ->
    case rand:uniform() of
        %Rand when Rand > 5.0e-1 ->
        Rand when Rand > 0 ->
            Response = <<"22:00:0a:b9:31:e1\n00:11:22:33:44:55">>,
            {ok, [], Response};
        _ -> {503, [], <<"Not Found">>}
    end;
handle('GET',
       [<<"latest">>, <<"meta-data">>, <<"network">>,
        <<"interfaces">>, <<"macs">>, <<"22:00:0a:b9:31:e1">>,
        <<"public-ipv4s">>],
       _Req) ->
    case rand:uniform() of
        %Rand when Rand > 5.0e-1 ->
        Rand when Rand > 0 ->
            Response = <<"54.235.213.32">>, {ok, [], Response};
        _ -> {503, [], <<"Not Found">>}
    end;
handle('GET',
       [<<"latest">>, <<"meta-data">>, <<"network">>,
        <<"interfaces">>, <<"macs">>, <<"00:11:22:33:44:55">>,
        <<"public-ipv4s">>],
       _Req) ->
    case rand:uniform() of
        %Rand when Rand > 5.0e-1 ->
        Rand when Rand > 0 ->
            Response = <<"11.22.33.44">>, {ok, [], Response};
        _ -> {503, [], <<"Not Found">>}
    end;
handle('GET',
       [<<"latest">>, <<"meta-data">>, <<"instance-id">>],
       _Req) ->
    case rand:uniform() of
        Rand when Rand > 0 ->
            Response = <<"i-05664d6383520bc3e">>, {ok, [], Response};
        _ -> {503, [], <<"Not Found">>}
    end;
handle('GET',
       [<<"latest">>, <<"meta-data">>, <<"security-groups">>],
       _Req) ->
    case rand:uniform() of
        Rand when Rand > 0 ->
            Response = <<"dog-test">>, {ok, [], Response};
        _ -> {503, [], <<"Not Found">>}
    end;
handle('GET',
       [<<"latest">>, <<"meta-data">>, <<"placement">>,<<"availability-zone">>],
       _Req) ->
    case rand:uniform() of
        Rand when Rand > 0 ->
            Response = <<"us-east-1a">>, {ok, [], Response};
        _ -> {503, [], <<"Not Found">>}
    end;
handle('GET', [<<"hello">>, <<"world">>], _Req) ->
    %% Reply with a normal response. `ok' can be used instead of `200'
    %% to signal success.
    {ok, [], <<"Hello World!">>};
handle(_, _, _Req) -> {404, [], <<"Not Found">>}.

%% @doc Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return `ok'.
handle_event(_Event, _Data, _Args) -> ok.
