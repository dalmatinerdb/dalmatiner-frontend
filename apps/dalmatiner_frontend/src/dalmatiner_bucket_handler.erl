%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(dalmatiner_bucket_handler).

-export([init/3, handle/2, terminate/3]).

-ignore_xref([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

content_type([]) ->
    other;
content_type([{{<<"text">>,<<"html">>,_},_,_} | _]) ->
    html;
content_type([{{<<"application">>,<<"xhtml+xml">>,_},_,_} | _]) ->
    html;
content_type([{{<<"application">>,<<"json">>,_},_,_} | _]) ->
    json;
content_type([{{<<"application">>,<<"msgpack">>,_},_,_} | _]) ->
    msgpack;
content_type([{{<<"application">>,<<"x-msgpack">>,_},_,_} | _]) ->
    msgpack;
content_type([_ | R]) ->
    content_type(R).

handle(Req, State) ->
    {ok, R, Req1} = cowboy_req:parse_header(<<"accept">>, Req),
    case content_type(R) of
        html ->
            F = fun (Socket, Transport) ->
                        File = code:priv_dir(dalmatiner_frontend) ++
                            "/static/bucket.html",
                        Transport:sendfile(Socket, File)
                end,
            Req2 = cowboy_req:set_resp_body_fun(F, Req1),
            {ok, Req3} = cowboy_req:reply(200, Req2),
            {ok, Req3, State};
        json ->
            {ok, Bs} = dalmatiner_connection:list(),
            {ok, Req2} =
                cowboy_req:reply(200, [{<<"content-type">>,
                                        <<"application/json-msgpack">>}],
                                 jsx:encode(Bs), Req1),
            {ok, Req2, State};
        msgpack ->
            {ok, Bs} = dalmatiner_connection:list(),
            {ok, Req2} =
                cowboy_req:reply(200, [{<<"content-type">>,
                                        <<"application/x-msgpack">>}],
                                 msgpack:pack(Bs), Req1),
            {ok, Req2, State};
        O ->
            io:format("~p <- ~p~n", [O, R]),
            {ok, Req2} = cowboy_req:reply(415, Req1),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, State) ->
    {ok, State}.
