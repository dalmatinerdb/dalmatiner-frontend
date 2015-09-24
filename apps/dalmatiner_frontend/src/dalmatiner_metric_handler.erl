%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(dalmatiner_metric_handler).

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

-dialyzer({no_opaque, handle/2}).
handle(Req, State) ->
    Req0 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    {ok, R, Req1} = cowboy_req:parse_header(<<"accept">>, Req0),
    {[Bucket | Path], Req2} = cowboy_req:path_info(Req1),
    case content_type(R) of
        html ->
            F = fun (Socket, Transport) ->
                        File = code:priv_dir(dalmatiner_frontend) ++
                            "/static/metric.html",
                        Transport:sendfile(Socket, File)
                end,
            Req3 = cowboy_req:set_resp_body_fun(F, Req2),
            {ok, Req4} = cowboy_req:reply(200, Req3),
            {ok, Req4, State};
        json ->
            {ok, Req3} =
                cowboy_req:reply(
                  200, [{<<"content-type">>, <<"application/json">>}],
                  jsx:encode(get_metrics(Bucket, Path)), Req2),
            {ok, Req3, State};
        msgpack ->
            {ok, Req3} =
                cowboy_req:reply(
                  200, [{<<"content-type">>, <<"application/x-msgpack">>}],
                  msgpack:pack(get_metrics(Bucket, Path)), Req2),
            {ok, Req3, State};
        _ ->
            {ok, Req2} = cowboy_req:reply(415, Req1),
            {ok, Req1, State}
    end.

terminate(_Reason, _Req, State) ->
    {ok, State}.

get_metrics(Bucket, []) ->
    {ok, Ms} = dalmatiner_connection:list(Bucket),
    Sep = <<"'.'">>,
    [<<"'", (dproto:metric_to_string(Metric, Sep))/binary, "'">>
         || Metric <- Ms];

get_metrics(Bucket, Path) ->
    {ok, Ms} = dalmatiner_connection:list(Bucket, dproto:metric_from_list(Path)),
    Sep = <<"'.'">>,
    [<<"'", (dproto:metric_to_string(Metric, Sep))/binary, "'">>
         || Metric <- Ms].
