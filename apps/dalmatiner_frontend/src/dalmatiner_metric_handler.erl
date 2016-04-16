%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(dalmatiner_metric_handler).

-export([init/3, handle/2, terminate/3]).

-ignore_xref([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.


-dialyzer({no_opaque, handle/2}).
handle(Req, State) ->
    Req0 = cowboy_req:set_resp_header(
             <<"access-control-allow-origin">>, <<"*">>, Req),
    {[Bucket | Path], Req1} = cowboy_req:path_info(Req0),
    {ContentType, Req2} = dalmatiner_idx_handler:content_type(Req1),
    case ContentType of
        html ->
            F = fun (Socket, Transport) ->
                        File = code:priv_dir(dalmatiner_frontend) ++
                            "/static/metric.html",
                        Transport:sendfile(Socket, File)
                end,
            Req3 = cowboy_req:set_resp_body_fun(F, Req2),
            {ok, Req4} = cowboy_req:reply(200, Req3),
            {ok, Req4, State};
        _ ->
            D = get_metrics(Bucket, Path),
            dalmatiner_idx_handler:send(ContentType, D, Req2, State)
    end.

terminate(_Reason, _Req, State) ->
    {ok, State}.

get_metrics(Bucket, []) ->
    {ok, Ms} = ddb_connection:list(Bucket),
    Sep = <<"'.'">>,
    [<<"'", (dproto:metric_to_string(Metric, Sep))/binary, "'">>
         || Metric <- Ms];

get_metrics(Bucket, Path) ->
    {ok, Ms} = ddb_connection:list(
                 Bucket, dproto:metric_from_list(Path)),
    Sep = <<"'.'">>,
    [<<"'", (dproto:metric_to_string(Metric, Sep))/binary, "'">>
         || Metric <- Ms].
