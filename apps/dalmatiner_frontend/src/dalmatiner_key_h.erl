-module(dalmatiner_key_h).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-ignore_xref([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.


-dialyzer({no_opaque, handle/2}).
handle(Req, State) ->
    {[Bucket | Path], Req1} = cowboy_req:path_info(Req),
    {ContentType, Req2} = dalmatiner_idx_handler:content_type(Req1),
    case ContentType of
        html ->
            F = fun (Socket, Transport) ->
                        File = code:priv_dir(dalmatiner_frontend) ++
                            "/static/key.html",
                        Transport:sendfile(Socket, File)
                end,
            Req3 = cowboy_req:set_resp_body_fun(F, Req2),
            {ok, Req4} = cowboy_req:reply(200, Req3),
            {ok, Req4, State};
        _ ->
            D = get_metrics(Bucket, Path),
            dalmatiner_idx_handler:send(ContentType, D, Req2, State)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

get_metrics(Bucket, []) ->
    {ok, Ms} = ddb_connection:list(Bucket),
    Sep = <<"'.'">>,
    [<<"'", (dproto:metric_to_string(Metric, Sep))/binary, "'">>
         || Metric <- Ms];

get_metrics(Bucket, Path) ->
    {ok, Ms} = ddb_connection:list_pfx(
                 Bucket, dproto:metric_from_list(Path)),
    Sep = <<"'.'">>,
    [<<"'", (dproto:metric_to_string(Metric, Sep))/binary, "'">>
         || Metric <- Ms].
