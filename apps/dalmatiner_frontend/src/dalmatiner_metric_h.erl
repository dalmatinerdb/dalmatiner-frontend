-module(dalmatiner_metric_h).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-ignore_xref([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

-dialyzer({no_opaque, handle/2}).
handle(Req, State) ->
    {ContentType, Req1} = dalmatiner_idx_handler:content_type(Req),
    case ContentType of
        html ->
            F = fun (Socket, Transport) ->
                        File = code:priv_dir(dalmatiner_frontend) ++
                            "/static/metric.html",
                        Transport:sendfile(Socket, File)
                end,
            Req2 = cowboy_req:set_resp_body_fun(F, Req1),
            {ok, Req3} = cowboy_req:reply(200, Req2),
            {ok, Req3, State};
        _ ->
            {Collection, Req2} = cowboy_req:binding(collection, Req1),
            {Depth, Req3} = cowboy_req:qs_val(<<"depth">>, Req2),
            {Prefix, Req4} = cowboy_req:binding(prefix, Req3),
            list_metrics(ContentType, Collection, Prefix, Depth, Req4, State)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

list_metrics(ContentType, Collection, undefined, undefined, Req, State) ->
    {ok, Ms} = dqe_idx:metrics(Collection),
    list_metrics(ContentType, Ms, Req, State);
list_metrics(ContentType, Collection, Prefix64, DepthBin, Req, State) ->
    Prefix = decode_prefix(Prefix64),
    Depth = decode_depth(DepthBin),
    {ok, Ms} = dqe_idx:metrics(Collection, Prefix, Depth),
    Ms1 = [Prefix ++ M || M <- Ms],
    list_metrics(ContentType, Ms1, Req, State).

list_metrics(ContentType, Metrics, Req, State) ->
    Ms = [[{key, base64:encode(dproto:metric_from_list(M))},
           {parts, M}] || M <- Metrics],
    dalmatiner_idx_handler:send(ContentType, Ms, Req, State).

decode_depth(undefined) ->
    1;
decode_depth(Depth) when is_binary(Depth) ->
    list_to_integer(binary_to_list(Depth)).

decode_prefix(undefined) ->
    [];
decode_prefix(<<>>) ->
    [];
decode_prefix(Prefix64) when is_binary(Prefix64) ->
    PrefixBin = base64:decode(Prefix64),
    dproto:metric_to_list(PrefixBin).
