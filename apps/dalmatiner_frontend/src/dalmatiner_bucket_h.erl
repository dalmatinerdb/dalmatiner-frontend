-module(dalmatiner_bucket_h).
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
                            "/static/bucket.html",
                        Transport:sendfile(Socket, File)
                end,
            Req2 = cowboy_req:set_resp_body_fun(F, Req1),
            {ok, Req3} = cowboy_req:reply(200, Req2),
            {ok, Req3, State};
        _ ->
            {ok, Bs} = ddb_connection:list(),
            Bs1 = [bucket_info(B) || B <- Bs],
            dalmatiner_idx_handler:send(ContentType, Bs1, Req1, State)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

bucket_info(B) ->
    case ddb_connection:info(B) of
        {ok, I} ->
            I#{name => B};
        _ ->
            #{name => B}
    end.
