-module(dalmatiner_event_h).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-ignore_xref([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.


-dialyzer({no_opaque, handle/2}).
handle(Req, State) ->
    {ContentType, Req1} = dalmatiner_idx_handler:content_type(Req),
    {Bucket, Req2} = cowboy_req:qs_val(<<"bucket">>, Req1),
    {StartS, Req3} = cowboy_req:qs_val(<<"start">>, Req2),
    {EndS, Req4} = cowboy_req:qs_val(<<"end">>, Req3),

    case {ContentType, Bucket, StartS, EndS} of
        {html, _, _, _} ->
            F = fun (Socket, Transport) ->
                        File = code:priv_dir(dalmatiner_frontend) ++
                            "/static/event.html",
                        Transport:sendfile(Socket, File)
                end,
            Req5 = cowboy_req:set_resp_body_fun(F, Req4),
            {ok, Req6} = cowboy_req:reply(200, Req5),
            {ok, Req6, State};
        {_, Bucket, _, _}
          when Bucket =:= undefined; Bucket =:= <<>> ->
            {ok, Req5} = cowboy_req:reply(400, [], "Bad bucket", Req4),
            {ok, Req5, State};
        {_, _, StartS, _}
          when StartS =:= undefined; StartS =:= <<>> ->
            {ok, Req5} = cowboy_req:reply(400, [], "Bad start", Req4),
            {ok, Req5, State};
        {_, _, _, EndS}
          when EndS =:= undefined; EndS =:= <<>> ->
            {ok, Req5} = cowboy_req:reply(400, [], "Bad end", Req4),
            {ok, Req5, State};
        _ ->
            StartSec = qdate:to_unixtime(StartS),
            Start = erlang:convert_time_unit(StartSec, seconds, nano_seconds),
            EndSec = qdate:to_unixtime(EndS),
            End = erlang:convert_time_unit(EndSec, seconds, nano_seconds),
            {ok, Es} = ddb_connection:read_events(Bucket, Start, End),
            Es1 = [[{<<"timestamp">>, T}, {<<"event">>, E}] ||
                      {T, E} <- Es],
            dalmatiner_idx_handler:send(ContentType, Es1, Req4, State)
    end.

terminate(_Reason, _Req, _State) ->
    ok.
