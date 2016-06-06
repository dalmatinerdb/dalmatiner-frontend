-module(dalmatiner_idx_handler).
-behaviour(cowboy_http_handler).

-export([send/4, content_type/1, init/3, handle/2, terminate/3]).

-ignore_xref([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

-dialyzer({no_opaque, handle/2}).
handle(Req, State) ->
    Req0 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
                                      <<"*">>, Req),
    case cowboy_req:qs_val(<<"q">>, Req0) of
        {undefined, Req1} ->
            F = fun (Socket, Transport) ->
                        File = code:priv_dir(dalmatiner_frontend) ++
                            "/static/index.html",
                        Transport:sendfile(Socket, File)
                end,
            Req2 = cowboy_req:set_resp_body_fun(F, Req1),
            {ok, Req3} = cowboy_req:reply(200, Req2),
            {ok, Req3, State};
        {Q, Req1} ->
            case timer:tc(dqe, run, [Q]) of
                {_, {error, E}} ->
                    Error = list_to_binary(dqe:error_string({error, E})),
                    lager:warning("Error in query [~s]: ~p", [Q, E]),
                    {ok, Req2} =
                        cowboy_req:reply(400, [], Error, Req1),
                    {ok, Req2, State};
                {T, {ok, Start, R2}} ->
                    R3 = [[{<<"n">>, Name},
                           {<<"r">>, Resolution},
                           {<<"v">>, mmath_bin:to_list(Data)}]
                          || {Name, Data, Resolution} <- R2],
                    D = [{<<"s">>, Start},
                         {<<"t">>, T},
                         {<<"d">>, R3}],
                    {ContentType, Req2} = content_type(Req1),
                    send(ContentType, D, Req2, State)
            end
    end.

content_type(Req) ->
    {ok, A, Req1} = cowboy_req:parse_header(<<"accept">>, Req),
    {content_type_(A), Req1}.

content_type_([]) ->
    other;
content_type_([{{<<"text">>, <<"html">>, _}, _, _} | _]) ->
    html;
content_type_([{{<<"application">>, <<"xhtml+xml">>, _}, _, _} | _]) ->
    html;
content_type_([{{<<"application">>, <<"json">>, _}, _, _} | _]) ->
    json;
content_type_([{{<<"application">>, <<"msgpack">>, _}, _, _} | _]) ->
    msgpack;
content_type_([{{<<"application">>, <<"x-msgpack">>, _}, _, _} | _]) ->
    msgpack;
content_type_([_ | R]) ->
    content_type_(R).

send(json, D, Req, State) ->
    {ok, Req1} =
        cowboy_req:reply(
          200, [{<<"content-type">>, <<"application/json">>}],
          jsone:encode(D), Req),
    {ok, Req1, State};
send(msgpack, D, Req, State) ->
    {ok, Req1} =
        cowboy_req:reply(
          200, [{<<"content-type">>, <<"application/x-msgpack">>}],
          msgpack:pack(D, [jsx]), Req),
    {ok, Req1, State};
send(_, _D, Req, State) ->
    {ok, Req1} = cowboy_req:reply(415, Req),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.
