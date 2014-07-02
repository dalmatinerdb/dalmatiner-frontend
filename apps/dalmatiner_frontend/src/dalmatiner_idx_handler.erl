%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(dalmatiner_idx_handler).

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
    case cowboy_req:qs_val(<<"q">>, Req) of
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
            case timer:tc(dql, execute, [Q]) of
                {_, {error, E}} ->
                    {ok, Req2} =
                        cowboy_req:reply(400, [], list_to_binary(E), Req1),
                    {ok, Req2, State};
                {T, {ok, R2}} ->
                    {ok, A, Req2} = cowboy_req:parse_header(<<"accept">>, Req1),
                    R3 = [[{<<"n">>, N}, {<<"v">>, mmath_bin:to_list(E)}]
                          || {N, E} <- R2],
                    D = [{<<"t">>, T},
                         {<<"d">>, R3}],
                    case content_type(A) of
                        json ->
                            {ok, Req3} =
                                cowboy_req:reply(
                                  200, [{<<"content-type">>, <<"application/json">>}],
                                  jsx:encode(D), Req2),
                            {ok, Req3, State};
                        msgpack ->
                            {ok, Req3} =
                                cowboy_req:reply(
                                  200, [{<<"content-type">>, <<"application/x-msgpack">>}],
                                  msgpack:pack(D, [jsx]), Req2),
                            {ok, Req3, State};
                        _ ->
                            {ok, Req2} = cowboy_req:reply(415, Req1),
                            {ok, Req2, State}
                    end
            end
    end.

terminate(_Reason, _Req, State) ->
    {ok, State}.
