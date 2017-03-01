-module(dalmatiner_idx_handler).
-behaviour(cowboy_http_handler).

-export([send/4, content_type/1, init/3, handle/2, terminate/3]).

-ignore_xref([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

-dialyzer({no_opaque, handle/2}).
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
            {Opts, Req2} = build_opts(Req1),
            ReqR = Req2,
            case timer:tc(dqe, run, [Q, Opts]) of
                {_, {error, E}} ->
                    Error = list_to_binary(dqe:error_string({error, E})),
                    lager:warning("Error in query [~s]: ~p", [Q, E]),
                    {ok, ReqR1} =
                        cowboy_req:reply(400, [], Error, ReqR),
                    {ok, ReqR1, State};
                {T, {ok, Start, R2}} ->
                    D = encode_reply(Start, T, R2),
                    {ContentType, ReqR1} = content_type(ReqR),
                    send(ContentType, D, ReqR1, State)
            end
    end.

encode_reply(Start, T, R2) ->
    R3 = [#{name => Name,
            resolution => Resolution,
            values => mmath_bin:to_list(Data),
            metadata => Mdata,
            type => <<"metrics">>}
          || #{name := Name,
               data := Data,
               type := metrics,
               metadata := Mdata,
               resolution := Resolution} <- R2],
    R4 = [#{name => Name,
            metadata => Mdata,
            values => [#{timestamp => Ts, event => E}
                       || {Ts, E} <- Data],
            type => <<"events">>}
          || #{name := Name,
               metadata := Mdata,
               data := Data,
               type := events} <- R2],
    D = #{start => Start,
          query_time => T,
          results => R3 ++ R4},
    case R2 of
        [#{type := graph,
           value := Graph} | _] ->
            maps:put(graph, Graph, D);
        _ ->
            D
    end.

content_type(Req) ->
    {ok, A, Req1} = cowboy_req:parse_header(<<"accept">>, Req),
    {content_type_(A), Req1}.

content_type_(undefined) ->
    json;
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
          msgpack:pack(D, [jsx, {allow_atom, pack}]), Req),
    {ok, Req1, State};
send(_, _D, Req, State) ->
    {ok, Req1} = cowboy_req:reply(415, Req),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.

build_opts(Req) ->
    O0 = case application:get_env(dalmatiner_frontend, log_slow) of
             {ok, true} ->
                 [{timeout, infinity}, log_slow_queries];
             _ ->
                 [{timeout, infinity}]
         end,
    {O1, R1} = case cowboy_req:qs_val(<<"debug">>, Req) of
                   {undefined, ReqX} ->
                       {O0, ReqX};
                   {<<>>, ReqX} ->
                       {[debug | O0], ReqX};
                   {Token, ReqX} ->
                       {[debug, {token, Token} | O0], ReqX}
               end,
    case cowboy_req:qs_val(<<"graph">>, R1) of
        {undefined, Rx1} ->
            {O1, Rx1};
        {_, Rx1} ->
            {[return_graph | O1], Rx1}
    end.
