%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(metric_handler).

-export([init/3, handle/2, terminate/3]).

-ignore_xref([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    handle(Method, Req2, State).

handle(<<"GET">>, Req, State) ->
    case cowboy_req:qs_val(<<"q">>, Req) of
        {undefined, Req2} ->
            Ms = metric_connection:list(),
            {ok, Req3} =
                cowboy_req:reply(
                  200, [{<<"content-type">>, <<"application/json">>}],
                  jsx:encode(Ms), Req2),
            {ok, Req3, State};
        {Q, Req2} ->
            R1 = mmath_bin:to_list(metric_qry_parser:run(Q)),
            {ok, Req3} =
                cowboy_req:reply(
                  200, [{<<"content-type">>, <<"application/json">>}],
                  jsx:encode(R1), Req2),
            {ok, Req3, State}
    end.

terminate(_Reason, _Req, _State) ->
	ok.
