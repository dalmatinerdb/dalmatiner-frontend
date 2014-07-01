%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(dalmatiner_handler).

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
            case cowboy_req:qs_val(<<"b">>, Req2) of
                {undefined, Req3} ->
                    {ok, Bs} = dalmatiner_connection:list(),
                    {ok, Req4} =
                        cowboy_req:reply(
                          200, [{<<"content-type">>, <<"application/x-msgpack">>}],
                          msgpack:pack(Bs), Req3),
                    {ok, Req4, State};
                {B, Req3} ->
                    {ok, Ms} = dalmatiner_connection:list(B),
                    {ok, Req4} =
                        cowboy_req:reply(
                          200, [{<<"content-type">>, <<"application/x-msgpack">>}],
                          msgpack:pack(Ms), Req3),
                    {ok, Req4, State}
                end;
        {Q, Req2} ->
            {T, R} = timer:tc(fun() -> mmath_bin:to_list(dql:execute(Q)) end),
            case R of
                {error, E} ->
                    {ok, Req3} =
                        cowboy_req:reply(
                          400, [],
                          list_to_binary(E)),
                    {ok, Req3, State};
                {ok, R1} ->
                    D = [{<<"t">>, T},
                         {<<"d">>, R1}],
                    {ok, Req3} =
                        cowboy_req:reply(
                          200, [{<<"content-type">>, <<"application/x-msgpack">>}],
                          msgpack:pack(D, [jsx]), Req2),
                    {ok, Req3, State}
            end
    end.

terminate(_Reason, _Req, _State) ->
	ok.
