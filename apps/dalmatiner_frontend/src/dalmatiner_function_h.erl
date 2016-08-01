-module(dalmatiner_function_h).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-ignore_xref([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.


-dialyzer({no_opaque, handle/2}).
handle(Req, State) ->
    Req0 = cowboy_req:set_resp_header(
             <<"access-control-allow-origin">>, <<"*">>, Req),

    {ContentType, Req1} = dalmatiner_idx_handler:content_type(Req0),
    case ContentType of
        html ->
            F = fun (Socket, Transport) ->
                        File = code:priv_dir(dalmatiner_frontend) ++
                            "/static/functions.html",
                        Transport:sendfile(Socket, File)
                end,
            Req2 = cowboy_req:set_resp_body_fun(F, Req1),
            {ok, Req3} = cowboy_req:reply(200, Req2),
            {ok, Req3, State};
        _ ->
            Fs = funs_to_json(dqe_fun:list()),
            dalmatiner_idx_handler:send(ContentType, Fs, Req1, State)
    end.

funs_to_json(Fs) ->
    [to_json(F) || F <- Fs].

to_json(F = {{Name, Sig, List}, Return, _}) ->
    [
     {<<"name">>, Name},
     {<<"desc">>, dqe_fun:render(F)},
     {<<"help">>, maybe_help(F)},
     {<<"combiner_type">>, atom_to_binary(List, utf8)},
     {<<"signature">>, [atom_to_binary(S, utf8) || S <- Sig]},
     {<<"return">>, atom_to_binary(Return, utf8)}
    ].

maybe_help({_, _, M}) ->
    case erlang:function_exported(M, help, 0) of
        true ->
            M:help();
        false ->
            <<>>
    end.

terminate(_Reason, _Req, _State) ->
    ok.
