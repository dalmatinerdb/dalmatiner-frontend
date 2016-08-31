-module(dalmatiner_cors_m).
-behaviour(cowboy_middleware).

-export([execute/2]).

%%
%% CORS middleware
%% =====================================

-spec execute(cowboy_req:req(), [{atom(), any()}]) ->
                     {ok, cowboy_req:req(), [{atom(), any()}]} |
                     {halt, cowboy_req:req()}.
execute(Req, Env) ->
    R1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
                                    <<"*">>, Req),
    case cowboy_req:method(R1) of 
        {<<"OPTIONS">>, R2} ->
            R3 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>,
                                            <<"GET">>, R2),
            R4 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>,
                                            <<"Authorization">>, R3),
            R5 = cowboy_req:set_resp_header(<<"access-control-max-age">>,
                                            <<"3600">>, R4),
            {ok, R6} = cowboy_req:reply(200, R5),
            {halt, R6};
        {_, R2} ->
            {ok, R2, Env}
    end.
