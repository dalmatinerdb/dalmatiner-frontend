-module(dqe_aggr1).
-behaviour(dflow_behaviour).

-export([init/1, start/4, emit/5, done/3]).

init([Aggr, SubQ]) ->
    {ok, Aggr, SubQ}.

start(_Start, _Count, _Parents, Aggr) ->
    {ok, Aggr}.

emit(_Child, Data, Resolution, _Parents, Aggr) ->
    {emit, mmath_aggr:Aggr(Data), Resolution, Aggr}.

done({last, _Child}, _Parents, Aggr) ->
    {done, Aggr}.
