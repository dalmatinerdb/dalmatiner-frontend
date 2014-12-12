-module(dqe_math).
-behaviour(dflow_behaviour).

-export([init/1, start/4, emit/5, done/3]).
-record(state, {
          op :: atom(),
          arg :: number()
         }).

init([Op, SubQ, Arg]) ->
    {ok, #state{op = Op, arg = Arg}, SubQ}.

start(_Start, _Count, _Parents, State) ->
    {ok, State}.

emit(_Child, Data, Resolution, _Parents, State = #state{op = Op, arg = Arg}) ->
    {emit, mmath_aggr:Op(Data, Arg), Resolution, State}.

done({last, _Child}, _Parents, Aggr) ->
    {done, Aggr}.
