-module(dqe_math).
-behaviour(dflow).

-export([init/1, start/2, emit/3, done/2]).
-record(state, {
          op :: atom(),
          arg :: number()
         }).

init([Op, SubQ, Arg]) ->
    {ok, #state{op = Op, arg = Arg}, SubQ}.

start({_Start, _Count}, State) ->
    {ok, State}.

emit(_Child, {Data, Resolution}, State = #state{op = Op, arg = Arg}) ->
    {emit, {mmath_aggr:Op(Data, Arg), Resolution}, State}.

done({last, _Child}, Aggr) ->
    {done, Aggr}.
