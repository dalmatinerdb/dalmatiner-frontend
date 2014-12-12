-module(dqe_funnel).

-behaviour(dflow).

-export([init/1, start/4, emit/5, done/3]).

-record(state, {}).

init([SubQs])  ->
    {ok, #state{}, SubQs}.

start(_Start, _Count, _Parents, State) ->
    {ok, State}.

emit(_Child, Data, Resolution, _Ps, State) ->
    {emit, Data, Resolution, State}.

done({last, _}, _Parents, State) ->
    {done, State};

done(_, _Parents, State) ->
    {ok, State}.
