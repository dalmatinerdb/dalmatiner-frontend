-module(dqe_funnel).

-behaviour(dflow).

-export([init/1, start/2, emit/4, done/2]).

-record(state, {}).

init([SubQs])  ->
    {ok, #state{}, SubQs}.

start({_Start, _Count}, State) ->
    {ok, State}.

emit(_Child, Data, Resolution, State) ->
    {emit, Data, Resolution, State}.

done({last, _}, State) ->
    {done, State};

done(_, State) ->
    {ok, State}.
