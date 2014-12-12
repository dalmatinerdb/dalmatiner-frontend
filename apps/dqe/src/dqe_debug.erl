-module(dqe_debug).

-behaviour(dflow).

-export([init/1, start/4, emit/5, done/3]).

-record(state, {start = now()}).

init([SubQ]) when not is_list(SubQ) ->
    {ok, #state{}, SubQ}.

start(_Start, _Count, _Parents, State) ->
    {ok, State}.

emit(Child, {Name, Data}, Resolution, _Ps, State) ->
    io:format("[~p:~s] ~p~n", [Child, Name, mmath_bin:to_list(Data)]),
    {emit, {Name, Data}, Resolution, State};

emit(Child, Data, Resolution, _Ps, State) ->
    io:format("[~p] ~p~n", [Child, mmath_bin:to_list(Data)]),
    {emit, Data, Resolution, State}.

done(Child, _Parents, State = #state{start = Start}) ->
    Diff  = round(timer:now_diff(now(), Start) / 1000),
    io:format("[~p] Finished after ~pms.~n", [Child, Diff]),
    {done, State}.
