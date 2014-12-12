-module(dqe_name).

-behaviour(dflow).

-export([init/1, start/4, emit/5, done/3]).

-record(state, {
          name
         }).

init([Name, SubQ]) ->
    {ok, #state{name = Name}, SubQ}.

start(_Start, _Count, _Parents, State) ->
    {ok, State}.

emit(_C, Data, Resolution, _Ps, State = #state{name = Name}) ->
    {emit, {Name, Data}, Resolution, State}.

done(_Child, _Parents, State) ->
    {done, State}.


