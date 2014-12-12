-module(dqe_name).

-behaviour(dflow).

-export([init/1, start/2, emit/4, done/2]).

-record(state, {
          name
         }).

init([Name, SubQ]) ->
    {ok, #state{name = Name}, SubQ}.

start({_Start, _Count}, State) ->
    {ok, State}.

emit(_C, Data, Resolution, State = #state{name = Name}) ->
    {emit, {Name, Data}, Resolution, State}.

done(_Child, State) ->
    {done, State}.


