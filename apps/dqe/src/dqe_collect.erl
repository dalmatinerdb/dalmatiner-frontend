-module(dqe_collect).

-behaviour(dflow).

-export([init/1, start/4, emit/5, done/3]).

-record(state, {
          acc :: binary(),
          resolution :: pos_integer()
         }).

init([SubQ]) when not is_list(SubQ) ->
    {ok, #state{}, SubQ}.

start(_Start, _Count, _Parents, State) ->
    {ok, State}.

%% When we get the first data we can calculate both the applied
%% time and the upwards resolution.
emit(_C, Data, Resolution, _Ps, State = #state{resolution = undefined}) ->
    {ok, State#state{resolution = Resolution, acc = Data}};

emit(_C, {Name, Data}, _R, _Ps, State = #state{acc = {Name, Acc}}) ->
    {ok, State#state{acc = {Name, <<Acc/binary, Data/binary>>}}};

emit(_C, Data, _R, _Ps, State = #state{acc = Acc}) ->
    {ok, State#state{acc = <<Acc/binary, Data/binary>>}}.

done(_Child, _Parents, State = #state{resolution = undefined}) ->
    {done, State};

done(_Child, _Parents, State = #state{resolution = Resolution, acc = Acc}) ->
    {done, Acc, Resolution, State#state{acc = <<>>}}.


