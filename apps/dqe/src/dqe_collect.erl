-module(dqe_collect).

-behaviour(dflow).

-export([init/1, start/2, emit/3, done/2]).

-record(state, {
          acc :: binary(),
          resolution :: pos_integer()
         }).

init([SubQ]) when not is_list(SubQ) ->
    {ok, #state{}, SubQ}.

start({_Start, _Count}, State) ->
    {ok, State}.

%% When we get the first data we can calculate both the applied
%% time and the upwards resolution.
emit(_C, {Name, Data, Resolution}, State = #state{resolution = undefined}) ->
    {ok, State#state{resolution = Resolution, acc = {Name, Data}}};

emit(_C, {Data, Resolution}, State = #state{resolution = undefined}) ->
    {ok, State#state{resolution = Resolution, acc = Data}};


emit(_C, {Name, Data, _R}, State = #state{acc = {Name, Acc}}) ->
    {ok, State#state{acc = {Name, <<Acc/binary, Data/binary>>}}};

emit(_C, {Data, _R}, State = #state{acc = Acc}) ->
    {ok, State#state{acc = <<Acc/binary, Data/binary>>}}.

done(_Child, State = #state{resolution = undefined}) ->
    {done, State};

done(_Child, State = #state{resolution = Resolution, acc = {Name, Acc}}) ->
    {done, {Name, Acc, Resolution}, State#state{acc = <<>>}};

done(_Child, State = #state{resolution = Resolution, acc = Acc}) ->
    {done, {Acc, Resolution}, State#state{acc = <<>>}}.
