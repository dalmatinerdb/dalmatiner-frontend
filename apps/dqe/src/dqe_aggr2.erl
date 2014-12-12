-module(dqe_aggr2).

-behaviour(dflow).

-export([init/1, start/4, emit/5, done/3]).

-record(state, {
          aggr :: atom(),
          time :: pos_integer(),
          acc = <<>> :: binary(),
          resolution :: pos_integer()
         }).

init([Aggr, SubQ, Time]) ->
    {ok, #state{aggr = Aggr, time = Time}, SubQ}.

start(_Start, _Count, _Parents, State) ->
    {ok, State}.

%% When we get the first data we can calculate both the applied
%% time and the upwards resolution.
emit(Child, Data, Resolution, Parents,
     State = #state{resolution = undefined, time = Time}) ->
    Time1 = dqe_time:apply_times(Time, Resolution),
    emit(Child, Data, Resolution, Parents,
         State#state{resolution = Time1 * Resolution, time = Time1});

emit(_Child, Data, _R, _Parents,
     State = #state{aggr = Aggr, time = Time, acc = Acc}) ->
    case execute(Aggr, <<Data/binary, Acc/binary>>, Time, <<>>) of
        {Acc1, <<>>} ->
            {ok, State#state{acc = Acc1}};
        {Acc2, AccEmit} ->
            {emit, AccEmit, State#state.resolution, State#state{acc = Acc2}}
    end.


done(_Child, _Parents, State = #state{acc = <<>>}) ->
    {done, State};

done(_Child, _Parents, State = #state{aggr = Aggr, time = Time, acc = Acc}) ->
    Data = mmath_aggr:Aggr(Acc, Time),
    {done, Data, State#state.resolution, State#state{acc = <<>>}}.


execute(Aggr, Acc, T1, AccEmit) when byte_size(Acc) >= T1 * 9 ->
    MinSize = T1 * 9,
    <<Data:MinSize/binary, Acc1/binary>> = Acc,
    Result = mmath_aggr:Aggr(Data, T1),
    execute(Aggr, Acc1, T1, <<AccEmit/binary, Result/binary>>);

execute(_, Acc, _, AccEmit) ->
    {Acc, AccEmit}.
