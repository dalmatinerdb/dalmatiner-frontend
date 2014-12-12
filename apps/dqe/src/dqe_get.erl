-module(dqe_get).

-behaviour(dflow).

-export([init/1, start/4, emit/5, done/3]).

-record(state, {
          bucket :: binary(),
          metric :: binary(),
          chunk = 100000 :: pos_integer()
         }).

init([Bucket, Metric]) ->
    init([Bucket, Metric, 100000]);

init([Bucket, Metric, Chunk]) ->
    {ok, #state{bucket = Bucket, metric = Metric, chunk = Chunk}, []}.

start(_Start, 0, _Parents, State) ->
    {done, State};

start(Start, Count, Parents,
      State = #state{bucket = Bucket, metric = Metric, chunk = Chunk}) when
      Count >= Chunk ->
    {ok, Res, Data} = dalmatiner_connection:get(Bucket, Metric, Start, Chunk),
    dflow_behaviour:emit(Parents, Data, Res),
    start(Start + Chunk, Count - Chunk, Parents, State);

start(Start, Count, _, State = #state{bucket = Bucket, metric = Metric}) ->
    {ok, Res, Data} = dalmatiner_connection:get(Bucket, Metric, Start, Count),
    {done, Data, Res, State}.

emit(_Child, _Data, _Resolution, _Parents, State) ->
    {ok, State}.

done(_, _Parents, State) ->
    {done, State}.
