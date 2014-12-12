-module(dqe_get).

-behaviour(dflow).

-export([init/1, start/2, emit/4, done/2]).

-record(state, {
          bucket :: binary(),
          metric :: binary(),
          chunk = 100000 :: pos_integer()
         }).

init([Bucket, Metric]) ->
    init([Bucket, Metric, 100000]);

init([Bucket, Metric, Chunk]) ->
    {ok, #state{bucket = Bucket, metric = Metric, chunk = Chunk}, []}.

start({_Start, 0}, State) ->
    {done, State};

start({Start, Count},
      State = #state{bucket = Bucket, metric = Metric, chunk = Chunk}) when
      Count >= Chunk ->
    {ok, Res, Data} = dalmatiner_connection:get(Bucket, Metric, Start, Chunk),
    %% We do a bit of cheating here this allows us to loop.
    dflow:start(self(), {Start + Chunk, Count - Chunk}),
    {emit, Data, Res, State};

start({Start, Count}, State = #state{bucket = Bucket, metric = Metric}) ->
    {ok, Res, Data} = dalmatiner_connection:get(Bucket, Metric, Start, Count),
    {done, Data, Res, State}.

emit(_Child, _Data, _Resolution, State) ->
    {ok, State}.

done(_, State) ->
    {done, State}.
