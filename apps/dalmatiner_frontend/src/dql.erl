-module(dql).
-export([prepare/1, parse/1, execute/1]).
-ignore_xref([prepare/1, parse/1, execute/1]).

parse(S) ->
    {ok, L, _} = dql_lexer:string(S),
    dql_parser:parse(L).


prepare(S) ->
    {Qs, Aliases, T, R} = case parse(S) of
                           {ok, {select, Qx, Aliasesx, Tx, Rx}} ->
                               {Qx, Aliasesx, Tx, Rx};
                           {ok, {select, Qx, Tx, Rx}} ->
                               {Qx, [], Tx, Rx}
                       end,
    Rms = to_ms(R),
    io:format("Q: ~p~n", [{Qs, Aliases, T, R}]),
    T1 = apply_times(T, Rms),
    io:format("T -> T1: ~p -> ~p~n", [T, T1]),
    {_AF, AliasesF, MetricsF} =
        lists:foldl(fun({alias, Alias, Resolution}, {QAcc, AAcc, MAcc}) ->
                            {Q1, A1, M1} = preprocess_qry(Resolution, AAcc, MAcc, Rms),
                            {[Q1 | QAcc], gb_trees:enter(Alias, Resolution, A1), M1}
                    end, {[], gb_trees:empty(), gb_trees:empty()}, Aliases),
    {QQ, AliasesQ, MetricsQ} =
        lists:foldl(fun(Q, {QAcc, AAcc, MAcc}) ->
                            {Q1, A1, M1} = preprocess_qry(Q, AAcc, MAcc, Rms),
                            {[Q1 | QAcc] , A1, M1}
                    end, {[], AliasesF, MetricsF}, Qs),
    {Start, Count} = compute_se(T1, Rms),
    io:format("range: ~p + ~p ~n", [Start, Count]),
    {QQ, Start, Count, AliasesQ, MetricsQ}.

compute_se({last, N}, Rms) ->
    _Now = {Mega, Sec, Micro} = now(),
    NowMs = ((Mega * 1000000  + Sec) * 1000000 + Micro) div 1000,
    %%UTC = calendar:now_to_universal_time(Now),
    %%UTCs = calendar:datetime_to_gregorian_seconds(UTC) - 62167219200,
    %%UTCms = (UTCs * 1000) + (NowMs rem 1000),
    RelativeNow = NowMs div Rms,
    {RelativeNow - N, N}.


preprocess_qry({aggr, AggF, Q, T}, Aliases, Metrics, Rms) ->
    {Q1, A1, M1} = preprocess_qry(Q, Aliases, Metrics, Rms),
    {{aggr, AggF, Q1, apply_times(T, Rms)}, A1, M1};

preprocess_qry({aggr, AggF, Q}, Aliases, Metrics, Rms) ->
    {Q1, A1, M1} = preprocess_qry(Q, Aliases, Metrics, Rms),
    {{aggr, AggF, Q1}, A1, M1};

preprocess_qry({maggr, AggF, Qs}, Aliases, Metrics, Rms) ->
    {Q1, A1, M1} =
        lists:foldl(fun(SubQ, {QAcc, AAcc, MAcc}) ->
                            {Qa1, Aa1, Ma1} = preprocess_qry(SubQ, AAcc, MAcc, Rms),
                            {[Qa1 | QAcc], Aa1, Ma1}
                    end, {[], Aliases, Metrics}, Qs),
    {{maggr, AggF, lists:reverse(Q1)}, A1, M1};

preprocess_qry({get, BM}, Aliases, Metrics, _Rms) ->
    Metrics1 = case gb_trees:lookup(BM, Metrics) of
                   none ->
                       gb_trees:insert(BM, {get, 1}, Metrics);
                   {value, {get, N}} ->
                       gb_trees:update(BM, {get, N + 1}, Metrics)
               end,
    {{get, BM}, Aliases, Metrics1};

preprocess_qry({mget, BM}, Aliases, Metrics, _Rms) ->
    Metrics1 = case gb_trees:lookup(BM, Metrics) of
                   none ->
                       gb_trees:insert(BM, {mget, 1}, Metrics);
                   {value, {get, N}} ->
                       gb_trees:update(BM, {mget, N + 1}, Metrics)
               end,
    {{mget, BM}, Aliases, Metrics1};

preprocess_qry(Q, A, M, _) ->
    {Q, A, M}.

execute(Qry) ->
    {Qs, S, C, A, M} = prepare(Qry),
    {D, _} = lists:foldl(fun(Q, {RAcc, MAcc}) ->
                                 {R, M1} = execute(Q, S, C, A, MAcc),
                                 {[R | RAcc], M1}
                         end, {[], M}, Qs),
    D.

execute({aggr, avg, Q, T}, S, C, A, M) ->
    {D, M1} = execute(Q, S, C, A, M),
    {mmath_aggr:avg(D, T), M1};

execute({aggr, sum, Q, T}, S, C, A, M) ->
    {D, M1} = execute(Q, S, C, A, M),
    {mmath_aggr:sum(D, T), M1};

execute({aggr, max, Q, T}, S, C, A, M) ->
    {D, M1} = execute(Q, S, C, A, M),
    {mmath_aggr:max(D, T), M1};

execute({aggr, min, Q, T}, S, C, A, M) ->
    {D, M1} = execute(Q, S, C, A, M),
    {mmath_aggr:min(D, T), M1};

execute({get, BM = {B, M}}, S, C, _A, Metrics) ->
    case gb_trees:get(BM, Metrics) of
        {get, 1} ->
            {ok, D} = dalmatiner_connection:get(B, M, S, C),
            {D, gb_trees:delete(BM, Metrics)};
        {get, M} ->
            {ok, D} = dalmatiner_connection:get(B, M, S, C),
            {D, gb_trees:update(BM, {get, D, M - 1}, Metrics)};
        {get, D, 1} ->
            {D, gb_trees:delete(BM, Metrics)};
        {get, D, M} ->
            {D, gb_trees:update(BM, {get, D, M - 1}, Metrics)}
    end;

execute({var, V}, S, C, A, M) ->
    execute(gb_trees:get(V, A), S, C, A, M).

apply_times({last, L}, R) ->
    {last, apply_times(L, R)};

apply_times(N, _) when is_integer(N) ->
    N;
apply_times(T, R) ->
    to_ms(T) div R.

to_ms({time, N, ms}) ->
    N;
to_ms({time, N, s}) ->
    N*1000;
to_ms({time, N, m}) ->
    N*1000*60;
to_ms({time, N, h}) ->
    N*1000*60*60;
to_ms({time, N, d}) ->
    N*1000*60*60*24;
to_ms({time, N, w}) ->
    N*1000*60*60*24*7.

