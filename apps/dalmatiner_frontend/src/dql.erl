-module(dql).
-export([prepare/1, parse/1, execute/1]).
-ignore_xref([prepare/1, parse/1, execute/1]).

parse(S) when is_binary(S)->
    parse(binary_to_list(S));
parse(S) ->
    case dql_lexer:string(S) of
        {error,{Line,dql_lexer,E},1} ->
            {error, io_lib:format("Error in line ~p: ~p", [Line, E])};
        {ok, L, _} ->
            case dql_parser:parse(L) of
                {error, {Line, dql_parser, E}} ->
                    {error, io_lib:format("Error in line ~p: ~s", [Line, E])};
                R ->
                    R
            end
    end.

prepare(S) ->
    case parse(S) of
        {ok, {select, Qx, Aliasesx, Tx, Rx}} ->
            prepare(Qx, Aliasesx, Tx, Rx);
        {ok, {select, Qx, Tx, Rx}} ->
            prepare(Qx, [], Tx, Rx);
        E ->
            E
    end.

prepare(Qs, Aliases, T, R) ->
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
                       gb_trees:insert(BM, {get, 0}, Metrics);
                   {value, {get, N}} ->
                       gb_trees:update(BM, {get, N + 1}, Metrics)
               end,
    {{get, BM}, Aliases, Metrics1};

preprocess_qry({mget, BM}, Aliases, Metrics, _Rms) ->
    Metrics1 = case gb_trees:lookup(BM, Metrics) of
                   none ->
                       gb_trees:insert(BM, {mget, 0}, Metrics);
                   {value, {get, N}} ->
                       gb_trees:update(BM, {mget, N + 1}, Metrics)
               end,
    {{mget, BM}, Aliases, Metrics1};

preprocess_qry({var, V}, Aliases, Metrics, _Rms) ->
    Metrics1 = case gb_trees:lookup(V, Aliases) of
                   none ->
                       io:format("yeeek: ~p(~p) ~p!~n", [Aliases, V, Metrics]),
                       Metrics;
                   {value, {get, BM}} ->
                       case gb_trees:lookup(BM, Metrics) of
                           none ->
                               gb_trees:insert(BM, {get, 1}, Metrics);
                           {value, {get, N}} ->
                               gb_trees:update(BM, {get, N + 1}, Metrics)
                       end
               end,
    {{var, V}, Aliases, Metrics1};

preprocess_qry(Q, A, M, _) ->
    {Q, A, M}.

execute(Qry) ->
    case prepare(Qry) of
        {Qs, S, C, A, M} ->
            {D, _} = lists:foldl(fun(Q, {RAcc, MAcc}) ->
                                         {R, M1} = execute(Q, S, C, A, MAcc),
                                         {[R | RAcc], M1}
                                 end, {[], M}, Qs),
            {ok, D};
        E ->
            E
    end.

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
        {get, N} when N =< 1 ->
            {ok, D} = dalmatiner_connection:get(B, M, S, C),
            {D, gb_trees:delete(BM, Metrics)};
        {get, N} ->
            {ok, D} = dalmatiner_connection:get(B, M, S, C),
            {D, gb_trees:update(BM, {get, D, N - 1}, Metrics)};
        {get, D, N} when N =< 1 ->
            {D, gb_trees:delete(BM, Metrics)};
        {get, D, N} ->
            {D, gb_trees:update(BM, {get, D, N - 1}, Metrics)}
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
