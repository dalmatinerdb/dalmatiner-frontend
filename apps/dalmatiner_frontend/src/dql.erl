-module(dql).
-export([prepare/1, parse/1, execute/1, unparse/1]).
-ignore_xref([prepare/1, parse/1, execute/1, unparse/1]).

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
    T1 = apply_times(T, Rms),
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
    {QQ, Start, Count, Rms, AliasesQ, MetricsQ}.

compute_se({between, S, E}, _Rms) when E > S->
    {S, E - S};
compute_se({between, S, E}, _Rms) ->
    {E, S - E};

compute_se({last, N}, Rms) ->
    _Now = {Mega, Sec, Micro} = now(),
    NowMs = ((Mega * 1000000  + Sec) * 1000000 + Micro) div 1000,
    %%UTC = calendar:now_to_universal_time(Now),
    %%UTCs = calendar:datetime_to_gregorian_seconds(UTC) - 62167219200,
    %%UTCms = (UTCs * 1000) + (NowMs rem 1000),
    RelativeNow = NowMs div Rms,
    {RelativeNow - N, N}.


preprocess_qry({named, N, Q}, Aliases, Metrics, Rms) ->
    {Q1, A1, M1} = preprocess_qry(Q, Aliases, Metrics, Rms),
    {{named, N, Q1}, A1, M1};
preprocess_qry({aggr, AggF, Q, T}, Aliases, Metrics, Rms) ->
    {Q1, A1, M1} = preprocess_qry(Q, Aliases, Metrics, Rms),
    {{aggr, AggF, Q1, T}, A1, M1};

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
        {Qs, S, C, Rms, A, M} ->
            {D, _} = lists:foldl(fun({named, Name, Q}, {RAcc, MAcc}) ->
                                         {{D, R}, M1} = execute(Q, S, C, Rms, A, MAcc),
                                         {[{Name, R, D} | RAcc], M1};
                                     (Q, {RAcc, MAcc}) ->
                                         {{D, R}, M1} = execute(Q, S, C, Rms, A, MAcc),
                                         {[{unparse(Q), R, D} | RAcc], M1}
                                 end, {[], M}, Qs),
            {ok, D};
        E ->
            E
    end.

execute({named, _, Q}, S, C, Rms, A, M) ->
    execute(Q, S, C, Rms, A, M);
execute({aggr, avg, Q, T}, S, C, Rms, A, M) ->
    {{D, Res}, M1} = execute(Q, S, C, Rms, A, M),
    T1 = apply_times(T, Rms * Res),
    {{mmath_aggr:avg(D, T1), T1}, M1};

execute({aggr, sum, Q, T}, S, C, Rms, A, M) ->
    {{D, Res}, M1} = execute(Q, S, C, Rms, A, M),
    T1 = apply_times(T, Rms * Res),
    {{mmath_aggr:sum(D, T1), T1}, M1};

execute({aggr, max, Q, T}, S, C, Rms, A, M) ->
    {{D, Res}, M1} = execute(Q, S, C, Rms, A, M),
    T1 = apply_times(T, Rms * Res),
    {{mmath_aggr:max(D, T1), T1}, M1};

execute({aggr, min, Q, T}, S, C, Rms, A, M) ->
    {{D, Res}, M1} = execute(Q, S, C, Rms, A, M),
    T1 = apply_times(T, Rms * Res),
    {{mmath_aggr:min(D, T1), T1}, M1};

execute({aggr, derivate, Q}, S, C, Rms, A, M) ->
    {{D, Res}, M1} = execute(Q, S, C, Rms, A, M),
    {{mmath_aggr:derivate(D), Res}, M1};

execute({get, BM = {B, M}}, S, C, _Rms, _A, Metrics) ->
    case gb_trees:get(BM, Metrics) of
        {get, N} when N =< 1 ->
            {ok, D} = dalmatiner_connection:get(B, M, S, C),
            {{D, 1}, gb_trees:delete(BM, Metrics)};
        {get, N} ->
            {ok, D} = dalmatiner_connection:get(B, M, S, C),
            {{D, 1}, gb_trees:update(BM, {get, {D, 1}, N - 1}, Metrics)};
        {get, D, N} when N =< 1 ->
            {D, gb_trees:delete(BM, Metrics)};
        {get, D, N} ->
            {D, gb_trees:update(BM, {get, D, N - 1}, Metrics)}
    end;

execute({var, V}, S, C, Rms, A, M) ->
    execute(gb_trees:get(V, A), S, C, Rms, A, M).

unparse(L) when is_list(L) ->
    <<_, _, R/binary>> = << <<", ", (unparse(Q))/binary>> || Q <- L >>,
    R;
unparse({select, Q, A, T, R}) ->
    <<"SELECT ", (unparse(Q))/binary, " FROM ", (unparse(A))/binary, " ",
      (unparse(T))/binary, " IN ", (unparse(R))/binary>>;
unparse({select, Q, T, R}) ->
    <<"SELECT ", (unparse(Q))/binary, " ", (unparse(T))/binary, " IN ",
      (unparse(R))/binary>>;
unparse({last, Q}) ->
    <<"LAST ", (unparse(Q))/binary>>;
unparse({var, V}) ->
    V;

unparse({alias, A, V}) ->
    <<(unparse(V))/binary, " AS ", A/binary>>;
unparse({get, {B, M}}) ->
    <<M/binary, " BUCKET ", B/binary>>;
unparse(N) when is_integer(N)->
    <<(integer_to_binary(N))/binary>>;
unparse({time, N, ms}) ->
    <<(integer_to_binary(N))/binary, "ms">>;
unparse({time, N, s}) ->
    <<(integer_to_binary(N))/binary, "s">>;
unparse({time, N, m}) ->
    <<(integer_to_binary(N))/binary, "m">>;
unparse({time, N, h}) ->
    <<(integer_to_binary(N))/binary, "h">>;
unparse({time, N, d}) ->
    <<(integer_to_binary(N))/binary, "d">>;
unparse({time, N, w}) ->
    <<(integer_to_binary(N))/binary, "w">>;
unparse({aggr, Fun, Q}) ->
    Funs = list_to_binary(atom_to_list(Fun)),
    Qs = unparse(Q),
    <<Funs/binary, "(", Qs/binary, ")">>;
unparse({aggr, Fun, Q, T}) ->
    Funs = list_to_binary(atom_to_list(Fun)),
    Qs = unparse(Q),
    Ts = unparse(T),
    <<Funs/binary, "(", Qs/binary, ", ", Ts/binary, ")">>.

apply_times({last, L}, R) ->
    {last, apply_times(L, R)};

apply_times({between, S, E}, R) ->
    {between, apply_times(S, R), apply_times(E, R)};

apply_times(N, _) when is_integer(N) ->
    N;

apply_times(now, R) ->
    _Now = {Mega, Sec, Micro} = now(),
    NowMs = ((Mega * 1000000  + Sec) * 1000000 + Micro) div 1000,
    NowMs div R;

apply_times({ago, T}, R) ->
    _Now = {Mega, Sec, Micro} = now(),
    NowMs = ((Mega * 1000000  + Sec) * 1000000 + Micro) div 1000,
    (NowMs - to_ms(T)) div R;

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
