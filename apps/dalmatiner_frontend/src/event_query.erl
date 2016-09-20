%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2016, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 20 Sep 2016 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(event_query).

-export([parse/1]).

parse(S) when is_binary(S) ->
    parse(binary_to_list(S));
parse(S) ->
    {ok, L, _} = event_query_lexer:string(S),
    {ok, T} = event_query_parser:parse(L),
    flatten(T).

flatten({'and', A, B}) ->
    lists:flatten([flatten(A), flatten(B)]);
flatten({'or', A, B}) ->
    [{'or', flatten(A), flatten(B)}];
flatten({'not', A}) ->
    [{'not', flatten(A)}];
flatten(O) ->
    lists:flatten([O]).
