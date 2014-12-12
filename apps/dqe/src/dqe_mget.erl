-module(dqe_mget).

-behaviour(dflow).

-export([init/1, start/4, emit/5, done/3]).

-record(state, {
          acc = gb_trees:empty(),
          aggr :: atom(),
          count,
          term_for_child = dict:new()
         }).

init([Aggr, SubQs]) ->
    SubQs1 = [{make_ref(), SubQ} || SubQ <- SubQs],
    {ok, #state{aggr = Aggr, count = length(SubQs1)}, SubQs1}.

start(_Start, _Count, _Parents, State) ->
    {ok, State}.

emit(Child, Data, Resolution, _Parents,
     State = #state{term_for_child = TFC, count = Count, acc = Tree,
                    aggr = Aggr}) ->
    TFC1 = dict:update_counter(Child, 1, TFC),
    Term = dict:fetch(Child, TFC1),
    Tree1 = add_to_tree(Term, Data, Tree),
    case shrink_tree(Tree1, Count, <<>>) of
        {Tree2, <<>>} ->
            {ok, State#state{acc = Tree2, term_for_child = TFC}};
        {Tree2, Data1} ->
            Data2 = case Aggr of
                        sum ->
                            Data1;
                        avg ->
                            mmath_aggr:scale(Data1, 1/Count)
                    end,
            {emit, Data2, Resolution,
             State#state{acc = Tree2, term_for_child = TFC}}
    end.

done({last, _Child}, _Parents, State) ->
    {done, State};

done(_, _Parents, State) ->
    {ok, State}.

add_to_tree(Term, Data, Tree) ->
    case gb_trees:lookup(Term, Tree) of
        none ->
            gb_trees:insert(Term, {Data, 1}, Tree);
        {value, {Sum, Count}} ->
            Sum1 = mmath_comb:sum([Sum, Data]),
            gb_trees:update(Term, {Sum1, Count+1}, Tree)
    end.

shrink_tree(Tree, Count, Acc) ->
    case gb_trees:is_empty(Tree) of
        true ->
            {Tree, Acc};
        _ ->
            case gb_trees:smallest(Tree) of
                {Term0, {Data, FirstCount}} when FirstCount =:= Count ->
                    Tree1 = gb_trees:delete(Term0, Tree),
                    shrink_tree(Tree1, Count, <<Acc/binary, Data/binary>>);
                _ ->
                    {Tree, Acc}
            end
    end.
