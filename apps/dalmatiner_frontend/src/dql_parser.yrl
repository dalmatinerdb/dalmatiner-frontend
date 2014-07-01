Nonterminals
funs fun selector select timeframe caggr_selectors caggr_selector aliases alias resolution int_or_time mb.

Terminals '(' ')' ','
metric glob_metric caggr aggr integer kw_bucket kw_select kw_last kw_as kw_from kw_in derivate time.

Rootsymbol select.

select -> kw_select funs timeframe : {select, '$2', '$3', {time, 1, s}}.
select -> kw_select funs kw_from aliases timeframe : {select, '$2', '$4', '$5', {time, 1, s}}.
select -> kw_select funs timeframe resolution : {select, '$2', '$3', '$4'}.
select -> kw_select funs kw_from aliases timeframe resolution : {select, '$2', '$4', '$5', '$6'}.

funs -> selector : ['$1'].
funs -> metric : [{var, unwrap('$1')}].
funs -> fun : ['$1'].
funs -> fun ',' funs : ['$1'] ++ '$3'.

alias -> selector kw_as metric : {alias, unwrap('$3'), '$1'}.

aliases -> alias : ['$1'].

aliases -> alias ',' aliases  : ['$1'] ++ '$3'.

fun -> derivate '(' fun ')' : {aggr, derivate, '$3'}.
fun -> derivate '(' selector ')' : {aggr, derivate, '$3'}.
fun -> aggr '(' fun ',' int_or_time ')' : {aggr, unwrap('$1'), '$3', '$5'}.
fun -> aggr '(' metric ',' int_or_time ')' : {aggr, unwrap('$1'), {var, unwrap('$3')}, '$5'}.
fun -> aggr '(' selector ',' int_or_time ')' : {aggr, unwrap('$1'), '$3', '$5'}.
fun -> caggr '(' fun ',' int_or_time ')' : {aggr, unwrap('$1'), '$3', '$5'}.
fun -> caggr '(' metric ',' int_or_time ')' : {aggr, unwrap('$1'), {var, unwrap('$3')}, '$5'}.
fun -> caggr '(' selector ',' int_or_time ')' : {aggr, unwrap('$1'), '$3', '$5'}.


selector -> mb : {get, '$1'}.
selector -> caggr '(' caggr_selectors ')': {maggr, unwrap('$1'), '$3'}.

caggr_selectors -> selector : ['$1'].
caggr_selectors -> caggr_selector : ['$1'].
caggr_selectors -> selector ',' caggr_selectors : ['$1'] ++ '$3'.
caggr_selectors -> caggr_selector ',' caggr_selectors : ['$1'] ++ '$3'.


caggr_selector -> glob_metric kw_bucket metric: {mget, {unwrap('$3'), unwrap('$1')}}.

timeframe -> kw_last int_or_time: {last, '$2'}.

resolution -> kw_in timeframe : '$2'.

mb -> metric kw_bucket metric : {unwrap('$3'), unwrap('$1')}.

timeframe -> integer time : {time, unwrap('$1'), unwrap('$2')}.
timeframe -> time : {time, 1, unwrap('$1')}.

int_or_time -> timeframe : '$1'.
int_or_time -> integer : unwrap('$1').

Erlang code.

-ignore_xref([format_error/1, parse_and_scan/1]).

unwrap({_,_,V}) -> V.