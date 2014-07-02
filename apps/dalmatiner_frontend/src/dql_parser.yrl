Nonterminals
funs fun selector select timeframe caggr_selectors caggr_selector aliases alias resolution int_or_time mb fune name.

Terminals '(' ')' ','
metric glob_metric caggr aggr integer kw_bucket kw_select kw_last kw_as kw_from kw_in derivate time.

Rootsymbol select.

select -> kw_select funs timeframe : {select, '$2', '$3', {time, 1, s}}.
select -> kw_select funs kw_from aliases timeframe : {select, '$2', '$4', '$5', {time, 1, s}}.
select -> kw_select funs timeframe resolution : {select, '$2', '$3', '$4'}.
select -> kw_select funs kw_from aliases timeframe resolution : {select, '$2', '$4', '$5', '$6'}.

funs -> fune : ['$1'].
funs -> fune ',' funs : ['$1'] ++ '$3'.

fune -> selector kw_as name : {named, '$3', '$1'}.
fune -> name kw_as name : {named, '$3', {var, '$1'}}.
fune -> fun kw_as name : {named, '$3', '$1'}.
fune -> selector : '$1'.
fune -> name : {var, '$1'}.
fune -> fun : '$1'.

alias -> selector kw_as name : {alias, '$3', '$1'}.

aliases -> alias : ['$1'].

aliases -> alias ',' aliases  : ['$1'] ++ '$3'.

name -> metric : unwrap('$1').
name -> aggr : list_to_binary(atom_to_list(unwrap('$1'))).
name -> caggr : list_to_binary(atom_to_list(unwrap('$1'))).

fun -> derivate '(' fun ')' : {aggr, derivate, '$3'}.
fun -> derivate '(' selector ')' : {aggr, derivate, '$3'}.
fun -> aggr '(' fun ',' int_or_time ')' : {aggr, unwrap('$1'), '$3', '$5'}.
fun -> aggr '(' name ',' int_or_time ')' : {aggr, unwrap('$1'), {var, '$3'}, '$5'}.
fun -> aggr '(' selector ',' int_or_time ')' : {aggr, unwrap('$1'), '$3', '$5'}.
fun -> caggr '(' fun ',' int_or_time ')' : {aggr, unwrap('$1'), '$3', '$5'}.
fun -> caggr '(' name ',' int_or_time ')' : {aggr, unwrap('$1'), {var, '$3'}, '$5'}.
fun -> caggr '(' selector ',' int_or_time ')' : {aggr, unwrap('$1'), '$3', '$5'}.


selector -> mb : {get, '$1'}.
selector -> caggr '(' caggr_selectors ')': {maggr, unwrap('$1'), '$3'}.

caggr_selectors -> selector : ['$1'].
caggr_selectors -> caggr_selector : ['$1'].
caggr_selectors -> selector ',' caggr_selectors : ['$1'] ++ '$3'.
caggr_selectors -> caggr_selector ',' caggr_selectors : ['$1'] ++ '$3'.


caggr_selector -> glob_metric kw_bucket name: {mget, {'$3', unwrap('$1')}}.

timeframe -> kw_last int_or_time: {last, '$2'}.

resolution -> kw_in timeframe : '$2'.

mb -> name kw_bucket name : {'$3', '$1'}.

timeframe -> integer time : {time, unwrap('$1'), unwrap('$2')}.
timeframe -> time : {time, 1, unwrap('$1')}.

int_or_time -> timeframe : '$1'.
int_or_time -> integer : unwrap('$1').

Erlang code.

-ignore_xref([format_error/1, parse_and_scan/1]).

unwrap({_,_,V}) -> V.