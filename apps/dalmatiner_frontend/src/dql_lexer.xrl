% DQL lexer - based on the example from https://github.com/relops/leex_yecc_example/blob/master/src/selector_lexer.xrl

Definitions.

D       = [0-9]
S       = [A-Za-z][A-Za-z0-9.-]*
GS      = [A-Za-z*][A-Za-z0-9*.-]*
WS      = ([\000-\s]|%.*)
AGGR    = (min|max)
CAGGR   = (avg|sum)
TIME    = (ms|s|m|h|d|w)
SELECT  = [Ss][Ee][Ll][Ee][Cc][Tt]
BUCKET  = [Bb][Uu][Cc][Kk][Ee][Tt]
LAST    = [Ll][Aa][Ss][Tt]
AS      = [Aa][Ss]
FROM    = [Ff][Rr][Oo][Mm]
LIKE    = [Ll][Ii][Kk][Ee]
BETWEEN = [Bb][Ee][Tt][Ww][Ee][Ee][Nn]
IN      = [Ii][Nn]
NOW     = [Nn][Oo][Ww]
AGO     = [Aa][Gg][Oo]
AND     = [Aa][Nn][Dd]

Rules.
{SELECT}    :   {token, {kw_select,     TokenLine}}.
{BUCKET}    :   {token, {kw_bucket,     TokenLine}}.
{LAST}      :   {token, {kw_last,       TokenLine}}.
{AS}        :   {token, {kw_as,         TokenLine}}.
{IN}        :   {token, {kw_in,         TokenLine}}.
{FROM}      :   {token, {kw_from,       TokenLine}}.
{BETWEEN}   :   {token, {kw_between,    TokenLine}}.
{LIKE}      :   {token, {kw_like,       TokenLine}}.
{NOW}       :   {token, {kw_now,        TokenLine}}.
{AGO}       :   {token, {kw_ago,        TokenLine}}.
{AND}       :   {token, {kw_and,        TokenLine}}.

derivate    :   {token, {derivate,      TokenLine, a(TokenChars)}}.
{AGGR}      :   {token, {aggr,          TokenLine, a(TokenChars)}}.
{CAGGR}     :   {token, {caggr,         TokenLine, a(TokenChars)}}.
{TIME}      :   {token, {time,          TokenLine, a(TokenChars)}}.

'{L}+'      :   S = strip(TokenChars,   TokenLen),
                {token, {string,        TokenLine, S}}.
{S}         :   {token, {metric,        TokenLine, b(TokenChars)}}.
{GS}        :   {token, {glob_metric,   TokenLine, list_to_binary(TokenChars)}}.
{D}+        :   {token, {integer,       TokenLine, i(TokenChars)}}.
[(),]       :   {token, {a(TokenChars), TokenLine}}.
{WS}+       :   skip_token.

Erlang code.

-ignore_xref([format_error/1, string/2, token/2, token/3, tokens/2, tokens/3]).

strip(TokenChars,TokenLen) -> lists:sublist(TokenChars, 2, TokenLen - 2).

a(L) -> list_to_atom(L).
i(L) -> list_to_integer(L).
b(L) -> list_to_binary(L).