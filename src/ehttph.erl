-module(ehttph).
-author("JC Sargenton <jice@igwan.eu>").

-export([parse_field/1]).

%%% @doc  parses a known header field's value,
%%%       leave it unchanged if unknown
parse_field(FieldTuple) ->
    ehttph_parse:parse_field(FieldTuple).

    
