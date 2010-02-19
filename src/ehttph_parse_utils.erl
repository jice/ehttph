%% @author JC Sargenton <jice@igwan.eu>
%% @copyright 2010 JC Sargenton
%%
%% @doc ehttph HTTP header fields library
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(ehttph_parse_utils).
-author("JC Sargenton <jice@igwan.eu>").

-export([linify/1, split/2, clean_spaces/1, lowercase/1]).

-export([split_crlf/1, split_2x_crlf/1, tokenize/2]).

-include("../include/ehttph_chars.hrl").

%%------------------------------------------------------------------------------
%% Utility functions
%%------------------------------------------------------------------------------

%% @doc  Separate a binary into lines (ignoring empty lines and keeping LWS)
%%       (does NOT remove trailing CRLFs from the binary).
%% @spec (binary()) -> [binary()]
linify(Binary) when is_binary(Binary) ->
    linify(Binary, <<>>, []).

linify(<<>>, <<>>, Acc) ->
    lists:reverse(Acc); % ignore empty lines
linify(<<>>, Line, Acc) ->
    linify(<<>>, <<>>, [Line | Acc]);
linify(<<?CRLF, C, Rest/binary>>, <<>>, Acc)
    when C =/= (?SP), C =/= (?HT) ->
    linify(<<C, Rest/binary>>, <<>>, Acc); % ignore empty lines
linify(<<?CRLF, C, Rest/binary>>, Line, Acc)
    when C =/= (?SP), C =/= (?HT) ->
    linify(<<C, Rest/binary>>, <<>>, [Line | Acc]);
linify(<<C, Rest/binary>>, Line, Acc) ->
    linify(Rest, <<Line/binary, C>>, Acc).

%% @doc  Splits the binary at the first occurrence of Sep (not included)
%% @spec (binary(),integer()) -> {Part1, Part2} | false
split(Binary, Sep)
  when is_binary(Binary), is_integer(Sep) ->
    split(Binary, Sep, <<>>).

split(<<>>, _Sep, _Acc) ->
    false;
split(<<S, Rest/binary>>, Sep, Acc) when S =:= Sep ->
    {Acc, Rest};
split(<<C, Rest/binary>>, Sep, Acc) ->
    split(Rest, Sep, <<Acc/binary, C>>).

%% @doc  Splits at the first CRLF
%% @spec (binary()) -> {Part1, Part2} | false
split_crlf(Binary) when is_binary(Binary) ->
    split_crlf(Binary, <<>>).

split_crlf(<<>>, _Acc) ->
    false;
split_crlf(<<?CRLF, Rest/binary>>, Acc) ->
    {Acc, Rest};
split_crlf(<<C, Rest/binary>>, Acc) ->
    split_crlf(Rest, <<Acc/binary, C>>).

%% @doc  Splits at the first *double* CRLF.
%% @spec (binary()) -> {Part1, Part2} | false
split_2x_crlf(Binary) when is_binary(Binary) ->
    split_2x_crlf(Binary, <<>>).

split_2x_crlf(<<>>, _Acc) ->
    false;
split_2x_crlf(<<?CRLF,?CRLF, Rest/binary>>, Acc) ->
    {Acc, Rest};
split_2x_crlf(<<C, Rest/binary>>, Acc) ->
    split_2x_crlf(Rest, <<Acc/binary, C>>).

%% @doc  Normalize (removes leading spaces and LWS).
%% @spec (binary()) -> binary()
clean_spaces(Binary) ->
    remove_tsp(
      remove_lsp(
	replace_lws(Binary))).


%%% Internal functions (not exported)

%% @doc replaces folding LWS (\r\n + any # of SP or HT) into a single SP
%% (replaces single \r\n with SP too, but we don't expect them)
replace_lws(Binary) ->
    replace_lws(Binary, []).

replace_lws(<<>>, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
replace_lws(<<?CRLF, Rest/binary>>, Acc) ->
    Rest2 = remove_lsp(Rest),
    replace_lws(Rest2, [32 | Acc]);
replace_lws(<<C, Rest/binary>>, Acc) ->
    replace_lws(Rest, [C | Acc]).

%% @doc removes spaces (SP and HT) from the beginning of a binary
remove_lsp(<<C, Rest/binary>>)
  when C =:= (?SP); C =:= (?HT) ->
    remove_lsp(Rest);
remove_lsp(Binary) -> Binary.

%% @doc removes spaces (SP and HT) from the end of a binary
remove_tsp(<<>>) ->
    <<>>;
remove_tsp(Binary) ->
    remove_tsp(Binary, size(Binary) - 1).

remove_tsp(Binary, Pos) ->
    case split_binary(Binary, Pos) of
	{Part, <<C>>} when C =:= (?SP); C =:= (?HT) ->
	    remove_tsp(Part, size(Part) - 1);
	{Part, <<C>>} ->
	    iolist_to_binary([Part, C])
    end.

%% @doc  Converts a binary to lower case.
%% @spec (binary()) -> (binary())
lowercase(Binary) ->
    lowercase(Binary, <<>>).

lowercase(<<>>, Acc) ->
    iolist_to_binary(Acc);
lowercase(<<C, Rest/binary>>, Acc)
  when C >= $A, C =< $Z ->
    lowercase(Rest, [Acc, C + 32]);
lowercase(<<C, Rest/binary>>, Acc) ->
    lowercase(Rest, [Acc, C]).

%% @doc  Tokenizes a binary using Separator (multiple consecutive Sep
%%       count as one).
%% @spec (binary(), integer()) -> [binary()]
tokenize(Binary, Sep) ->
    tokenize(Binary, Sep, [], []).

tokenize(<<>>, _Sep, [], Acc) -> % ignore empty tokens
    lists:reverse(Acc);
tokenize(<<>>, _Sep, TokenAcc, Acc) ->
    Token = iolist_to_binary(lists:reverse(TokenAcc)),
    lists:reverse([Token | Acc]);
tokenize(<<S, Rest/binary>>, Sep, [], Acc)
  when S =:= Sep -> % ignore empty tokens
    tokenize(Rest, Sep, [], Acc);
tokenize(<<S, Rest/binary>>, Sep, TokenAcc, Acc)
  when S =:= Sep ->
    Token = iolist_to_binary(lists:reverse(TokenAcc)),
    tokenize(Rest, Sep, [], [Token | Acc]);
tokenize(<<C, Rest/binary>>, Sep, TokenAcc, Acc) ->
    tokenize(Rest, Sep, [C | TokenAcc], Acc).

