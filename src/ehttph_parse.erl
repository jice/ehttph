%% @author JC Sargenton <jice@igwan.eu>
%% @copyright 2010 JC Sargenton
%%
%% @doc ehttph HTTP header fields library
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(ehttph_parse).
-author("JC Sargenton <jice@igwan.eu>").

-import(ehttph_parse_utils,
	[split/2, linify/1, clean_spaces/1, lowercase/1]).

-export([parse_field/1, name_to_atom/1]).


-include("../include/ehttph_records.hrl").
-include("../include/ehttph_chars.hrl").

-include_lib("eunit/include/eunit.hrl").

-spec parse_field/1 ::
  ({Name :: binary()|string()|atom(), Value :: binary()|string()}) ->
    % values not parsed (yet)
    {'From' |
     'Proxy-Authorization' |
     'Referer' |
     'Content-Location' |
     'Location' |
     'Proxy-Authenticate' |
     'WWW-Authenticate',     binary()} |
    %
    {'Cache-Control' |
     'Pragma' |
     'Transfer-Encoding' |
     'Expect',               [#element{}]} |
    % value is a list of simple binary tokens
    {'Connection' |
     'Trailer' |
     'Allow' |
     'Content-Encoding' |
     'Accept-Ranges',        [binary()]} |
    % erlang date value
    {'Date' |
     'If-Modified-Since' |
     'If-Unmodified-Since' |
     'If-Range' |
     'Expires' |
     'Last-Modified',        date()} |
    %
    {'Retry-After',          integer() | date()} |
    {'Upgrade',              [#product{}]} |
    %
    {'User-Agent' |
     'Server',               [#product{} | #comment{}]} |
    %
    {'Via',                  [#via_value{}]} |
    {'Warning',              [#warning_value{}]} |
    %
    {'Accept' |
     'Accept-Charset' |
     'Accept-Encoding' |
     'Accept-Language' |
     'TE',                   [#accept_element{}]} |
    %
    {'If-Match' |
     'If-None-Match',        '*' | [#entity_tag{}]} |
    %
    {'ETag',                 #entity_tag{}} |
    %
    {'Vary',                 '*' | [binary()]} |
    %
    {'Host',                 {Hostname::binary(), Port::integer()}} |
    %
    {'Max-Forwards' |
     'Content-Length' |
     'Age',                  integer()} |
    %
    {'Range',                [#byte_range_spec{}]} |
    %
    {'Content-Range',        #byte_content_resp_spec{}} |
    %
    {'Content-Type',         #media_type{}} |
    %
    {'Content-MD5',          md5_binary()} |
    % unknown fields
    {binary(),               binary()}.
    

%%% @doc  parses a known field's value or leave it unchanged if unknown
parse_field({Name, Value}) ->
    NormName = normalize_name(Name),
    NormValue = normalize_value(Value),
    {NormName, parse_known_field(NormName, NormValue)}.


%%% @doc  replace field name with the canonical name as atom()
normalize_name(Name) when is_list(Name) ->
    normalize_name(iolist_to_binary(Name));
normalize_name(Name) when is_atom(Name) ->
    normalize_name(atom_to_binary(Name, latin1));
normalize_name(Name) ->
    CleanName = clean_spaces(Name),
    LCName = lowercase(CleanName),
    name_to_atom(LCName).

%%% @doc  removes unnecessary spaces from value
normalize_value(Value) when is_list(Value) ->
    normalize_value(iolist_to_binary(Value));
normalize_value(Value) ->
    clean_spaces(Value).

%% @doc  Converts a known lowercase binary header field name to an atom.
%% @spec (FieldName::binary()) -> atom() | binary()
% General Header Fields
name_to_atom(<<"cache-control">>)       -> 'Cache-Control';
name_to_atom(<<"connection">>)          -> 'Connection';
name_to_atom(<<"date">>)                -> 'Date';
name_to_atom(<<"pragma">>)              -> 'Pragma';
name_to_atom(<<"trailer">>)             -> 'Trailer';
name_to_atom(<<"transfer-encoding">>)   -> 'Transfer-Encoding';
name_to_atom(<<"upgrade">>)             -> 'Upgrade';
name_to_atom(<<"via">>)                 -> 'Via';
name_to_atom(<<"warning">>)             -> 'Warning';
% Request Header Fields
name_to_atom(<<"accept">>)              -> 'Accept';
name_to_atom(<<"accept-charset">>)      -> 'Accept-Charset';
name_to_atom(<<"accept-encoding">>)     -> 'Accept-Encoding';
name_to_atom(<<"accept-language">>)     -> 'Accept-Language';
name_to_atom(<<"authorization">>)       -> 'Authorization';
name_to_atom(<<"expect">>)              -> 'Expect';
name_to_atom(<<"from">>)                -> 'From';
name_to_atom(<<"host">>)                -> 'Host';
name_to_atom(<<"if-match">>)            -> 'If-Match';
name_to_atom(<<"if-modified-since">>)   -> 'If-Modified-Since';
name_to_atom(<<"if-none-match">>)       -> 'If-None-Match';
name_to_atom(<<"if-range">>)            -> 'If-Range';
name_to_atom(<<"if-unmodified-since">>) -> 'If-Unmodified-Since';
name_to_atom(<<"max-forwards">>)        -> 'Max-Forwards';
name_to_atom(<<"proxy-authorization">>) -> 'Proxy-Authorization';
name_to_atom(<<"range">>)               -> 'Range';
name_to_atom(<<"referer">>)             -> 'Referer';
name_to_atom(<<"te">>)                  -> 'TE';
name_to_atom(<<"user-agent">>)          -> 'User-Agent';
% Response Header Fields
name_to_atom(<<"accept-ranges">>)       ->  'Accept-Ranges';
name_to_atom(<<"age">>)                 ->  'Age';
name_to_atom(<<"etag">>)                ->  'ETag';
name_to_atom(<<"location">>)            ->  'Location';
name_to_atom(<<"proxy-authenticate">>)  ->  'Proxy-Authenticate';
name_to_atom(<<"retry-after">>)         ->  'Retry-After';
name_to_atom(<<"server">>)              ->  'Server';
name_to_atom(<<"vary">>)                ->  'Vary';
name_to_atom(<<"www-authenticate">>)    ->  'WWW-Authenticate';
% Entity Header Fields
%  resource attributes
name_to_atom(<<"expires">>)             -> 'Expires';
name_to_atom(<<"last-modified">>)       -> 'Last-Modified';
name_to_atom(<<"allow">>)               -> 'Allow';
%  entity attributes
name_to_atom(<<"content-encoding">>)    -> 'Content-Encoding';
name_to_atom(<<"content-language">>)    -> 'Content-Language';
name_to_atom(<<"content-length">>)      -> 'Content-Length';
name_to_atom(<<"content-location">>)    -> 'Content-Location';
name_to_atom(<<"content-md5">>)         -> 'Content-MD5';
name_to_atom(<<"content-range">>)       -> 'Content-Range';
name_to_atom(<<"content-type">>)        -> 'Content-Type';
% Other Header Fields
name_to_atom(FieldName)                 -> lowercase(FieldName).

%% @doc  parses a known field's value
%% @spec (field_name(), field_value()) -> parsed_value()
parse_known_field('Cache-Control', Value) ->
    parse_list_bin(Value, fun parse_directive/1);
parse_known_field('Connection', Value) ->
    parse_list_bin(Value, fun parse_single_ci_token/1);
parse_known_field('Date', Value) ->
    parse_date(Value);
parse_known_field('Pragma', Value) ->
    parse_list_bin(Value, fun parse_directive/1);
parse_known_field('Trailer', Value) ->
    parse_list_bin(Value, fun parse_single_ci_token/1);
parse_known_field('Transfer-Encoding', Value) ->
    parse_list_bin(Value, fun parse_transfer_coding/1);
parse_known_field('Upgrade', Value) ->
    parse_list_bin(Value, fun parse_product/1);
parse_known_field('Via', Value) ->
    parse_list_bin(Value, fun parse_via/1);
parse_known_field('Warning', Value) ->
    parse_list_bin(Value, fun parse_warning_value/1);
parse_known_field('Accept', Value) ->
    parse_accept_value(Value, fun parse_media_range/1);
parse_known_field('Accept-Charset', Value) -> 
    parse_accept_value(Value, fun parse_simple_range/1);
parse_known_field('Accept-Encoding', Value) ->
    parse_accept_value(Value, fun parse_simple_range/1);
parse_known_field('Accept-Language', Value) ->
    parse_accept_value(Value, fun parse_language_range/1);
parse_known_field('Authorization', Value) ->
    % TODO : see RFC
    Value;
parse_known_field('Expect', Value) ->
    parse_list_bin(Value, fun parse_expectation/1);
parse_known_field('From', Value) ->
    % RFC2822
    Value;
parse_known_field('Host', Value) ->
    parse_host(Value);
parse_known_field('If-Match', <<"*">>) ->
    '*';
parse_known_field('If-Match', Value) ->
    parse_list_bin(Value, fun parse_entity_tag/1);
parse_known_field('If-Modified-Since', Value) ->
    parse_date(Value);
parse_known_field('If-None-Match', <<"*">>) ->
    '*';
parse_known_field('If-None-Match', Value) ->
    parse_list_bin(Value, fun parse_entity_tag/1);
parse_known_field('If-Range', Value) ->
    parse_alternative(Value, [fun(V)-> parse_entity_tag(read(V)) end,
			      fun parse_date/1]);
parse_known_field('If-Unmodified-Since', Value) ->
    parse_date(Value);
parse_known_field('Max-Forwards', Value) ->
    parse_integer(Value);
parse_known_field('Proxy-Authorization', Value) ->
    %% TODO : see RFC
    Value;
parse_known_field('Range', Value) ->
    parse_byte_range_specs(read(Value));
parse_known_field('Referer', Value) ->
    %% TODO : ( absoluteURI | relativeURI ) RFC3986
    Value;
parse_known_field('TE', Value) ->
    parse_accept_value(Value, fun parse_simple_range/1);
parse_known_field('User-Agent', Value) ->
    parse_product_comment(read(Value));
parse_known_field('Accept-Ranges', Value) ->
    parse_list_bin(Value, fun parse_single_ci_token/1);
parse_known_field('Age', Value) ->
    parse_integer(Value);
parse_known_field('ETag', Value) ->
    parse_entity_tag(read(Value));
parse_known_field('Location', Value) ->
    %% TODO : absoluteURI [ "#" fragment ] RFC3986
    Value;
parse_known_field('Proxy-Authenticate', Value) ->
    %% TODO : 1#challenge
    Value;
parse_known_field('Retry-After', Value) ->
    parse_alternative(Value, [fun parse_integer/1, fun parse_date/1]);
parse_known_field('Server', Value) ->
    parse_product_comment(read(Value));
parse_known_field('Vary',  <<"*">>) ->
    '*';
parse_known_field('Vary',  Value) ->
    parse_list_bin(Value, fun parse_single_ci_token/1);
parse_known_field('WWW-Authenticate', Value) ->
    %% TODO : 1#challenge
    Value;
parse_known_field('Allow', Value) ->
    parse_list_bin(Value, fun parse_single_cs_token/1);
parse_known_field('Content-Encoding', Value) ->
    parse_list_bin(Value, fun parse_single_ci_token/1);
parse_known_field('Content-Language', Value) ->
    parse_list_bin(Value, fun parse_content_language/1);
parse_known_field('Content-Length', Value) ->
    parse_integer(Value);
parse_known_field('Content-Location', Value) ->
    %% TODO : ( absoluteURI | relativeURI ) RFC3986
    Value;
parse_known_field('Content-MD5', Value) ->
    parse_md5_digest(Value);
parse_known_field('Content-Range', Value) ->
    parse_content_range_spec(read(Value));
parse_known_field('Content-Type', Value) ->
    parse_media_type(read(Value));
parse_known_field('Expires', Value) ->
    parse_date(Value);
parse_known_field('Last-Modified', Value) ->
    parse_date(Value);
parse_known_field(_Name, Value) ->
    Value.

%%% Parse values that can have alternative types
parse_alternative(Value, [Fun|NextFuns]) ->
    try Fun(Value) of
	ParsedValue -> ParsedValue
    catch
	_:_ -> parse_alternative(Value, NextFuns)
    end.

%%% Accept-* fields

parse_accept_value(Bin, RangeFun) ->
    AcceptElements = parse_list_bin(Bin, fun parse_accept_element/1),
    [ AcceptElement#accept_element
      {range=RangeFun(AcceptElement#accept_element.range)}
      || AcceptElement <- AcceptElements].

%%% Accept-* ranges

parse_media_range([{token,<<"*">>},'/',{token,<<"*">>}]) ->	    
    #media_range{type='*', subtype='*'};
parse_media_range([{token,Type},'/',{token,<<"*">>}]) ->
    #media_range{type=lowercase(Type), subtype='*'};
parse_media_range([{token,Type},'/',{token,SubType}]) ->
    #media_range{type=lowercase(Type), subtype=lowercase(SubType)}.

parse_simple_range([{token,<<"*">>}]) ->
    '*';
parse_simple_range([{token,Range}]) ->
    lowercase(Range).

parse_language_range([{token,<<"*">>}]) ->
    #language_range{tag='*', subtag='*'};
parse_language_range([{token,LanguageRange}]) ->
    case parse_language(LanguageRange) of
	[Tag] ->
	    #language_range{tag=Tag, subtag='*'};
	[Tag, Subtag] ->
	    #language_range{tag=Tag, subtag=Subtag}
    end.

%%% Binary (not tokenized) values
  
parse_date(Bin) ->
    ehttph_rfc1123:parse_date(Bin).

parse_integer(Bin) ->
    list_to_integer(binary_to_list(Bin)).

parse_md5_digest(Bin) ->
    <<_:128>> = base64:decode(Bin).

%% @doc  Normalizes and parses the host part.
%% @spec (host()) -> {HostName::binary(),Port::integer()}
parse_host(Host) ->
    case split(Host, $:) of
	{Hostname, Port} ->
	    PortNumber = list_to_integer(binary_to_list(Port)),
	    {lowercase(Hostname), PortNumber};
	_ ->
	    {lowercase(Host), 80}
    end.

%%% Symbolic values

%% @spec (ListInTokens::[token()]) ->
%%           [ElemParseFun(ElementInTokens::[token()])]
parse_list_bin(Bin, ElemParseFun) ->
    parse_list_symbols(read(Bin), ElemParseFun).

parse_list_symbols(Symbols, ElemParseFun) ->
    [ElemParseFun(Element) || Element <- tokenize(Symbols, ','), Element =/= []].

%% @spec (CommentToken::token()) -> CommentValue::comment_value()
parse_comment({comment, _} = Comment) ->
    Comment.

parse_qvalue(<<"0">>) ->
    0.0;
parse_qvalue(<<"1">>) ->
    1.0;
parse_qvalue(<<"0.">>) ->
    0.0;
parse_qvalue(<<"1.">>) ->
    1.0;
parse_qvalue(<<"0.",Dec/binary>>) when size(Dec) =:= 1 ->    
    parse_integer(Dec) / 10;
parse_qvalue(<<"0.",Dec/binary>>) when size(Dec) =:= 2 ->
    parse_integer(Dec) / 100;
parse_qvalue(<<"0.",Dec/binary>>) when size(Dec) =:= 3 ->
    parse_integer(Dec) / 1000;
parse_qvalue(<<"1.",Dec/binary>>) ->
    0 = parse_integer(Dec),
    1.0.

%%% Element-specific parsing

%%%%%%%%%%%%%%%

%% @doc parses a header-element
%% @spec ([token()]) -> #element{}
parse_element(Tokens) ->
    parse_element_name(Tokens, #element{}).

parse_element_name(Tokens, Element) ->
    parse_element_name(Tokens, Element, []).

parse_element_name([], Element, Acc) ->
    Element#element{name=lists:reverse(Acc)};
parse_element_name(['='|Rest], Element, Acc) ->
    parse_element_value(Rest, Element#element{name=lists:reverse(Acc)});
parse_element_name([';'|Rest], Element, Acc) ->
    parse_element_params(Rest, Element#element{name=lists:reverse(Acc)});
parse_element_name([Token|Rest], Element, Acc) ->
    parse_element_name(Rest, Element, [Token|Acc]).

parse_element_value(Tokens, Element) ->
    parse_element_value(Tokens, Element, []).

parse_element_value([], Element, Acc) ->
    Element#element{value=lists:reverse(Acc)};
parse_element_value([';'|Rest], Element, Acc) ->
    parse_element_params(Rest, Element#element{value=lists:reverse(Acc)});
parse_element_value([Token|Rest], Element, Acc) ->
    parse_element_value(Rest, Element, [Token|Acc]).

parse_element_params(Tokens, Element) ->
    Element#element{params=lists:map(fun parse_element_param/1,
				     tokenize(Tokens, ';'))}.

parse_element_param([{token,Name}]) ->
    #param{name=lowercase(Name)};
parse_element_param([{token,Name},'=']) ->
    #param{name=lowercase(Name)};
parse_element_param([{token,Name},'=',{_,Value}]) ->
    LCName = lowercase(Name),
    %% Charset is case-insensitive
    LCValue = case LCName of
		  <<"charset">> -> lowercase(Value);
		  _             -> Value
	      end,
    #param{name=LCName, value=LCValue}.


parse_accept_element(Tokens) ->
    #element{name=Range, params=Params} = parse_element(Tokens),
    {RangeParams, QValue, ExtParams} = split_accept_params(Params),
    #accept_element{range=Range,
		    range_params=RangeParams,
		    qvalue=QValue,
		    ext_params=ExtParams}.

split_accept_params(Params) ->
    {ValueParams, Rest} =
	lists:splitwith(fun (#param{name= <<"q">>}) -> false;
			    (_) -> true end,
			Params),
    {QValue, Extensions} = 
	case Rest of
	    [#param{name= <<"q">>,value=QVal}|Exts] ->
		{parse_qvalue(QVal), Exts};
	    [] ->
		{1.0, []}
	end,
    {ValueParams, QValue, Extensions}.

%%%%%%%%%%%%%%%

parse_directive(Tokens) ->
    parse_directive_element(parse_element(Tokens)).

parse_directive_element(#element{name=[{token,Name}],
				 value=[{Type,Value}],
				 params=[]}) 
  when Type =:= token; Type =:= quoted_string ->
    #element{name=lowercase(Name), value=Value};
parse_directive_element(#element{name=[{token,Name}],
				 value=undefined,
				 params=[]}) ->
    #element{name=lowercase(Name), value=undefined}.

parse_transfer_coding(Tokens) ->
    parse_transfer_coding_element(parse_element(Tokens)).

parse_transfer_coding_element(#element{name=[{token,Name}],
				       value=undefined,
				       params=Params}) ->
    #element{name=lowercase(Name), params=Params}.

parse_single_ci_token([{token,Name}]) ->
    lowercase(Name).

parse_single_cs_token([{token,Name}]) ->
    Name.

parse_product([{token,Name},'/',{token,Version}]) ->
    #product{name=Name,version=Version};
parse_product([{token,Name}]) ->
    #product{name=Name}.

parse_via(Value) ->
    {{Name,Version}, [' '|Rest]} = parse_protocol(Value),
    {ReceivedBy, Rest2} = parse_host_or_pseudonym(Rest),
    Comment =
	case Rest2 of
	    [] -> undefined;
	    [' ',C] -> parse_comment(C)
	end,
    #via_value{protocol_name=Name,
	       protocol_version=Version,
	       received_by=ReceivedBy,
	       comment=Comment}.

parse_protocol([{token,Name},'/',{token,Version}|Rest]) ->
    {{Name, Version}, Rest};
parse_protocol([{token,Version}|Rest]) ->
    {{<<"HTTP">>, Version}, Rest}.

parse_host_or_pseudonym([{token,Host},':',{token,Port}|Rest]) ->
    {{lowercase(Host),parse_integer(Port)}, Rest};
parse_host_or_pseudonym([{token,HostOrPseudonym}|Rest]) ->
    {lowercase(HostOrPseudonym), Rest}.

parse_warning_value(Value) ->
    {Code, [' '|Rest]} = parse_warn_code(Value),
    {Agent, [' ',{quoted_string,Text}|Rest2]} =	parse_host_or_pseudonym(Rest),
    Date = 
	case Rest2 of
	    [] -> undefined;
	    [' ',{quoted_string, D}] -> parse_date(D)
	end,
    #warning_value{code=Code,agent=Agent,text=Text,date=Date}.

parse_warn_code([{token,WarnCode}|Rest]) when size(WarnCode) =:= 3 ->
    {parse_integer(WarnCode), Rest}.

parse_language(Language) ->
    parse_language(lowercase(Language), []).

parse_language(<<>>, Acc) ->
    lists:reverse(Acc);
parse_language(Language, Acc) ->
    case split(Language, $-) of
	false ->
	    parse_language(<<>>, [Language|Acc]);
	{SubTag, Rest} when size(SubTag) =< 8 ->
	    parse_language(Rest, [SubTag|Acc])
    end.

parse_content_language([{token,Language}]) ->
    parse_language(Language).


parse_expectation(Tokens) ->
    #element{name=[{token,Name}],value=Value} = Element = parse_element(Tokens),
    Element#element{name=lowercase(Name),
		    value=case Value of 
			      [{token,V}] ->
				  lowercase(V);
			      [{quoted_string,V}] ->
				  V;
			      undefined ->
				  undefined
			  end}.

parse_entity_tag([{token,<<"W">>},'/',{quoted_string,OpaqueTag}]) ->
    #entity_tag{weak=true,opaque=OpaqueTag};
parse_entity_tag([{quoted_string,OpaqueTag}]) ->
    #entity_tag{opaque=OpaqueTag}.

parse_byte_range_specs([{token,<<"bytes">>},'='|Specs]) ->
    parse_list_symbols(Specs, fun parse_byte_range_spec/1).

parse_byte_range_spec([{token,Spec}]) ->
    case split(Spec, $-) of
	{<<_,_/binary>> = First, <<>>} ->
	    #byte_range_spec{first=parse_integer(First)};
	{<<_,_/binary>> = First, Last} ->
	    #byte_range_spec{first=parse_integer(First),
			     last=parse_integer(Last)};
	{<<>>, <<_,_/binary>> = Last} ->
	    #byte_range_spec{last=parse_integer(Last)}
    end.

parse_product_comment(Value) ->    
    parse_product_comment(Value, []).

parse_product_comment([], Acc) ->
    lists:reverse(Acc);
parse_product_comment([{comment,_} = Comment|Rest], Acc) ->
    parse_product_comment(Rest, [Comment|Acc]);
parse_product_comment([{token,Name},'/',{token,Version}|Rest], Acc) ->
    Product = [{token,Name},'/',{token,Version}],
    parse_product_comment(Rest, [parse_product(Product)|Acc]);
parse_product_comment([{token,Version}|Rest], Acc) ->
    Product = [{token,Version}],
    parse_product_comment(Rest, [parse_product(Product)|Acc]);
parse_product_comment([' '|Rest], Acc) ->
    parse_product_comment(Rest, Acc).

parse_content_range_spec([{token,<<"bytes">>},' ',
			  {token,Range},'/',{token,LengthOrAsterisk}]) ->
    {First, Last} = parse_resp_byte_range_spec(Range),
    Length = case LengthOrAsterisk of
		 <<"*">> -> undefined;
		 X -> parse_integer(X)
	     end,
    #byte_content_resp_spec{first=First,last=Last,length=Length}.

parse_resp_byte_range_spec(<<"*">>) ->
    {undefined, undefined};
parse_resp_byte_range_spec(Range) ->
    {First, Last} = split(Range, $-),
    {parse_integer(First), parse_integer(Last)}.

parse_media_type(Tokens) ->
    #element{name=[{token,Type},'/',{token,SubType}],
	     value=undefined,
	     params=Params} = parse_element(Tokens),
    #media_type{type=lowercase(Type),
		subtype=lowercase(SubType),
		params=Params}.

%%% scanning

%% @doc  First pass of scanning
read(Binary) ->
    read(Binary, []).

read(<<>>, Acc) ->
    lists:reverse(Acc);
% token
read(<<C, _/binary>> = Bin, Acc) when ?TCHAR(C) ->
    {Token, Rest} = read_token(Bin),
    read(Rest, [{token, Token}|Acc]);
% lws
read(<<C, Rest/binary>>, [Prev|_] = Acc) when ?SP_or_HT(C), is_atom(Prev) ->
    Rest2 = read_lws(Rest),
    read(Rest2, Acc);
read(<<C, Rest/binary>>, Acc) when ?SP_or_HT(C) ->
    Rest2 = read_lws(Rest),
    read(Rest2, [' '|Acc]);
% quoted-string
read(<<?DQUOTE, _/binary>> = Bin, Acc) ->
    {String, Rest} = read_quoted_string(Bin),
    read(Rest, [{quoted_string, String}|Acc]);
% comment
read(<<?OPAR, _/binary>> = Bin, Acc) ->
    {Comment, Rest} = read_comment(Bin),
    read(Rest, [Comment|Acc]);
% Separator (when the preceding token is a space, it is removed)
read(<<C, Rest/binary>>, [' '|Acc]) when ?SEPARATOR(C) ->
    read(Rest, [list_to_atom([C])|Acc]);
read(<<C, Rest/binary>>, Acc) when ?SEPARATOR(C) ->
    read(Rest, [list_to_atom([C])|Acc]).


%% @doc  Parses one token.
read_token(Binary) ->
    read_token(Binary, <<>>).

read_token(<<>>, <<>>) ->
    false;
read_token(<<>>, Acc) ->
    {Acc, <<>>};
read_token(<<C, Rest/binary>>, Acc) when ?TCHAR(C) ->
    read_token(Rest, <<Acc/binary, C>>);
read_token(_, <<>>) ->
    false;
read_token(Rest, Acc) ->
    {Acc, Rest}.

%% @doc  Reads until a non-whitespace character and return the rest
read_lws(<<C, Rest/binary>>) when ?SP_or_HT(C) ->
    read_lws(Rest);
read_lws(Rest) ->
    Rest.

%% @doc  Reads a quoted string
read_quoted_string(<<?DQUOTE, Bin/binary>>) ->
    read_quoted_string(Bin, <<>>).

read_quoted_string(<<?DQUOTE, Rest/binary>>, Acc) ->
    {Acc, Rest};
read_quoted_string(<<?BACKSLASH, Char, Rest/binary>>, Acc) when ?CHAR(Char) ->
    read_quoted_string(Rest, <<Acc/binary,Char>>);
read_quoted_string(<<Char, Rest/binary>>, Acc) when ?TEXT(Char) ->
    read_quoted_string(Rest, <<Acc/binary,Char>>).    

%% @doc  Reads a comment (comments can be nested)
read_comment(<<?OPAR, Bin/binary>>) ->
    read_comment(Bin, <<>>, []).

read_comment(<<?CPAR, Rest/binary>>, <<>>, Acc) ->
    {{comment, lists:reverse(Acc)}, Rest};
read_comment(<<?CPAR, Rest/binary>>, TextAcc, Acc) ->
    NewAcc = [TextAcc|Acc],
    {{comment, lists:reverse(NewAcc)}, Rest};
read_comment(<<?BACKSLASH, Char, Rest/binary>>, TextAcc, Acc)
  when ?CHAR(Char) ->
    read_comment(Rest, <<TextAcc/binary,Char>>, Acc);
read_comment(<<?OPAR, _/binary>> = Bin, TextAcc, Acc) ->
    NewAcc = [TextAcc|Acc],
    {NestedComment, Rest} = read_comment(Bin),
    read_comment(Rest, <<>>, [NestedComment|NewAcc]);
read_comment(<<Char, Rest/binary>>, TextAcc, Acc) when ?TEXT(Char) ->
    read_comment(Rest, <<TextAcc/binary,Char>>, Acc).
    
%% @ doc  split a list into a number of sublist delimited by Separator
tokenize(List, Separator) ->
    tokenize(List, Separator, []).

tokenize([], _Separator, Acc) ->
    lists:reverse(Acc);
tokenize(List, Separator, Acc) ->
    case
	lists:splitwith(
	  fun (S) when S =:= Separator -> false;
	      (_) -> true
	  end, List) of
	{Token, [Separator|Rest]} ->
	    tokenize(Rest, Separator, [Token|Acc]);
	{Token, Rest} ->
	    tokenize(Rest, Separator, [Token|Acc])
    end.

  





%%% EUNIT TESTS

cachecontrol_test() ->
    {'Cache-Control',
      [#element{name= <<"no-cache">>,value=undefined},
       #element{name= <<"max-stale">>,value= <<"123">>}]} =
	parse_field({<<"CACHE-contrOL">>,<<"no-CacHe ,max-StAle= 123">>}).

connection_test() ->
    {'Connection', [<<"close">>,<<"open">>]} =
	parse_field({<<"connection">>,<<" clOse,OpEn  ">>}).

date_test() ->
    {'Date', {{1979,9,17},{16,30,0}}} =
	parse_field({<<"DAte">>,<<"Mon, 17 Sep 1979 16:30:00 GMT">>}).

pragma_test() ->
    {'Pragma',
      [#element{name= <<"no-cache">>, value=undefined},
       #element{name= <<"extpragma">>, value= <<"ExT">>},
       #element{name= <<"token">>, value= <<"Value">>}]} =
	parse_field({<<"pragma">>,<<"No-CaChe,,ExTPRAGMA=\"ExT\", Token=Value">>}).

trailer_test() ->
    {'Trailer', [<<"accept">>, <<"host">>]} =
	parse_field({<<"trailer">>,<<" accepT ,Host">>}).

transferencoding_test() ->
    {'Transfer-Encoding',
      [#element{name= <<"chunked">>, params=[]},
       #element{name= <<"other">>, params=
		[#param{name= <<"name">>,value= <<"VALUE">>},
		 #param{name= <<"name2">>,value= <<"Value2">>}]}]} =
	parse_field({<<"transfer-encoding">>,<<"chuNkEd,oTher;Name=VALUE;Name2=\"Value2\"">>}).

upgrade_test() ->
    {'Upgrade',
      [#product{name= <<"HTTP">>, version= <<"2.0">>},
       #product{name= <<"IRC">>, version= <<"x.11">>}]} =
	parse_field({<<"upgrade">>,<<"HTTP/2.0,IRC/x.11">>}).

via_test() ->
    {'Via',
      [#via_value{protocol_name= <<"HTTP">>,
		  protocol_version= <<"1.0">>,
		  received_by= {<<"revproxy.example.com">>,1234},
		  comment= {comment,[<<"Damn old version!">>]}},
       #via_value{protocol_name= <<"IRC">>,
		  protocol_version= <<"2.0">>,
		  received_by= <<"host">>}]} =
	parse_field({<<"via">>,<<"1.0 RevProxy.Example.com:1234 (Damn old version!), IRC/2.0 hOst">>}).

warning_test() ->
    {'Warning',
      [#warning_value{code=150,
		      agent={<<"k1.example.com">>,1234},
		      text= <<"your server's on fire!">>,
		      date={{2008,5,25},{20,9,25}}},
       #warning_value{code=158,
		      agent= <<"thetroll">>,
		      text= <<"mind the step">>}]} =
	parse_field({<<"warning">>,<<"150 k1.example.com:1234  \"your server's on fire!\" \"Sun, 25 May 2008 20:09:25 GMT\", 158 thetroll \"mind the step\"">>}).

accept_test() ->
    {'Accept',
      [#accept_element{range=#media_range{type= <<"text">>, subtype='*'},
		       range_params=[#param{name= <<"level">>,value= <<"2">>}],
		       qvalue=0.8, ext_params=[]},
       #accept_element{range=#media_range{type= <<"image">>,
					  subtype= <<"png">>},
		       range_params=[], qvalue=1.0, ext_params=[]},
       #accept_element{range=#media_range{type='*',subtype='*'},
		       range_params=[], qvalue=0.0,
		       ext_params=[#param{name= <<"name">>,
					  value= <<"valUe">>}]}]} =
	parse_field({<<"accept">>,<<"teXt/*;level=2;q=0.80,image/pnG,*/*;q=0.;namE=valUe">>}).

acceptcharset_test() ->
    {'Accept-Charset',
      [#accept_element{range = <<"utf-8">>,range_params = [],
		       qvalue = 1.0,ext_params = []},
       #accept_element{range = '*',range_params = [],qvalue = 0.0,
		       ext_params = []}]} =
	parse_field({<<"accept-charset">>,<<"UTf-8,*;\r\n q=0 ">>}).

acceptencoding_test() ->
    {'Accept-Encoding',
      [#accept_element{range = '*',range_params = [],qvalue = 0.0,
		       ext_params = []},
       #accept_element{range = <<"compress">>,range_params = [],
		       qvalue = 1.0,ext_params = []},
       #accept_element{range = <<"gzip">>,range_params = [],
		       qvalue = 1.0,ext_params = []}]} =
	parse_field({<<"accept-encoding">>,<<"*;q=0.0,comPRess,gzip,,">>}).

acceptlanguage_test() ->
    {'Accept-Language',
      [#accept_element{range = #language_range{tag= <<"fr">>,
					       subtag= <<"fr">>},
                       range_params = [],qvalue = 1.0,ext_params = []},
       #accept_element{range = #language_range{tag= <<"fr">>,
					       subtag= <<"bl">>},
		       range_params = [],qvalue = 0.7,ext_params = []},
       #accept_element{range = #language_range{tag= <<"gcf">>,
					       subtag= '*'},
		       range_params = [],qvalue = 0.3,
		       ext_params = []}]} =
	parse_field({<<"accept-language">>,<<"fr-FR,fr-BL;q=0.7,gcf;q=0.3">>}).

authorization_test() ->
    {'Authorization', <<"this is some credentials">>} =
	parse_field({<<"authorization">>,<<"this is some credentials">>}).

expect_test() ->
    {'Expect',
      [#element{name= <<"100-continue">>, params=[]},
       #element{name= <<"love">>, value= <<"true">>,
		params=[#param{name= <<"param1">>,value= <<"YeS">>}]},
       #element{name= <<"hate">>, value= <<"NonE">>,
		params=[]}]} =
	parse_field({<<"expect">>,<<"100-Continue,lOVe=trUe;pARam1 =YeS,hate=\"NonE\"">>}).

from_test() ->
    {'From', <<"mymail@example.com">>} =
	parse_field({<<"from">>,<<"mymail@example.com">>}).

host_test() ->
    {'Host', {<<"www.example.com">>, 80}} =
	parse_field({<<"host">>,<<"www.example.com:80">>}).

ifmatch_test() ->
    {'If-Match',
      [#entity_tag{weak=true, opaque= <<"ThisIsAWeakTag">>},
       #entity_tag{weak=false, opaque= <<"A strong Tag">>}]} =
	parse_field({<<"if-match">>,<<"W/\"ThisIsAWeakTag\",\"A strong Tag\"">>}).

ifnonematch_test() ->
    {'If-None-Match',
      [#entity_tag{weak=true, opaque= <<"ThisIsAWeakTag">>},
       #entity_tag{weak=false, opaque= <<"A strong Tag">>}]} =
	parse_field({<<"if-none-match">>,<<"W/\"ThisIsAWeakTag\",\"A strong Tag\"">>}).

ifmodifiedsince_test() ->
    {'If-Modified-Since', {{1979,9,17},{16,30,0}}} =
	parse_field({<<"if-modified-since">>,<<"Mon, 17 Sep 1979 16:30:00 GMT">>}).

ifunmodifiedsince_test() ->
    {'If-Unmodified-Since', {{1979,9,17},{16,30,0}}} =
	parse_field({<<"if-unmodified-since">>,<<"Mon, 17 Sep 1979 16:30:00 GMT">>}).

ifrange_test() ->
    {'If-Range', {{1979,9,17},{16,30,0}}} =
	parse_field({<<"if-range">>,<<"Mon, 17 Sep 1979 16:30:00 GMT">>}).

ifrange2_test() ->
    {'If-Range', #entity_tag{weak=true, opaque= <<"ThisIsAWeakTag">>}} =
	parse_field({<<"if-range">>,<<"W/\"ThisIsAWeakTag\"">>}).

maxforwards_test() ->
    {'Max-Forwards', 16} =
	parse_field({<<"max-forwards">>,<<"16">>}).

proxyauthorization_test() ->
    {'Proxy-Authorization', <<"some credentials">>} =
	parse_field({<<"proxy-authorization">>,<<"some credentials">>}).

range_test() ->
    {'Range',
      [#byte_range_spec{first=400, last=500},
       #byte_range_spec{last=500},
       #byte_range_spec{first=600}]} =
	parse_field({<<"range">>,<<"bytes = 400-500, -500 , 600-,,">>}).

referer_test() ->
    {'Referer', <<"http://example.com/blog/1">>} =
	parse_field({<<"referer">>,<<"http://example.com/blog/1">>}).

te_test() ->
    {'TE',
      [#accept_element{range = <<"deflate">>, range_params= [], qvalue=0.5,
		       ext_params = [#param{name= <<"prefered">>,
					    value= <<"true">>}]},
       #accept_element{range = <<"trailers">>, range_params= [], qvalue=1.0,
		       ext_params = []},
       #accept_element{range = <<"other">>,
		       range_params= [#param{name= <<"a">>,value= <<"B">>}],
		       qvalue=1.0,
		       ext_params = []}]} =
	parse_field({<<"te">>,<<"deFlate;q=0.5;prefered=true,trailers,other;A=B">>}).

useragent_test() ->
    {'User-Agent',
      [#product{name= <<"Bidule">>, version= <<"2.0">>},
       {comment, [<<"Linux; ">>,{comment, [<<"un super OS">>]}]},
       #product{name= <<"Erlang">>, version= <<"R15B-1">>}]} =
	parse_field({<<"user-agent">>,<<"Bidule/2.0 (Linux; (un super OS)) Erlang/R15B-1">>}).

allow_test() ->
    {'Allow', [<<"GET">>,<<"PUT">>,<<"POST">>,<<"PUT">>]} =
	parse_field({<<"allow">>,<<"GET,PUT,POST,PUT">>}).

contentencoding_test() ->
    {'Content-Encoding', [<<"gzip">>,<<"compress">>,<<"deflate">>]} =
	parse_field({<<"content-encoding">>,<<"GZIP,compress,DEFLATE">>}).

contentlanguage_test() ->
    {'Content-Language', [[<<"fr">>],[<<"fr">>,<<"bl">>],[<<"en">>]]} =
	parse_field({<<"content-language">>,<<"fr,fr-BL,en">>}).

contentlength_test() ->
    {'Content-Length', 5482} =
	parse_field({<<"content-length">>,<<"5482">>}).

contentlocation_test() ->
    {'Content-Location', <<"http://localhost:1234">>} =
	parse_field({<<"content-location">>,<<"http://localhost:1234">>}).

contentmd5_test() ->
    {'Content-MD5', <<556854875:128>>} =
	parse_field({<<"content-md5">>,<<"AAAAAAAAAAAAAAAAITDuWw==">>}).

contentrange_test() ->
    {'Content-Range',
      #byte_content_resp_spec{first=125,last=256,length=3000}} =
	parse_field({<<"content-range">>,<<"bytes 125-256/3000">>}).

contenttype_test() ->
    {'Content-Type',
      #media_type{type= <<"text">>,
		  subtype= <<"html">>,
		  params= [#param{name= <<"charset">>,value= <<"utf-8">>}]}} =
	parse_field({<<"content-type">>,<<"TeXt/HtML ; ChArSET=UTf-8">>}).

expires_test() ->
    {'Expires', {{1979,9,17},{16,30,0}}} =
	parse_field({<<"expires">>,<<"Mon, 17 Sep 1979 16:30:00 GMT">>}).

lastmodified_test() ->
    {'Last-Modified', {{1979,9,17},{16,30,0}}} =
	parse_field({<<"last-modified">>,<<"Mon, 17 Sep 1979 16:30:00 GMT">>}).

acceptranges_test() ->
    {'Accept-Ranges', [<<"bytes">>,<<"frames">>]} =
	parse_field({<<"Accept-ranges">>,<<"Bytes,,Frames">>}).

age_test() ->
    {'Age', 240} = parse_field({<<"age">>, <<"240">>}).

etag_test() ->
    {'ETag', #entity_tag{weak=true, opaque= <<"ThisIsAWeakTag">>}} =
	parse_field({<<"eTag">>,<<"W/\"ThisIsAWeakTag\"">>}).

location_test() ->
    {'Location', <<"http://example.com/test">>} =
	parse_field({<<"location">>,<<"http://example.com/test">>}).

proxyauthenticate_test() ->
    {'Proxy-Authenticate', <<"ThisIsaChallenge">>} =
	parse_field({<<"Proxy-authenticate">>,<<"ThisIsaChallenge">>}).

retryafter_test() ->
    {'Retry-After', 125} =
	parse_field({<<"Retry-After">>,<<"125">>}).

retryafter2_test() ->
    {'Retry-After', {{1979,9,17},{16,30,0}}} =
	parse_field({<<"Retry-After">>,<<"Mon, 17 Sep 1979 16:30:00 GMT">>}).

server_test() ->
    {'Server',
     [#product{name= <<"Webserver">>, version= <<"4.1.0">>},
      {comment, [<<"mod_erlang">>,
		 {comment, [<<"best product ever">>]}]}]} =
	parse_field({<<"serVer">>,<<"Webserver/4.1.0  (mod_erlang(best product ever))">>}).

vary_test() ->
    {'Vary', '*'} = parse_field({<<"vary">>,<<" * ">>}).

vary2_test() ->
    {'Vary', [<<"cookie">>,<<"accept">>]} =
	parse_field({<<"vary">>, <<"Cookie,Accept">>}).

wwwauthenticate_test() ->
    {'WWW-Authenticate', <<"ThisIsaChallenge">>} =
	parse_field({<<"www-Authenticate">>,<<"ThisIsaChallenge">>}).
