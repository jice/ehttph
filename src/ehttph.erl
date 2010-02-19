%% @author JC Sargenton <jice@igwan.eu>
%% @copyright 2010 JC Sargenton
%%
%% @doc ehttph HTTP header fields library
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(ehttph).
-author("JC Sargenton <jice@igwan.eu>").

-export([parse_field/1]).

-export([field_value/2]).

-export([header_field_type/1]).


%%% @doc  parses a known header field's value,
%%%       leave it unchanged if unknown
parse_field(FieldTuple) ->
    ehttph_parse:parse_field(FieldTuple).

%%% @doc  determines the field's value as prescribed by RFC2616
%%%       repeated (and repeatable) field's values are combined in a list
-spec field_value(atom() | binary() | string(),
		  [{atom() | binary(), binary()}]) ->
    (binary() | any() | [any()]).

field_value(Name, Fields) when is_list(Name) ->
    field_value(ehttph_parse:name_to_atom(list_to_binary(Name)), Fields);
field_value(Name, Fields) when is_atom(Name) orelse is_binary(Name) ->
    HasListValue = has_list_value(Name),
    case [V || {K, V} <- Fields, K =:= Name] of
	[] when not HasListValue ->
	    undefined;
	[Value] when not HasListValue ->
	    {_, ParsedValue} = parse_field({Name, Value}),
	    ParsedValue;
	Values ->
	    lists:flatten( [element(2,parse_field({Name, Value}))
			    || Value <- Values] )
    end.
	    
%% @doc  Determines if a fieldname has a value of the list() type.
%%       The absence of such fields should be interpreted as an empty list.
%%       It is used by igweb_api:field_value/2 to appropriately return [] instead
%%       of undefined.
%% @spec (field_name()) -> true | false
has_list_value('Cache-Control')     -> true;
has_list_value('Connection')        -> true;
has_list_value('Pragma')            -> true;
has_list_value('Trailer')           -> true;
has_list_value('Transfer-Encoding') -> true;
has_list_value('Upgrade')           -> true;
has_list_value('Via')               -> true;
has_list_value('Warning')           -> true;
has_list_value('Accept')            -> true;
has_list_value('Vary')              -> true;
has_list_value('Accept-Ranges')     -> true;
has_list_value('Accept-Charset')    -> true;
has_list_value('Accept-Encoding')   -> true;
has_list_value('Accept-Language')   -> true;
has_list_value('Expect')            -> true;
has_list_value('If-Match')          -> true;
has_list_value('If-None-Match')     -> true;
has_list_value('TE')                -> true;
has_list_value('Allow')             -> true;
has_list_value('Content-Encoding')  -> true;
has_list_value('Content-Language')  -> true;
has_list_value(_Other)              -> false.

%% The header field type of a header
%% @spec (atom()) -> general | request | response | entity
header_field_type('Cache-Control')       -> general;
header_field_type('Connection')          -> general;
header_field_type('Date')                -> general;
header_field_type('Pragma')              -> general;
header_field_type('Trailer')             -> general;
header_field_type('Transfer-Encoding')   -> general;
header_field_type('Upgrade')             -> general;
header_field_type('Via')                 -> general;
header_field_type('Warning')             -> general;

header_field_type('Accept')              -> request;
header_field_type('Accept-Charset')      -> request;
header_field_type('Accept-Encoding')     -> request;
header_field_type('Accept-Language')     -> request;
header_field_type('Authorization')       -> request;
header_field_type('Expect')              -> request;
header_field_type('From')                -> request;
header_field_type('Host')                -> request;
header_field_type('If-Match')            -> request;
header_field_type('If-Modified-Since')   -> request;
header_field_type('If-None-Match')       -> request;
header_field_type('If-Range')            -> request;
header_field_type('If-Unmodified-Since') -> request;
header_field_type('Max-Forwards')        -> request;
header_field_type('Proxy-Authorization') -> request;
header_field_type('Range')               -> request;
header_field_type('Referer')             -> request;
header_field_type('TE')                  -> request;
header_field_type('User-Agent')          -> request;

header_field_type('Accept-Ranges')       -> response;
header_field_type('Age')                 -> response;
header_field_type('Allow')               -> response;
header_field_type('ETag')                -> response;
header_field_type('Location')            -> response;
header_field_type('Proxy-Authenticate')  -> response;
header_field_type('Retry-After')         -> response;
header_field_type('Server')              -> response;
header_field_type('Vary')                -> response;
header_field_type('WWW-Authenticate')    -> response;

header_field_type('Content-Encoding')    -> entity;
header_field_type('Content-Language')    -> entity;
header_field_type('Content-Length')      -> entity;
header_field_type('Content-Location')    -> entity;
header_field_type('Content-MD5')         -> entity;
header_field_type('Content-Range')       -> entity;
header_field_type('Content-Type')        -> entity;
header_field_type('Expires')             -> entity;
header_field_type('Last-Modified')       -> entity;

%% unknown header fields are considered Entity Header Fields
%% but this is to distinguish between them and those defined in RFC2616
header_field_type(_Name) when is_binary(_Name);is_list(_Name) ->
    unknown.
    
    
