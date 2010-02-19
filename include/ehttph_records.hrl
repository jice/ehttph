%% @author JC Sargenton <jice@igwan.eu>
%% @copyright 2010 JC Sargenton
%%
%% @doc ehttph HTTP header fields library
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-type bin()    :: binary().
-type qvalue() :: float().
-type md5_binary() :: binary(). % the parsed MD5 as a 16 bytes binary 
-type date()   :: {{Y::integer(),M::integer(),D::integer()},
		   {H::integer(),Min::integer(),S::integer()}}.

-record(param, {
	  name  :: bin(),
	  value :: bin()
	 }).

-record(element, {
	  name        :: bin(),
	  value       :: bin(),
	  params = [] :: [#param{}]
	 }).

-record(comment, {
	  value = [] :: [bin() | #comment{}]
	  }).

-record(product, {
	  name    :: bin(),
	  version :: bin()
	 }).

-record(via_value, {
	  protocol_name    :: bin(),
	  protocol_version :: bin(),
	  received_by      :: bin() | {bin(), integer()},
	  comment          :: #comment{}
	 }).

-record(warning_value, {
	  code  :: integer(),
	  agent :: bin() | {bin(), integer()},
	  text  :: bin(),
	  date  :: date()
	 }).

-record(media_range, {
	  type    :: '*' | bin(),
	  subtype :: '*' | bin()
	 }).

-record(language_range, {
	  tag    :: '*' | bin(),
	  subtag :: '*' | bin()
	  }).

-record(accept_element, {
	  range             :: '*' | bin() | #language_range{} | #media_range{},
	  range_params = [] :: [#param{}],
	  qvalue            :: qvalue(),
	  ext_params = []   :: [#param{}]
	 }).

-record(entity_tag, {
	  weak = false :: boolean(),
	  opaque       :: bin()
	 }).

-record(byte_range_spec, {
	  first :: integer(),
	  last  :: integer()
	 }).


-record(byte_content_resp_spec,	{
	  first  :: integer(),
	  last   :: integer(),
	  length :: integer()
	 }).

-record(media_type, {
	  type        :: bin(),
	  subtype     :: bin(),
	  params = [] :: [#param{}]
	 }).




