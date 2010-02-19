%% @author JC Sargenton <jice@igwan.eu>
%% @copyright 2010 JC Sargenton
%%
%% @doc ehttph HTTP header fields library
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

%% character classes

-define(DIGIT(X), $0 =< X andalso X =< $9).

-define(ALPHA(X), (?LOALPHA(X)) orelse (?UPALPHA(X))).

-define(LOALPHA(X), $a =< X andalso X =< $z).

-define(UPALPHA(X), $A =< X andalso X =< $Z).

-define(DQUOTE, 34).

-define(BACKSLASH, $\\).

-define(COMMA, 44).

-define(OPAR, $().

-define(CPAR, $)).

-define(SP, 32).

-define(HT, 9).

-define(CR, 13).

-define(LF, 10).

-define(NUL, 0).

-define(DEL, 127).

-define(SP_or_HT(X), X =:= ?SP orelse X =:= ?HT).

-define(TCHAR(X),
	X =:= $! orelse
	X =:= $# orelse
	X =:= $$ orelse
	X =:= 37 orelse % percent
	X =:= $& orelse
	X =:= 39 orelse % single quote
	X =:= $* orelse
	X =:= $+ orelse
	X =:= $- orelse
	X =:= 46 orelse % dot
	X =:= $^ orelse
	X =:= $_ orelse
	X =:= $` orelse
	X =:= $| orelse
	X =:= $~ orelse
	X =:= $# orelse
	(?DIGIT(X)) orelse (?ALPHA(X))).

-define(SEPARATOR(X),
	X =:= $( orelse
	X =:= $) orelse
	X =:= $< orelse
	X =:= $> orelse % percent
	X =:= $@ orelse
	X =:= $, orelse % single quote
	X =:= $; orelse
	X =:= $: orelse
	X =:= $\ orelse
	X =:= ?DQUOTE orelse % dot
	X =:= $/ orelse
	X =:= $[ orelse
	X =:= $] orelse
	X =:= $? orelse
	X =:= $= orelse
	X =:= ${ orelse
	X =:= $} orelse
	X =:= ?SP orelse
	X =:= ?HT).

-define(CHAR(X), X >= 0 andalso X =< 127 andalso
	X =/= ?CR andalso X =/= ?LF andalso X =/= ?NUL).

% any TEXT excluding DEL
-define(TEXT(X),
	X >= 16#20 andalso
	X =< 16#FF andalso
	X =/= ?DEL).	

-define(CRLF, "\r\n").

