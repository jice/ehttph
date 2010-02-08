-module(ehttph_rfc1123).
%%% Available in httpd_utils (inets) but reimplemented for the following reasons :
%%%  - to not depend on the inets application for one function
%%%  - the httpd_utils version accepts timezones other than "GMT", which is forbidden by RFC2616

-export([parse_date/1, make_date/1, make_date/0]).


%% @doc Returns a RFC1123 formatted date as a iolist
make_date() ->
    parse_date(calendar:now_to_universal_time(now())).

make_date({{Year, Month, Day} = Date, {H, M, S}}) ->
    WkDay = wk_day(calendar:day_of_the_week(Date)),
    MonthName = month_name(Month),
    Args = [WkDay, Day, MonthName, Year, H, M, S],
    io_lib:format("~s, ~2..0B ~s ~4..0B ~2..0B:~2..0B:~2..0B GMT", Args).

%% @doc Parses a RFC1123 date and returns it in Erlang format
parse_date(<<WkDay:3/binary,", ",
		    Day:2/binary," ",Month:3/binary," ",Year:4/binary," ",
		    H:2/binary,":",M:2/binary,":",S:2/binary," GMT">>) ->
    [DayInt, YearInt, HInt, MInt, SInt] = [list_to_integer(binary_to_list(X))
				       || X <- [Day, Year, H, M, S]],
    MonthInt = parse_month_name(Month),
    Date = {YearInt, MonthInt, DayInt},
    ok = valid_date(WkDay, Date),
    Time = {HInt, MInt, SInt},
    ok = valid_time(Time),
    {Date, Time}.
		    
valid_date(WkDay, Date) ->
    DayNumber = parse_wk_day(WkDay),
    DayNumber = calendar:day_of_the_week(Date),    
    ok.

valid_time({H, M, S})
  when H >= 0 andalso H =< 23 andalso
       M >= 0 andalso M =< 59 andalso
       S >= 0 andalso S =< 59 ->
    ok.

wk_day(1) -> "Mon";
wk_day(2) -> "Tue";
wk_day(3) -> "Wed";
wk_day(4) -> "Thu";
wk_day(5) -> "Fri";
wk_day(6) -> "Sat";
wk_day(7) -> "Sun".

parse_wk_day(<<"Mon">>) -> 1;
parse_wk_day(<<"Tue">>) -> 2;
parse_wk_day(<<"Wed">>) -> 3;
parse_wk_day(<<"Thu">>) -> 4;
parse_wk_day(<<"Fri">>) -> 5;
parse_wk_day(<<"Sat">>) -> 6;
parse_wk_day(<<"Sun">>) -> 7.

month_name(1) -> "Jan";
month_name(2) -> "Feb";
month_name(3) -> "Mar";
month_name(4) -> "Apr";
month_name(5) -> "May";
month_name(6) -> "Jun";
month_name(7) -> "Jul";
month_name(8) -> "Aug";
month_name(9) -> "Sep";
month_name(10) -> "Oct";
month_name(11) -> "Nov";
month_name(12) -> "Dec".

parse_month_name(<<"Jan">>) -> 1;
parse_month_name(<<"Feb">>) -> 2;
parse_month_name(<<"Mar">>) -> 3;
parse_month_name(<<"Apr">>) -> 4;
parse_month_name(<<"May">>) -> 5;
parse_month_name(<<"Jun">>) -> 6;
parse_month_name(<<"Jul">>) -> 7;
parse_month_name(<<"Aug">>) -> 8;
parse_month_name(<<"Sep">>) -> 9;
parse_month_name(<<"Oct">>) -> 10;
parse_month_name(<<"Nov">>) -> 11;
parse_month_name(<<"Dec">>) -> 12.

