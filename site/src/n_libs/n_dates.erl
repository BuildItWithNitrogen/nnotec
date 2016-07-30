%% -*- mode: nitrogen -*-
%%% -----------------------------------------------
%%% Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc date functions 
%%% @end
%%% -----------------------------------------------

-module (n_dates).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("n_records.hrl").

   

datepicker(ID, Text) -> 
   #datepicker_textbox { 
      id=ID, 
      text=Text, 
      options=[ 
         {dateFormat, "mm/dd/yy"}, 
         {showButtonPanel, true} 
      ] 
    }.

date_span(DateTime, N) ->
   EarlyDate = qdate:add_unit(day, -N, DateTime),
   StartSpan = qdate:to_date(EarlyDate),
   LastDate = qdate:add_unit(day, N, DateTime),
   EndSpan = qdate:to_date(LastDate),
   {StartSpan, EndSpan}.

date_within(Record, DateSpan) ->
   DateTime = nnote_api:date(Record),
   {StartSpan, EndSpan} = DateSpan,
   Flag1 = DateTime >= StartSpan,
   Flag2 = DateTime =< EndSpan,
   Flag1 and Flag2.


today() ->
   UT = calendar:universal_time(),
   LT = calendar:universal_time_to_local_time(UT),
   {Date, _} = LT,
   {Y, M, D} = Date,
   Y1 = integer_to_list(Y),
   M1 = integer_to_list(M),
   D1 = integer_to_list(D),
   M1 ++ "-" ++ D1 ++ "-" ++ Y1.


