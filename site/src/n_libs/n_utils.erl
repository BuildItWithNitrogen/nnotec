%% -*- mode: nitrogen -*-
%%% -----------------------------------------------
%%% Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc Utility functions 
%%% @end
%%% -----------------------------------------------

-module (n_utils).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

create_id() ->
   Rand = crypto:rand_uniform(100,999),
   DateTime = calendar:universal_time(),
   Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
   MegaSeconds = Seconds * 1000 + Rand,
   integer_to_list(MegaSeconds, 16).

id_created(ID) ->
   N = list_to_integer(ID, 16),
   I = N div 1000,
   calendar:gregorian_seconds_to_datetime(I).
 
get_user_id() -> "123".

get_nickname() -> "Marsha".

initial_cap(String) ->
   [First | Rest] = String,
   Cap = string:to_upper(First),
   [Cap | Rest].

draw_link(Record) ->   
   ID = nnote_api:id(Record),
   NoteType = nnote_api:type(Record),
   DateTime = nnote_api:date(Record),
   Date = qdate:to_string("m/d/Y", DateTime),
   Topic = nnote_api:topic(Record),   
   EditUrl =[ "/nnote/add_edit?", wf:to_qs([{id, ID}, {note_type, NoteType}])] ,
   Menuid = wf:temp_id(),
   [    #link {
        body = [Date, " ", "&#8212;", " ", Topic], 
        click=#toggle{target=Menuid}
      },
      #panel{id=Menuid, style="display:none", body=[
        #link {text="edit", url=EditUrl},
        " | ",
        #link {text="delete", postback={delete, ID}}
      ]},
      #br {}
   ].


compare(Record1, Record2) ->
  nnote_api:date(Record1) < nnote_api:date(Record2).


