%% -*- mode: nitrogen -*-
%%% -----------------------------------------------
%%% Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc nnote 
%%% @end
%%% -----------------------------------------------


-module(nnote_api).

-export([ init_table/0, 
          put_record/1,
          put_all_values/1,
          new/1,
          get_record/1, 
          get_all_values/1,
          get_all/0, 
          get_records_by_type/2,
          get_records_by_date/3,
          search/3,
          id/1,
          user_id/1,
          date/1,
          type/1,
          event/1,
          source/1,
          topic/1,
          question/1,
          tags/1,
          note/1,
          id/2,
          user_id/2,
          date/2,
          type/2,
          event/2,
          source/2,
          topic/2,
          question/2,
          tags/2,
          note/2
        ]).

-define(DB, nnote_db_mnesia).


init_table() ->
   ?DB:init_table().

put_all_values([UserID, Type, Date, Event, Source, 
              Topic, Question, Tags, Note]) ->
   ?DB:put_all_values([UserID,
                   Type,
                   Date,
                   Event,
                   Source,
                   Topic,
                   Question,
                   Tags,
                   Note
                 ]).

put_record(Record) ->
   ?DB:put_record(Record).

new(Record) -> 
   ?DB:new(Record).

get_record (ID) ->
   ?DB:get_record (ID).

get_all_values(Record) ->
   ?DB:get_all_values(Record).

get_all() ->
   ?DB:get_all().

get_records_by_type(UserID, Type) ->
   ?DB:get_records_by_date(UserID, Type).

get_records_by_date(UserID, Type, Date) ->
   ?DB:get_records_by_date(UserID, Type, Date).

search(UserID, NoteType, SearchList) ->
   ?DB:search(UserID, NoteType, SearchList).

id(Record) ->
   ?DB:id(Record).

user_id(Record) ->
   ?DB:user_id(Record).

date(Record) ->
   ?DB:date(Record).

type(Record) ->
   ?DB:type(Record).

event(Record) ->
   ?DB:event(Record).

source(Record) ->
   ?DB:source(Record).

topic(Record) ->
   ?DB:topic(Record).

question(Record) ->
   ?DB:question(Record).

tags(Record) ->
   ?DB:tags(Record).

note(Record) ->
   ?DB:note(Record).


id(Record, ID) ->
   ?DB:id(Record, ID).

user_id(Record, UserID) ->
   ?DB:user_id(Record, UserID).

date(Record, Date) ->
   ?DB:date(Record, Date).

type(Record, Type) ->
   ?DB:type(Record, Type).

event(Record, Event) ->
   ?DB:event(Record, Event).

source(Record, Source) ->
   ?DB:source(Record, Source).

topic(Record, Topic) ->
   ?DB:topic(Record, Topic).

question(Record, Question) ->
   ?DB:question(Record, Question).

tags(Record, Tags) ->
   ?DB:tags(Record, Tags).

note(Record, Note) ->
   ?DB:note(Record, Note).


