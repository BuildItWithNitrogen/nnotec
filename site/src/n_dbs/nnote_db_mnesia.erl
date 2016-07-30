%% -*- mode: nitrogen -*-
%%% -----------------------------------------------
%%% Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc nnote 
%%% @end
%%% -----------------------------------------------


-module(nnote_db_mnesia).

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
          delete/1, 
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


-include_lib("stdlib/include/qlc.hrl").
-include("n_records.hrl").

-define(TABLE, nnote).


init_table() ->
    mnesia:create_table(?TABLE,
        [ {disc_copies, [node()] },
          {attributes, record_info(fields, ?TABLE)} 
        ]).

put_record(Record) ->
    Insert =
       fun() ->
           mnesia:write(Record)
       end,
       {atomic, Results} = mnesia:transaction(Insert),
       Results.

put_all_values([UserID, Type, Date, Event, Source, Topic, Question, Tags, Note]) ->
    % Note date format mm/dd/yyyy is converted to Erlang DateTime
      #nnote{user_id  = UserID,
             type     = Type,
             date     = qdate:to_date(Date),
             event    = Event,
             source   = Source,
             topic    = Topic,
             question = Question,
             tags     = Tags,
             note     = Note
            }.


new(Record) ->
    put_record(Record).

get_record (ID) ->
    Query =
        fun() ->
            mnesia:read({?TABLE, ID})
        end,
    {atomic, Results} = mnesia:transaction(Query),
    case length(Results) < 1 of
       true  -> [];
       false -> hd(Results)
    end.

get_all_values(Record) ->
   % Note Erlang datetime converted to "mm/dd/yyyy"
   DateTime = date(Record),
   Date = qdate:to_string("m/d/Y", DateTime),
   Record1 = date(Record, Date),
   [_|Tail] = tuple_to_list(Record1),
   Tail.

get_all() ->
    Query =
    fun() ->
        qlc:eval( qlc:q(
            [ Record || Record <- mnesia:table(?TABLE) ]
        ))
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.

get_records_by_type(UserID, Type) ->
   Query =     
      fun() ->         
         qlc:eval( qlc:q(
            [Record || Record <- mnesia:table(?TABLE), 
                                   Record#nnote.id == UserID,
                                   Record#nnote.type == Type]
       ))
    end,    
    {atomic, Results} = mnesia:transaction(Query),     
    Results.


get_records_by_date(UserID, Type, Date) ->
   DateTime = qdate:to_date(Date),
   DateSpan = n_dates:date_span(DateTime, 7),
   Query =     
      fun() ->         
         qlc:eval( qlc:q(
	    [Record || Record <- mnesia:table(?TABLE), 
                                 n_dates:date_within(Record, DateSpan),
			         Record#nnote.user_id == UserID,
                                 Record#nnote.type >= Type
             ]))
    end,    
    {atomic, Results} = mnesia:transaction(Query),     
    Results.


search(UserID, NoteType, SearchList) ->
   Query =     
      fun() ->         
         qlc:eval( qlc:q(
            [Record || Record <- mnesia:table(?TABLE),
                                    Record#nnote.user_id == UserID,
                                    Record#nnote.type == NoteType,
                                    n_search:filter(SearchList, Record)]
                                   
       ))
    end,    
    {atomic, Results} = mnesia:transaction(Query),     
    Results.


delete(ID) ->
    Insert =
       fun() ->
           mnesia:delete({?TABLE, ID})
       end,
       {atomic, Results} = mnesia:transaction(Insert),
       Results.


%% GET

id(Record) ->
   Record#nnote.id.

user_id(Record) ->
   Record#nnote.user_id.

date(Record) ->
   Record#nnote.date.

type(Record) ->
   Record#nnote.type.

event(Record) ->
  Record#nnote.event.

source(Record) ->
   Record#nnote.source.

topic(Record) ->
   Record#nnote.topic.

question(Record) ->
   Record#nnote.question.

tags(Record) ->
   Record#nnote.tags.

note(Record) ->
   Record#nnote.note.

%% PUT

id(Record, ID) ->
   Record#nnote{id=ID}.

user_id(Record, UserID) ->
   Record#nnote{user_id=UserID}.

date(Record, Date) ->
   Record#nnote{date=Date}.

type(Record, Type) ->
   Record#nnote{type=Type}.

event(Record, Event) ->
  Record#nnote{event=Event}.

source(Record, Source) ->
   Record#nnote{source=Source}.

topic(Record, Topic) ->
   Record#nnote{topic=Topic}.

question(Record, Question) ->
   Record#nnote{question=Question}.

tags(Record, Tags) ->
   Record#nnote{tags=Tags}.

note(Record, Note) ->
   Record#nnote{note=Note}.



