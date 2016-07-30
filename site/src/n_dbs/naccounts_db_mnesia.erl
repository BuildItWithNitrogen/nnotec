%% -*- mode: nitrogen -*-
%%% -----------------------------------------------
%%% Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc naccounts 
%%% @end
%%% -----------------------------------------------


-module(naccounts_db_mnesia).

-export([ init_table/0, 
          put_record/1,
          get_record/1, 
          get_all/0,
          attempt_login/2,
          create_account/2
       ]).

-include_lib("stdlib/include/qlc.hrl").
-include("n_records.hrl").

-define(TABLE, naccounts).

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

get_record (Email) ->
    Query =
        fun() ->
            mnesia:read({?TABLE, Email})
        end,
    {atomic, Results} = mnesia:transaction(Query),
    case length(Results) < 1 of
       true  -> [];
       false -> hd(Results)
    end.

get_all() ->
    Query =
    fun() ->
        qlc:eval( qlc:q(
            [ Record || Record <- mnesia:table(?TABLE) ]
        ))
    end,
    {atomic, Results} = mnesia:transaction(Query),
    Results.


attempt_login(UserName, Password) ->
     Record = get_record(UserName),
     case Record of
        [] -> false;
         _ -> PWHash = Record#naccounts.pwhash,
              erlpass:match(Password, PWHash)
     end.

create_account(UserName, Password) ->
   PWHash = erlpass:hash(Password),
   Record = #naccounts{email=UserName, pwhash=PWHash},
   put_record(Record),
   ok. 
