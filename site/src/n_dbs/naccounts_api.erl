%% -*- mode: nitrogen -*-
%%% -----------------------------------------------
%%% Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc naccounts 
%%% @end
%%% -----------------------------------------------



-module(naccounts_api).

-export([ init_table/0, 
          put_record/1,
          get_record/1,
          get_all/0, 
          attempt_login/2,
          create_account/2
        ]).

-define(DB, naccounts_db_mnesia).


init_table() ->
   ?DB:init_table().

put_record(Record) ->
   ?DB:put_record(Record).

get_record(Email) ->
   ?DB:get_record(Email).

get_all() ->
   ?DB:get_all().

attempt_login(UserName, Password) ->
   ?DB:attempt_login(UserName, Password).

create_account(UserName, Password) ->
   ?DB:create_account(UserName, Password). 
    
