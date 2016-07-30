%% -*- mode: nitrogen -*-
%%% -----------------------------------------------
%%% Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc menus 
%%% @end
%%% -----------------------------------------------



-module (n_menus).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").


%% ***************************************************
%% Main Menu 
%% ***************************************************

main_menu() ->
   [{"home | ",        {main, "/"}},
    {"nindex | ",      {main, "/nindex"}},
    {"nnote | " ,      {main, "/nnote"}},
    {"tips & info | ", {main, tips}},
    {"logout",         {main, logout}}
   ].

%% ***************************************************
%% Main Menu Executive 
%% ***************************************************

show_main_menu(Selected) ->
   MenuList = main_menu(),
   [ #panel {id = main_menu, body =
     [show_main_menu_item(MenuItem, Selected) || MenuItem <- MenuList]
   }].

%% ***************************************************
%% Main Menu Helpers 
%% ***************************************************

% show_main_menu_item({"logout", {main, "/"}}, _) ->
%  wf:clear_user(),
%  [#link {class = "mm", text="logout", url="/"}];

show_main_menu_item(MenuItem, Selected) ->
  io:format("Show main - user: ~p~n", [wf:user()]),
  {Text, Postback} = MenuItem,
  Item = strip_space_bar(Text),
  Class = if_selected(Item, Selected),
  [#link {class = Class, text = Text, postback = Postback}].


strip_space_bar(String) ->
   Offset = string:str(String, " | "),
   case Offset > 0 of
      true  -> string:left(String, Offset - 1);
      false -> String
   end.

if_selected(Text, Selected) ->
   case Text == Selected of
      true  -> "mmselected" ;
      false -> "mm" 
   end.

%% ***************************************************
%% Sidebar menus 
%% ***************************************************


show_menu_item(MenuItem, Selected) ->
  {Text, Postback} = MenuItem,
  [#radio{
           name=Text,
           text=Text,
           checked = checked(Text, Selected),
           value=Text,
           postback=Postback
         },
   #br {}
 ].

%% @doc Return true if selection checked; else false
checked(Text, Selected) ->
   case Text == Selected of
      true  -> true;
      false -> false
   end.



