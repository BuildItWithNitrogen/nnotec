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
   [{"home | ",    {main, "/"}},
    {"nindex | ",  {main, "/nindex"}},
    {"nnote | " ,  {main, "/nnote"}},
    {"tips",       {main, tips}}
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

show_main_menu_item(MenuItem, Selected) ->
  {Text, {_, Page}} = MenuItem,
   io:format("Text: ~p~n", [Text]),
  Item = extract_menu_item(Text),
   io:format("Menu item: ~p~n", [Item]),
  Class = if_selected(Item, Selected),
  [#link {class = Class, text = Text, postback = {main, Page}}].

if_selected(Text, Selected) ->
   io:format("Selected: ~p ~p~n", [Text, Selected]),
   case Text == Selected of
      true  -> "mmselected" ;
      false -> "mmunselected" 
   end.

extract_menu_item(String) ->
   io:format("I'm extracting! ~p~n", [String]),
   Tokens = string:tokens(String, " "),
   io:format("I've extracted: ~p~n", [Tokens]),
   [Page| _] = Tokens,
   io:format("Page: ~p~n", [Page]),
   Page.



%% ***************************************************
%% Sidebar menus 
%% ***************************************************

% show_menus(MenuList, Selected) ->
%   [show_menu(Menu, Selected) || Menu <- MenuList].


% show_menu(Menu, Selected) ->
%   MenuGroup = nnote:menu_group(Menu),
%   [ #h4 {class=select, text=Menu},
%         [show_menu_item(MenuItem, Selected) || MenuItem <- MenuGroup]
%   ].

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



