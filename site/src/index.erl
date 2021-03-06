%% -*- mode: nitrogen -*-
%%% -----------------------------------------------
%%% Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc index 
%%% @end
%%% -----------------------------------------------

-module (index).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros 
%% ***************************************************

-define(PATH, "/").
-define(TEMPLATE, "./site/templates/n_apps.html").
-define(MMSELECTED, "home").
-define(TITLE, "Welcome!").
-define(TOP, "Build it with Nitrogen").
-define(UVARS, [id, note_type, task]).
-define(NICKNAME, n_utils:get_nickname()).
-define(ACCESS, private).
-define(USER, wf:user()).

%% ***************************************************
%% Template and Title 
%% ***************************************************

main() -> #template { file=?TEMPLATE}. 

title() -> ?TITLE.

%% ***************************************************
%% Panel definitions 
%% ***************************************************

top() ->
   [ #h1 {text=?TOP} 
   ].

main_menu() ->
   [ #panel {body = 
        [ #p {id = main_menu}]
   }].

sidebar() ->
   [ #panel {body = 
        [ #p {id = sidebar}]
   }].

content() ->
   [ #panel {body = 
        [ #p {id = content},
          open_sesame(?ACCESS)
        ]
   }].

%% ***************************************************
%% Page state functions
%% ***************************************************

get_page_state() ->
  List = wf:mq(?UVARS),
  list_to_tuple(List).


open_sesame(public)  -> 
   show_page();

open_sesame(private) -> 
   case ?USER == undefined of
      true  -> wf:redirect("/n_signin");
      false -> show_page()
   end.

show_page() ->
   PageState = get_page_state(),
   wf:replace(main_menu, n_menus:show_main_menu(?MMSELECTED)),
   wf:replace(sidebar, show_sidebar(PageState)),
   wf:replace(content, show_content(PageState)),
   [ #p {} ].


%% ***************************************************
%% Sidebar executives 
%% ***************************************************

show_sidebar({undefined, undefined, undefined}) ->
   [ #panel {id = sidebar, body =
     [ #h3 {text="SELECT"},
       show_menu("WEB SITE", unselected)
     ]
   }].

%% ***************************************************
%% Sidebar functions 
%% ***************************************************

show_menu(Menu, Selected) ->
   [ #h4 {class=select, text=Menu},
         [n_menus:show_menu_item(MenuItem, Selected) || MenuItem <- menu(Menu)]
   ].


%% ***************************************************
%% Sidebar menus 
%% ***************************************************

menu("WEB SITE") ->
   [{"nitrogen", {goto, "http://nitrogenproject.com/"}},
    {"erlang", {goto, "http://erlang.org/doc/apps/stdlib/"}},
    {"hacker news", {goto, "https://news.ycombinator.com/"}}
   ].

%% ***************************************************
%% Content executives 
%% ***************************************************

show_content({undefined, undefined, undefined}) ->
   [ #panel {id = content, body = 
        [greeting()] 
   }].

%% ***************************************************
%% Content 
%% ***************************************************

greeting() ->
   [#h2 {class=content, text=["Welcome to ", ?NICKNAME , "\'s ", "Nitrogen Applications!"]},
    #p {body = "Our motto: <em>\"Build it Fast with Nitrogen\"</em>"}
   ]. 

%% ***************************************************
%% Content helpers
%% ***************************************************


%% ***************************************************
%% Main menu events 
%% ***************************************************

event({main, tips}) ->
   wf:update(content, tips());

event({main, logout}) ->
   wf:clear_user(),
   wf:redirect("/");

event({main, Link}) ->
   wf:redirect(Link);

%% ***************************************************
%% Content events 
%% ***************************************************

event(content) ->
   PageState = get_page_state(),
   wf:update(content, show_content(PageState));

%% ***************************************************
%%  Info events 
%% ***************************************************

%% ***************************************************
%% Sidebar events 
%% ***************************************************

event({goto, Link}) ->
   wf:redirect(Link).

%% ***************************************************
%% Tips 
%% ***************************************************


tips() ->
   [ #panel {id = content, body =
       [ #h2 {class="content", text="Tips & Info"},
         #p {body="The applications in this framework were developed by Jesse Gumm and Lloyd R. Prentice for their book <em>Build it with Nitrogen</em>. These applications are available for use and modification under the MIT License."},
         #br {},
         #button {text = "done", postback = content}
       ]
   }].

%% ***************************************************
%% Info 
%% ***************************************************

info(none) ->
   [ #panel {id = content, body =
       [ no_info(),
         #br {}
       ]
   }].


no_info() ->
   [ #h2 {class=content, body = "<i>No info todays</i>" },
     #button {text = "Done", postback = content}
   ].



 
