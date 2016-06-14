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

-define(PAGE, "/").
-define(TEMPLATE, "./site/templates/n_apps.html").
-define(TITLE, "Welcome!").
-define(TOP, "Build it with Nitrogen").
-define(MMSELECTED, "home").
-define(USERNAME, "Rusty Nail").
-define(UVARS, [id, note_type, task]).

%% ***************************************************
%% Template 
%% ***************************************************

main() -> #template { file=?TEMPLATE}. % "./site/templates/wg.html" }.

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
          update_page()
        ]
   }].

%% ***************************************************
%% Update page 
%% ***************************************************

update_page() ->
   PageState = get_page_state(),
   wf:replace(main_menu, n_menus:show_main_menu(?MMSELECTED)),
%   wf:replace(sidebar, show_side_panel(PageState)),
   wf:replace(content, show_content(PageState)),
   [ #p {} ].

%% ***************************************************
%% Page state functions 
%% ***************************************************

get_page_state() ->
  List = [wf:q(Var) || Var <- ?UVARS],
  list_to_tuple(List).

%% ***************************************************
%% Sidebar executives 
%% ***************************************************

%% ***************************************************
%% Sidebar menus 
%% ***************************************************

%% ***************************************************
%% Content executives 
%% ***************************************************

show_content({undefined, undefined, undefined}) ->
   [ #panel {id = content, body = 
        [welcome()] 
   }].

%% ***************************************************
%% Content clips
%% ***************************************************

welcome() ->
   [#h2 {class=content, text=["Welcome to ", ?USERNAME , "\'s ", "Nitrogen Applications!"]},
    #p {body = "Our motto: <em>\"Build it Fast with Nitrogen\"</em>"}
   ]. 

%% ***************************************************
%% Top menu events 
%% ***************************************************


event({main, tips}) ->
   wf:update(main_menu, n_menus:show_main_menu("tips")),
   wf:update(content, tips());

event(content) ->
   PageState = get_page_state(),
   wf:update(main_menu, n_menus:show_main_menu(?MMSELECTED)),
   wf:update(content, show_content(PageState));

%% ***************************************************
%%  Info events 
%% ***************************************************

%% ***************************************************
%% Sidebar events 
%% ***************************************************

event({main, Link}) ->
   wf:redirect(Link).

%% ***************************************************
%% Tips 
%% ***************************************************

tips() ->
   [ #panel {id = content, body =
       [ #h2 {class="content", text="Tips"},
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



 
