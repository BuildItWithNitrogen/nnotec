%% -*- mode: nitrogen -*-
%%% -----------------------------------------------
%%% Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc nindex 
%%% @end
%%% -----------------------------------------------

-module (nindex).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros 
%% ***************************************************

-define(PAGE, "/nnindex").
-define(TEMPLATE, "./site/templates/n_apps.html").
-define(TITLE, "nnindex").
-define(TOP, "nindex").
-define(MMSELECTED, "nindex").
-define(USERNAME, "Jesse").
-define(UVARS, [id, note_type, task]).

%% ***************************************************
%% Template 
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
   ID        = wf:q(id),
   NoteType  = wf:q(note_type),
   Task      = wf:q(task),
   {ID, NoteType, Task}.


%% ***************************************************
%% Sidebar executive 
%% ***************************************************

%% ***************************************************
%% Sidebar 
%% ***************************************************

%% ***************************************************
%% Sidebar menus 
%% ***************************************************

%% ***************************************************
%% Content executives 
%% ***************************************************

show_content({undefined, undefined, undefined}) ->
   [ #panel {id = content, body = 
        [start_state()] 
   }].

%% ***************************************************
%% Content clips
%% ***************************************************

start_state() ->
   [#h2 {class=content, text=["My Favorite Web Links"]}
   ]. 

%% ***************************************************
%% Top menu events 
%% ***************************************************

event({main, tips}) ->
   wf:update(main_menu, n_menus:show_main_menu("tips")),
   wf:update(content, tips());

event(content) ->
   wf:update(main_menu, n_menus:show_main_menu(?MMSELECTED)),
   PageState = get_page_state(),
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



 
