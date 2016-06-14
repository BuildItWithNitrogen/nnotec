%% -*- mode: nitrogen -*-
%%% -----------------------------------------------
%%% Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc nnote 
%%% @end
%%% -----------------------------------------------

-module (nnote).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros 
%% ***************************************************

-define(PAGE, "/nnote").
-define(TEMPLATE, "./site/templates/n_apps.html").
-define(TITLE, "nnote").
-define(TOP, "nnote").
-define(MMSELECTED, "nnote").
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
   wf:replace(sidebar, show_side_panel(PageState)),
   wf:replace(content, show_content(PageState)),
   [ #p {} ].

%% ***************************************************
%% Page state functions 
%% ***************************************************

get_page_state() ->
  List = [wf:q(Var) || Var <- ?UVARS],
  list_to_tuple(List).

%% ***************************************************
%% Sidebar executive 
%% ***************************************************

show_side_panel({_, undefined, undefined}) ->
   [ #panel {id = sidebar, body =
     [ #h3 {text="SELECT"},
       show_menus(menus(), undefined)
     ]
   }];

show_side_panel({_, NoteType, undefined}) ->
   [ #panel {id = sidebar, body =
     [ #h3 {text="SELECT"},
       show_menus(menus(), NoteType)
     ]
   }].

show_menus(MenuList, Selected) ->
   [show_menu(Menu, Selected) || Menu <- MenuList].


show_menu(Menu, Selected) ->
   MenuGroup = menu_group(Menu),
   [ #h4 {class=select, text=Menu},
         [n_menus:show_menu_item(MenuItem, Selected) || MenuItem <- MenuGroup]
   ].

%% ***************************************************
%% Sidebar menus 
%% ***************************************************

menus() ->
   ["NOTE TYPE"].

menu_group("NOTE TYPE") ->
   [{"conference", {select, "conference"}},
    {"idea", {select, "idea"}},
    {"interview", {select, "interview"}},
    {"lab", {select, "lab"}},
    {"lecture", {select, "lecture"}},
    {"research", {select, "research"}},
    {"web", {select, "web"}}
   ].

%% ***************************************************
%% Content executives 
%% ***************************************************

show_content({undefined, undefined, undefined}) ->
   [#panel {id = content, body = 
       [ #h2 {class=content, text="My Notes"}] 
   }];

show_content({undefined, NoteType, undefined}) ->
   ButtonText = ["Add new ", NoteType, " note"],
   TextValue = "",
   [ #panel {id = content, body =
      [#h2 {class=content, text="My Notes"},
       search_form(ButtonText, TextValue)
      ]
    }].

%% ***************************************************
%% Content clips
%% ***************************************************

% ***************************************************
%% Content clips - search form 
%% ***************************************************

search_form(ButtonText, TextValue) ->
     [#br {},
      #button {text=ButtonText,
               postback={add_contact, {undefined, undefined}}
              },
      #br {},
      #br {},
      #label {text="enter search words"},
      #textbox {id = search_terms, text=TextValue},
      #button {text="Search",
               postback=search
              },
      #button {text="Info",
               postback={info, search}
              }
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

event({info, Function}) ->
    wf:update(content, info(Function));

%% ***************************************************
%% Sidebar events 
%% ***************************************************

event({main, Link}) ->
   wf:redirect(Link);

event({select, NoteType}) ->
   Redirect = [?PAGE, "?", wf:to_qs([ {note_type, NoteType} ]) ],
   wf:redirect(Redirect).

%% ***************************************************
%% Tips 
%% ***************************************************

tips() ->
  [ #panel {id = content, body =
       [ #h2 {class="content", text = "Tips"},
         #br {},
         #button {text = "done", postback = content}
       ]
   }].

%% ***************************************************
%% Info 
%% ***************************************************

info(search) ->
   [ #panel {id = content, body =
       [ nnote_info_search(),
         #br {}
       ]
   }].


nnote_info_search() ->
   [ #h2 {class=content, body = "<i>Search Words</i>" },
     #br {},
     #button {text = "Done", postback = content}
   ].


 
