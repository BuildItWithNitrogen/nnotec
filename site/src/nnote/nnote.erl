%% -*- mode: nitrogen -*-
%%% -----------------------------------------------
%%% Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc nnote 
%%% @end
%%% -----------------------------------------------


-module(nnote).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros 
%% ***************************************************

-define(PATH, "/nnote").
-define(TEMPLATE, "./site/templates/n_apps.html").
-define(MMSELECTED, "nnote").
-define(TITLE, "Welcome to nnote!").
-define(TOP, "nnote").
-define(UVARS, [note_type, task]).
-define(NICKNAME, n_utils:get_nickname()).
-define(ACCESS, private).
-define(USER, wf:user()).


%% ***************************************************
%% Template and title
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

show_sidebar({undefined, undefined}) ->
   [ #panel {id = sidebar, body =
     [ #h3 {text="SELECT"},
       show_menu("NOTE TYPE", unselected)
     ]
   }];

show_sidebar({NoteType, undefined}) ->
   [ #panel {id = sidebar, body =
     [ #h3 {text="SELECT"},
       show_menu("NOTE TYPE", NoteType)
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

menu("NOTE TYPE") ->
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

show_content({undefined, undefined}) ->
   [#panel {id = content, body = 
       [ content_headline(),
         #p {class=content, text="Select note type."}
       ]
   }];

show_content({NoteType, undefined}) ->
   Records = undefined,
   [display_forms(NoteType, Records)];

show_content({NoteType, search_by_tag}) ->
   Records = tag_search(NoteType),
   [display_forms(NoteType, Records)];

show_content({NoteType, search_by_date}) ->
   Records = date_search(NoteType),
   [display_forms(NoteType, Records)].

%% ***************************************************
%% Content
%% ***************************************************

display_forms(NoteType, Records) ->
   [ #panel {id = content, body =
      [content_headline(),
       add_note_button(NoteType),
       search_by_tag(),
       search_by_date(),
       search_results(Records)
      ]
    }].

%% ***************************************************
%% Content helpers
%% ***************************************************

content_headline() ->
   [#h2 {class=content, text="My Notes"}].

add_note_button(NoteType) ->
   ButtonText = ["Enter new ", NoteType, " note"],
   [#button {text=ButtonText,
             postback={add_note, wf:q(note_type)}
            }
   ].

search_by_tag() ->
     [#label {text="enter search words"},
      #textbox {id = search_words},
      #button {text="Search", postback=search_by_tag},
      #button {text="Info", postback={info, search_by_tag}
     }].

search_by_date() ->
   [ #label {text="enter date"},
     n_dates:datepicker(search, ""),
     #button {text="Search",
              postback = search_by_date
             },
     #button {text="Info",
              postback={info, search_by_date}
             }
    ].

tag_search(NoteType) ->
   UserID = n_utils:get_user_id(),
   SearchList = wf:q(search_words),
   Records = nnote_api:search(UserID, NoteType, SearchList),
   lists:sort(fun n_utils:compare/2, Records).


date_search(NoteType) ->
   UserID = n_utils:get_user_id(),
   Date = wf:q(search),
   Records = nnote_api:get_records_by_date(UserID, NoteType, Date),
   lists:sort(fun n_utils:compare/2, Records).
 

search_results(undefined) ->
    [#p {}
    ];

search_results([]) ->
    [ #hr {},
     #h2 {class = content, text="Search Results"},
     #p {text = "No notes found"}
    ];

search_results(Records) ->
    [#hr {},
     #h2 {class = content, text="Search Results"},
     [n_utils:draw_link(Record) || Record <- Records]
    ]. 
   


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


event(search_by_tag) ->
   NoteType = wf:q(note_type),
   wf:replace(content, show_content({NoteType, search_by_tag}));

event(search_by_date) ->
   NoteType = wf:q(note_type),
   wf:replace(content, show_content({NoteType, search_by_date}));
   

%% ***************************************************
%%  Info events 
%% ***************************************************

event({info, Function}) ->
    wf:update(content, info(Function));

%% ***************************************************
%% Sidebar events 
%% ***************************************************

event({select, NoteType}) ->
   Redirect = [?PATH, "?", wf:to_qs([ {note_type, NoteType} ]) ],
   wf:redirect(Redirect);

%% ***************************************************
%% Content events 
%% ***************************************************

event({add_note, NoteType}) ->
   Redirect = ["/nnote/add_edit", "?", wf:to_qs([ {id, "new"}, {note_type, NoteType} ]) ],
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

info(search_by_tag) ->
   [ #panel {id = content, body =
       [ search_by_tag_info(),
         #br {}
       ]
   }];

info(search_by_date) ->
   [ #panel {id = content, body =
       [ search_by_date_info(),
         #br {}
       ]
   }].

search_by_tag_info() ->
   [ #h2 {class=content, body =
           "<i>Search Words</i>" },
     #h3 {class=content, text="Format"},
     #p {class=content, text = 
          "word1 Word2 ... word3"},    
     #h3 {class=content, text = "Note"},
     #p {class=content, text =
             "Search words are case sensitive"},
     #br {},
     #button {text = "Done", postback = content}
   ].

search_by_date_info() ->
   [ #h2 {class=content, body =
           "<i>Search By Date</i>" },
     #h3 {class=content, text = "Date Format"},
     #p {class=content, text = "mm/dd/yyyy."},
     #h3 {class=content, text = "Results"},
     #p {class=content, text = "mm/dd/yyyy plus or minus seven days."},
     #br {},
     #button {text = "Done", postback = content}
   ].

 
