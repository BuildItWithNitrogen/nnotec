%% -*- mode: nitrogen -*-

%%% -----------------------------------------------
%%% Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc nnote add/edit 
%%% @end
%%% -----------------------------------------------


-module(nnote_add_edit).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros 
%% ***************************************************

-define(PATH, "/nnote/add_edit").
-define(TEMPLATE, "./site/templates/n_apps.html").
-define(MMSELECTED, "nnote").
-define(TITLE, "Add/edit note").
-define(TOP, "nnote").
-define(UVARS, [id, note_type, task]).
-define(NICKNAME, n_utils:get_nickname()).
-define(ACCESS, private).
-define(USER, wf:user()).


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


show_sidebar({_ID, NoteType, _}) ->
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

show_content({undefined, undefined, undefined}) ->
   [#panel {id = content, body = 
       [ #h2 {class=content, text="My Notes"}] 
   }];

show_content({"new", NoteType, undefined}) ->
   [ #panel {id = content, body =
      [ content_headline("new", NoteType),
        add_edit_form("new", NoteType)]
    }];

show_content({ID, NoteType, undefined}) ->
   [ #panel {id = content, body =
       [ content_headline(ID, NoteType),
         add_edit_form(ID, NoteType) ]
    }].


%% ***************************************************
%% Content
%% ***************************************************

content_headline("new", NoteType) ->
    NoteType1 = n_utils:initial_cap(NoteType),
    [#h2 {class=content, text=["Enter", " ",NoteType1, " Note"]} ];

content_headline(_ID, NoteType) ->
    NoteType1 = n_utils:initial_cap(NoteType),
    [#h2 {class=content, text=["Edit", " ",NoteType1, " Note"]} ].


add_edit_form("new", NoteType) ->
   UserID = n_utils:get_user_id(),
   Today = calendar:universal_time(),
   Date = qdate:to_string("m/d/Y", Today),
   form(["", UserID, Date, NoteType, 
          "", "", "", "", "", ""])
   ;

add_edit_form(ID, _NoteType) ->
   Record = nnote_api:get_record(ID), 
   Values = nnote_api:get_all_values(Record),
   form(Values).

form([ID, UserID, Date, NoteType, Event, Source, 
      Topic, Question, Tags, Note]) ->
    wf:defer(save_note, topic, #validate{validators=[
          #is_required{text="Topic required"}]}),
    wf:defer(save_note, note, #validate{validators=[
          #is_required{text="Note required"}]}),
    ?WF_IF(show_event(NoteType),
       wf:defer(save_note, event, #validate{validators=[
            #is_required{text="Event required"}]})
    ),
    ?WF_IF(show_source(NoteType),
       wf:defer(save_note, source, #validate{validators=[
            #is_required{text="Source required"}]})
    ),
    [ #hidden {id=note_id, text = ID},
      #hidden {id=user_id, text = UserID},
      #hidden {id=n_type, text = NoteType},
      #label {text="date"},
      n_dates:datepicker(date, Date),
      #label {text=event_label(NoteType)},
      #textbox{id=event, text=Event, show_if=show_event(NoteType)},
      #label {text=source_label(NoteType)},
      #textbox {id=source, text=Source, show_if=show_source(NoteType)},
      #label {text=topic},
      #textbox {id=topic, text=Topic},
      #label {text=question_label(NoteType)},
      #textbox {id=question, text=Question, show_if=show_question(NoteType)},
      #label {text="search words: word1 word2 Word3 ..."},    
      #textbox {id = tags, text=Tags},
      #button {text="Info", postback={info, search_by_tag}},
      #br {},
      #label {text="note"},
      #textarea{id=note, text=Note, rows=5, columns=40},
      #br{},
      #button {id=save_note, text=button_text(ID),
               postback=save_note},
      #button{text="cancel", postback=cancel}
     ].
      

%% ***************************************************
%% Content helpers 
%% ***************************************************


button_text("new") ->
   "Enter new note";

button_text(_ID) ->
   "Submit changes".


save("new") ->
   Params = wf:mq([user_id, n_type, date, event, source,
                  topic, question, tags, note]),
   Record = nnote_api:put_all_values(Params),
   nnote_api:put_record(Record),
   NoteType = wf:q(n_type),
   Redirect = ["/nnote/add_edit", "?", 
       wf:to_qs([ {id, "new"}, {note_type, NoteType} ]) ],
   wf:redirect(Redirect);
  
save(ID) ->
   Params = wf:mq([user_id, n_type, date, event, source,
                  topic, question, tags, note]),
   Record = nnote_api:put_all_values(Params),
   Record1 = nnote_api:id(Record, ID),   
   nnote_api:put_record(Record1),
   NoteType = wf:q(n_type),
   Redirect = ["/nnote", "?", 
       wf:to_qs([{note_type, NoteType} ]) ],
   wf:redirect(Redirect).
  

event_label("conference") -> "conference";
event_label("lecture")    -> "event";
event_label(_)          -> "".

show_event("conference") -> true;
show_event("lecture")    -> true;
show_event(_)  -> false.

source_label("conference") -> "speaker";
source_label("idea")        -> "";
source_label("lab")        -> "";
source_label("lecture")    -> "speaker";
source_label("web")        -> "URL";
source_label(_)            -> "source".

show_source("idea")        -> false;
show_source("lab")         -> false;
show_source(_)             -> true.

question_label("conference") -> "";
question_label("idea")       -> "";
question_label("web")        -> "";
question_label(_)            -> "question".       

show_question("interview") -> true;
show_question("lab")       -> true;
show_question("lecture")   -> true;
show_question("research")  -> true;
show_question(_)           -> false.



%% ***************************************************
%% Top menu events 
%% ***************************************************

event({main, tips}) ->
   wf:update(content, tips());

event({main, logout}) ->
   wf:clear_user(),
   wf:redirect("/");

event({main, Link}) ->
   wf:redirect(Link);

event(content) ->
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

event({select, NoteType}) ->
   Redirect = [?PATH, "?", wf:to_qs([ {id, "new"},{note_type, NoteType} ]) ],
   wf:redirect(Redirect);

%% ***************************************************
%% Content events 
%% ***************************************************

event(save_note) ->
   wf:wire(#confirm{text="Save?", postback=confirm_ok});

event(confirm_ok) ->
   ID = wf:q(id),
   save(ID);

event(cancel) ->
   wf:redirect("/nnote").

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
 
