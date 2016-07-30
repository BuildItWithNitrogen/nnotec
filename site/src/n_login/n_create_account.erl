%% -*- mode: nitrogen -*-
%%% -----------------------------------------------
%%% Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc create account 
%%% @end
%%% -----------------------------------------------

-module (n_create_account).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

%% ***************************************************
%% Macros 
%% ***************************************************

-define(PATH, "/n_signin").
-define(TEMPLATE, "./site/templates/n_apps.html").
-define(MMSELECTED, "").
-define(TITLE, "Nnote Sign In").
-define(TOP, "Build it with Nitrogen").
-define(UVARS, []).
-define(USERNAME, "Marsha").
-define(ACCESS, public).
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
          update_page(?ACCESS)
        ]
   }].

%% ***************************************************
%% Page state functions
%% ***************************************************

get_page_state() ->
  List = wf:mq(?UVARS),
  list_to_tuple(List).

update_page(public)  -> show_page();
update_page(private) -> login().

login() -> wf:redirect("/login").

show_page() ->
   PageState = get_page_state(),
   wf:replace(content, show_content(PageState)),
  [ #p {} ].


%% ***************************************************
%% Sidebar executives 
%% ***************************************************

%% ***************************************************
%% Sidebar functions 
%% ***************************************************

%% ***************************************************
%% Sidebar menus 
%% ***************************************************

%% ***************************************************
%% Content executives 
%% ***************************************************

show_content({}) ->
   [ #panel {id = content, body = 
        [create_account_form()] 
   }].

%% ***************************************************
%% Content clips
%% ***************************************************

create_account_form() ->  
   wf:defer(save, username, #validate{validators=[
      #is_required{text="Username Required"}]}),
   wf:defer(save, password, #validate{validators=[
      #is_required{text="Password Required"}]}),
   wf:defer(save, password2, #validate{validators=[
      #confirm_same{text="Passwords do not match",
                    confirm_id=password}]}),
   [#h1 {text="Create Account"},
    #br {},
    #label{text="Email or Username"},
    #textbox{id=username},
    #label{text="Password"},
    #password{id=password},
    #label{text="Confirm Password"},
    #password{id=password2},
    #p{},
    #button{id=login, text="Save Account", postback=save}
  ].




welcome() ->
   [#h2 {class=content, text=["Welcome to ", ?USERNAME , "\'s ", "Nitrogen Applications!"]},
    #p {body = "Our motto: <em>\"Build it Fast with Nitrogen\"</em>"}
   ]. 

%% ***************************************************
%% Main menu events 
%% ***************************************************


event({main, tips}) ->
   wf:update(content, tips());

event({main, Link}) ->
   wf:redirect(Link);


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
   wf:redirect(Link);

%% ***************************************************
%% Content events 
%% ***************************************************

event(save) ->
  [Username, Password] = wf:mq([username, password]),  
  ok = naccounts_api:create_account(Username, Password),  
  wf:user(Username),  
  wf:redirect("/").


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



 
