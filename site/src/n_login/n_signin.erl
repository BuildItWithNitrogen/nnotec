%% -*- mode: nitrogen -*-
%%% -----------------------------------------------
%%% Lloyd R. Prentice
%%% @copyright 2016 Lloyd R. Prentice 
%%% @version 0.01
%%% @doc login 
%%% @end
%%% -----------------------------------------------

-module (n_signin).

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
        [login_form()] 
   }].

%% ***************************************************
%% Content clips
%% ***************************************************

login_form() ->
    case ?USER of % wf:user() of
        undefined ->
           wf:defer(login, email, #validate{validators=[
             #is_required{text="Email required"},
             #is_email{text="Not a valid email address."}]}),
           wf:defer(login, password, #validate{validators=[
             #is_required{text="Password required"}]}),
           [ #panel {class="content", body =
               [ #h1 {body="Sign In"},
                 #br {},
                 #label{text="Email"},
                 #textbox{id=username, class="standard"},
                 #p {},
                 #label{text="Password"},
                 #password{id=password, class="standard"},
                 #p{},
                 #button {id=signin, text="Sign In", postback=signin},
                 #p{}
               ]
            }];
         _   ->  
            [ # panel {class="content", body =
                [#p {text="You are logged in."}
                ]
            }]  
    end.



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

event(signin) ->
   [Username, Password] = wf:mq([username, password]),
   Result = naccounts_api:attempt_login(Username, Password),
   case Result of
      true  -> wf:user(Username),
               wf:redirect("/");
      false -> wf:wire(#alert{
                      text="Can't find username or password."
                      })
   end.
   

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



 
