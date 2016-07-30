
-module(n_search).

-compile(export_all).

-include("n_records.hrl").

%% ---------------------------------------------
%% @doc Search by keyword
%% ---------------------------------------------


long_string(Record) ->
    Values = nnote_api:get_all_values(Record),
    List = [Value || Value <- Values, is_list(Value)],
    string:join(List, " ").

dump_punc(String) ->
    re:replace(String, "[,.?!#^*()-]", "", [global, {return, list}]).

unique_words(String) ->
    Tokens = string:tokens(String, " "),
    sets:from_list(Tokens). 

shared(Set1, Set2) ->
   sets:intersection(Set1, Set2).

filter(SearchString, Record) ->
    LongString = long_string(Record),
    CleanString = dump_punc(LongString),
    NoteSet = unique_words(CleanString),
    SearchSet = unique_words(SearchString),
    SharedWords = shared(NoteSet, SearchSet),
    sets:size(SharedWords) > 0.    

