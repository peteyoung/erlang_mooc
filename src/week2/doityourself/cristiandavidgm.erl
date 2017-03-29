-module(cristiandavidgm).
-export([index_file/1,test/0,
         index_line/3]).

-define(TIMEOUT, 200).

test() ->
    [] = index([],[],1),
    [{"testing",[1]}] = index(["testing 123"],[],1),
    [{testing,[1]}] = cristiandavidgm:index_line("testing 123",[],1),
    [{grajales,[2]},{david,[2]}] = index_words(["david","grajales"],[],2),
    ["Cristian","David","Grajales"] = get_words("Cristian David Grajales"),
    "david grajales coding erlang  wow " = normalize_string("David,Grajales coding Erlang? wow!"),
    true = valid_char($A),
    false = valid_char($"),
    "cristian david" = lowercase("Cristian David"),
    ok.

% Given a filename index every VALID word and returns a proplist containing 
% the word and the lines in wich it appears.
index_file(Name) ->
    Content = get_file_contents(Name),
    Index = index(Content),
    io:format("Index: ~p ",[Index]).

% Given a List of strings index every word in the list in a given format
index(X) ->
    index(X,[], 1).

index([], _Index, _LineNumber) ->
    [];
index([H|[]], Index, LineNumber) ->
    TmpIndex = index_line(H, Index, LineNumber),
    TmpIndex2 = lists:map(fun({Key, Val}) -> { atom_to_list(Key), Val } end, TmpIndex ),
    lists:sort(fun({KeyA, _}, {KeyB, _} ) ->  KeyA < KeyB end, TmpIndex2 );
index([H|T], Index, LineNumber) ->
    NewIndex = index_line(H, Index, LineNumber),
    index(T, NewIndex, LineNumber+1).

% Given a string index every word and add the result to a previous given Index
% wich is a proplist
index_line(RawLine, Index, LineNumber) ->
    Line = normalize_string(RawLine),
    Words = get_words(Line),
    NewIndex = index_words(Words, Index, LineNumber), 
    NewIndex.

% Given a list of words, a number que represents a line numer and a previous list 
% of indexed words, index the words and return the new index list.
index_words([], Index, _LineNumber) ->
    Index;
index_words([Word|RestOfWords], Index, LineNumber) ->
    case proplists:is_defined( list_to_atom(Word), Index) of
        true -> 
            OldValue = proplists:get_value(list_to_atom(Word), Index),
            NewValue = [ LineNumber | OldValue ],
            TmpIndex = proplists:delete(list_to_atom(Word), Index),
            NewIndex = [ { list_to_atom(Word), NewValue } | TmpIndex ];
        false ->  
            NewIndex = [ { list_to_atom(Word), [LineNumber] } | Index ]
    end,
    index_words(RestOfWords, NewIndex, LineNumber).

% Given a String return all the words that it contanis removing duplicates and
% words that have more than 3 characters.
get_words(Line) ->
    AllWords = string:tokens(Line, " "),
    lists:filter(fun(X) -> length(X) > 3 end, AllWords).

% Given a string, removes non alphanumeric characters, punctiation caracters etc
% leaving only alphanumeric characters
normalize_string(Line) ->
    Fun = fun(X) -> 
        case valid_char(X) of
            true -> X;
            false-> $\s
        end
    end,
    lowercase( lists:map(Fun, Line) ).

% Given a character determine if it is a valid character to be used in a word
valid_char(X) ->
    not lists:member(X,",.:;_-/\\[]{}&%$!\"ºª{}´¨+*`^?¿¡'=()ç·").

% Given a string, convert all its characters to lowercase
lowercase(String) ->
   lowercase(String, []).
lowercase([], Output) ->
    lists:reverse(Output);
lowercase([Char | Rest], Output) when Char >= $A, Char =< $Z ->
    lowercase(Rest, [Char - ($A - $a) | Output]);
lowercase([Char | Rest], Output) ->
    lowercase(Rest, [Char | Output]).

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
    lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.
