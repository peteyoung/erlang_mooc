%% https://hastebin.com/birajucino.pl
-module(index).
-export([get_file_contents/1,show_file_contents/1,
        index/1, extract_word/1, extract_words/1,
	parse/1, get_next_num/2, build_tuples/1,
	format_index_line/2]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
  

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
lists:reverse(Rev).
%% index:get_file_contents("gettysburg-address.txt").


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


index(Name) ->
    Lines = get_file_contents(Name),
    WordsByLine = parse(Lines),
    LinesByWord = map_lines_to_words(WordsByLine),
    format_index(LinesByWord).
%% index:index("gettysburg-address.txt").
%% D = index:index("gettysburg-address.txt").


%% iterate dict of line nums keyed by word
format_index(LinesByWord) ->
    format_index(dict:fetch_keys(LinesByWord), LinesByWord, []).

format_index([], _LinesByWord, Index) ->
    Index;
format_index([Word|More], LinesByWord, Index) ->
    LNums = dict:fetch(Word, LinesByWord),
    format_index(More, LinesByWord, [format_index_line(Word, LNums)|Index]).


%% format an entry as { "word", [{x,x}, {y,z}] }
%% Word - word to be indexed
%% LNums - list of line numbers word was found on
format_index_line(Word, LNums) ->
    { Word, build_tuples(LNums) }.
%% index:format_index_line("baz", [1,2,3,4]).
%% index:format_index_line("bar", [1,2,3,4,6,7,8,10]).
%% index:format_index_line("foo", [3,4,5,7,11,12,13]).


%% iterate list of line nums and get tuples for word
build_tuples([]) -> [];
build_tuples([Num]) -> [{Num,Num}];
build_tuples([Start|More]) ->
    {End, Rem} = get_next_num(Start, More),
    [{Start,End} | build_tuples(Rem)].
%% index:build_tuples([1,2,3,4]) == [{1,4}].
%% index:build_tuples([1,2,3,4,6,7,8,10]) == [{1,4},{6,8},{10,10}].
%% index:build_tuples([3,4,5,7,11,12,13]) == [{3,5},{7,7},{11,13}].
%% index:build_tuples([1]) == [{1,1}].


%% iterate list of numbers and return start and end of any series
%% as a tuple. e.g. 1,2,3,4 -> {1,4}.
%% any non series number merely gets duplicated 1,5,6,7 -> {1,1}.
%% any remaining numbers also get returned.
get_next_num(Num, []) ->
    {Num, []};
get_next_num(Num, [H|T]) when Num == H; Num+1 == H ->
    get_next_num(H, T);
get_next_num(Num, [H|T]) ->
    {Num, [H|T]}.
%% index:get_next_num(1, [2,3,4,6]) == {4,[6]}.
%% index:get_next_num(1, [3,4,5,6]) == {1,[3,4,5,6]}.
%% index:get_next_num(1, [1,2,3,4,6]) == {4,[6]}.
%% index:get_next_num(1, [1,2,3,4,5,6]) == {6,[]}.
%% index:get_next_num(1, [1]) == {1,[]}.
%% index:get_next_num(1, [2]) == {2,[]}.
%% index:get_next_num(1, [3]) == {1,[3]}.



%% iterate dict of line nums -> words and pivot to words -> line nums
map_lines_to_words(WordsByLine) ->
    map_lines_to_words(WordsByLine, dict:new()).

map_lines_to_words([], Dict) ->
    Dict;
map_lines_to_words([H|T], Dict) ->
    {LineNum, Words} = H,
    map_lines_to_words(T, add_line_to_words(LineNum, Words, Dict)).


add_line_to_words(_, [], Dict) ->
    Dict;
add_line_to_words(LineNum, [Word|More], Dict) ->
    add_line_to_words(LineNum, More, dict:append(Word, LineNum, Dict)).


parse(Lines) -> parse(Lines, [], 1).

parse([], Words, _LineNum) -> 
    lists:reverse(Words);
parse([Line|Lines], Words, LineNum) ->
    parse(Lines, [{LineNum, extract_words(Line)}|Words], LineNum+1).
%% index:parse(index:get_file_contents("gettysburg-address.txt")).


extract_words(Line) -> extract_words(Line, []).

extract_words([], Words) ->
    lists:reverse(Words);
extract_words(Line, Words) ->
    {Remainder, Word} = extract_word(Line),
    extract_words(Remainder, [Word|Words]).


extract_word(Line) -> extract_word(Line, []).

extract_word([], Word) -> %% EOL
    {[], lists:reverse(Word)};
extract_word([H|T], Word) when H == 32 -> %% Space
    {T, lists:reverse(Word)};
extract_word([H|T], Word) when H >= 65, H =< 90 -> %% A-Z
    extract_word(T, [H+32|Word]); %% conv. to lower case
extract_word([H|T], Word) when H >= 97, H =< 122 -> %% a-z
    extract_word(T, [H|Word]);
extract_word([_|T], Word) -> %% ignore other chars
    extract_word(T, Word).

