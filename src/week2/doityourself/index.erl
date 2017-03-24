-module(index).
-export([get_file_contents/1,show_file_contents/1,
        index/1, extract_word/1, extract_words/1,
	parse/1]).

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
%    Lines = get_file_contents(Name),
%    WordsByLine = parse(Lines),
%    WordsByLine.
[].

parse(Line) -> parse(Line, []).

parse([], Words) -> 
    lists:reverse(Words);
parse([Line|Lines], Words) ->
    parse(Lines, [extract_words(Line)|Words]).
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

