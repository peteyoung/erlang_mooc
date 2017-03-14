-module(sort).
-export([msort/1, count/1, split/1, merge/2]).

msort([]) ->
    [];
msort([X]) ->
    [X];
msort([X|Xs]) ->
    {L, R} = split([X|Xs]),
    merge(msort(L), msort(R)).

%% sort:msort([5,2,8,4,3,9,6,1,7,0]) == [0,1,2,3,4,5,6,7,8,9].


merge([], []) ->
    [];
merge([HL|Ls], []) ->
    [HL|merge(Ls, [])];
merge([], [HR|Rs]) ->
    [HR|merge([], Rs)];
merge([HL|Ls], [HR|Rs]) ->
    case HL < HR of
        true -> [HL|merge(Ls, [HR|Rs])];
	false -> [HR|merge([HL|Ls], Rs)]
    end.

%% sort:merge([1], [2]) == [1,2].
%% sort:merge([1,2], []) == [1,2].
%% sort:merge([], [1,2]) == [1,2].
%% sort:merge([3], [1,2]) == [1,2,3].
%% sort:merge([3,4], [1,2]) == [1,2,3,4].


count(Xs) ->
    count(Xs, 0).
count([], A) ->
    A;
count([_|Xs], A) ->
    count(Xs, A+1).

%% sort:count([1, 2, 3, 4, 5, 6]) == 6.
%% sort:count([]) == 0.
%% sort:count("hello") == 5.
%% sort:count("abcdefghijklmnopqrstuvwxyz") == 26.


split(Xs) ->
    M = (count(Xs) div 2) - 1,
    split(Xs, [], [], M, true).
split([], L, R, _, _) ->
    {L, R};
split([X|Xs], L, R, M, true) ->
    split(Xs, [X|L], R, M, count(L)<M);
split([X|Xs], L, R, M, false) ->
    split(Xs, L, [X|R], M, false).

    %% io:fwrite("in true   X:~w, Xs:~w, L:~w, R:~w, M:~w, cL:~w, b:~w~n", [X, Xs, L, R, M, count(L), count(L)<M]),
    %% io:fwrite("in false  X:~w, Xs:~w, L:~w, R:~w, M:~w, cL:~w, b:~w~n", [X, Xs, L, R, M, count(L), count(L)<M]),

%% sort:split([1, 2 ,3 ,4 ,5 ,6]).
%% sort:split([1, 2 ,3]).
