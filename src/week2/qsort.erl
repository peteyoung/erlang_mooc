-module(qsort).
-export([qsort/1, first/1, last/1, count/1, take/2, middle/1,
         qsMax/1, qsMin/1, remove/2, medOf3/1, pivot/1, 
	 partition/2, flatten/1]).

%% qsort:qsort([9,6,5,7,1,8,3,2,4]).
qsort([]) -> [];
qsort([X]) -> [X];
qsort([X,Y]) when X < Y -> [X,Y];
qsort([X,Y]) -> [Y,X];
qsort(X) ->
    Pivot = pivot(X),
    {left, L, right, R} = 
        partition(X, Pivot),
    flatten([qsort(L), qsort(R)]).

%% take first item in list
%% qsort:first([1,5,2,4,3]) == 1.
%% qsort:first([3,4,2,5,1]) == 3.
first([X|_Xs]) -> X.


%% take last item in list
%% qsort:last([1,5,2,4,3]) == 3.
%% qsort:last([3,4,2,5,1]) == 1.
last([X]) -> X;
last([_X|Xs]) -> last(Xs).


%% count the number of items in a list
%% qsort:count([0,9,8,7,6,5,4,3,2,1]) == 10.
%% qsort:count([]) == 0.
count(X) -> count(X, 0).

count([], N) -> N;
count([_X|Xs], N) -> count(Xs, N+1).


%% take the Nth item in a list
%% qsort:take([0,9,8,7,6,5,4,3,2,1], 1) == 0.
%% qsort:take([0,9,8,7,6,5,4,3,2,1], 5) == 6.
%% qsort:take([0,9,8,7,6,5,4,3,2,1], 10) == 1.
take([X|_Xs], N) when N =< 1 -> X;
take([_X|Xs], N) -> take(Xs, N-1).


%% take the middle item in a list
%% qsort:middle([0,9,8,7,6,5,4,3,2,1]) == 5.
%% qsort:middle([9,8,7,6,5,4,3,2,1]) == 5.
middle(X) -> 
    N = (count(X) div 2) + 1,
    take(X, N).


%% find the max valued item in list
%% qsort:qsMax([2,5,6,7,5,3,1]) == 7.
qsMax(X) -> qsMax(X, 0).

qsMax([], M) -> M;
qsMax([X|Xs], M) when X > M -> qsMax(Xs, X);
qsMax([_X|Xs], M) -> qsMax(Xs, M).


%% find the qsMin valued item in list
%% qsort:qsMin([99]) == 99.
%% qsort:qsMin([1,2]) == 1.
%% qsort:qsMin([2,1]) == 1.
%% qsort:qsMin([9,8,7,6,5,4,3,2,1]) == 1.
%% qsort:qsMin([2,1,3,4,5,6,7,8,9]) == 1.
qsMin([X]) -> X;
qsMin([X,Y]) when X < Y -> X;
qsMin([_X,Y]) -> Y;
qsMin([X,Y|Xs]) when X < Y -> qsMin([X|Xs]);
qsMin([_X,Y|Xs]) -> qsMin([Y|Xs]).


%% remove a value from a list
%% qsort:remove([1,2,3,4,5,6,7,8,9,0], 7) == [1,2,3,4,5,6,8,9,0].
%% qsort:remove([2,6,2,6,2,6,2,6], 6) == [2,2,2,2].
remove([], _V) -> [];
remove([X|Xs], V) when X == V -> remove(Xs, V);
remove([X|Xs], V) -> [X|remove(Xs, V)].


%% find median of 3
%% qsort:medOf3([1,2,3]) == 2.
%% qsort:medOf3([1,3,2]) == 2.
%% qsort:medOf3([2,1,3]) == 2.
%% qsort:medOf3([2,3,1]) == 2.
%% qsort:medOf3([3,1,2]) == 2.
%% qsort:medOf3([3,2,1]) == 2.
medOf3(X) -> 
    [P] = remove(remove(X, qsMax(X)), qsMin(X)),
    P.


%% find pivot using median of 3
%% qsort:pivot([9,1,5,7,4,8,3,2,6]) == 6.
%% qsort:pivot([9,1,5,4,7,8,3,2,6]) == 7.
%% qsort:pivot([9,6,5,7,1,8,3,2,4]) == 4.
%% qsort:pivot([0,9,6,5,7,1,8,3,2,4]) == 1.
pivot(X) -> medOf3([first(X), middle(X), last(X)]).


%% partition a list by pivot. Left is < pivot. Right is > pivot.
%% qsort:partition([9,6,5,7,1,8,3,2,4], 4) == {left,[4,2,3,1],right,[8,7,5,6,9]}.
partition(X, P) -> partition(X, [], [], P).

partition([], L, R, _P) -> {left, L, right, R};
partition([X|Xs], L, R, P) when X =< P ->
    partition(Xs, [X|L], R, P);
partition([X|Xs], L, R, P) ->
    partition(Xs, L, [X|R], P).


%$ flatten a list of lists into a list
%% BUG: only flattens two levels deep correctly
%% qsort:flatten([[1,2,3],4,[5,6,[7,8,9]]]) == [1,2,3,4,5,6,7,8,9].
%% qsort:flatten([[1,2,3],4,[5,6,[7,8,9]]]).
%% qsort:flatten([1,[2,3]]).
%% qsort:flatten([1,[2,[3,4]]]).
%% qsort:flatten([1,[2,[3,[4,5]]]]).
%% qsort:flatten([[[1,2], 3], 4]).

flatten([]) ->
%    io:fwrite("flatten 1 []~n~n"),
    [];
flatten([[]|T]) ->
%    io:fwrite("flatten 2 [[]|~w]~n", [T]),
%    io:fwrite("          flatten(~w)~n~n", [T]),
    flatten(T);
flatten([[H|T]|T2]) ->
%    io:fwrite("flatten 3 [[~w|~w]|~w]~n", [H,T,T2]),
%    io:fwrite("          [~w|flatten([~w|~w])]~n~n", [H,T,T2]),
    [H|flatten([T|T2])];
flatten([H|T]) ->
%    io:fwrite("flatten 4 [~w|~w]~n", [H,T]),
%    io:fwrite("          [~w|flatten(~w)]~n~n", [H,T]),
    [H|flatten(T)].


