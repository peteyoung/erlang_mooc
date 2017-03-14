-module(listfuncs).
-export([join/2, concat/1, member/2]).

join(Xs, Ys) ->
    join2(invert(Xs), Ys).

%% listfuncs:join("hel","lo") == "hello".

join2([], Ys) ->
    Ys;
join2([X|Xs], Ys) ->
    join2(Xs, [X|Ys]).

invert(Xs) ->
    invert(Xs, []).
invert([], Ys) ->
    Ys;
invert([X|Xs], Ys) ->
    invert(Xs, [X|Ys]).


concat([]) ->
    [];
concat([L|Ls]) ->
    join(L, concat(Ls)).

%% listfuncs:concat(["goo","d","","by","e"]) == "goodbye".
%% listfuncs:concat([[1,2,3], [4,5,6], [7,8,9], [0]]). == [1,2,3,4,5,6,7,8,9,0].


member(_X, []) ->
    false;
member(X, [Y|Ys]) ->
    case X == Y of
        true -> true;
	false -> member(X, Ys)
    end.

%% listfuncs:member(2,[2,0,0,1]).
%% listfuncs:member(20,[2,0,0,1]).
