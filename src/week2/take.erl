-module(take).
-export([take/2]).

take(_N, [], L) ->
    L;
take(0, _Xs, L) ->
    L;
take(N, [X|Xs], L) ->
    take(N-1, Xs, [X|L]).

take(N, Xs) ->
    rev(take(N, Xs, [])).

rev([], R) ->
    R;
rev([X|Xs], R) ->
    rev(Xs, [X|R]).

rev(Xs) ->
    rev(Xs, []).
