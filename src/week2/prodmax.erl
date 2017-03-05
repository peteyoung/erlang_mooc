-module(prodmax).
-export([prodDR/1, prodTR/1, maxDR/1, maxTR/1]).

prodDR([X|Xs]) ->
    X * prodDR(Xs);
prodDR([]) ->
    1.


prodTR(Xs) ->
    prodTR(Xs, 1).

prodTR([X|Xs], P) ->
    prodTR(Xs, P*X);
prodTR([], P) ->
    P.


maxDR([X]) ->
    X;
maxDR([X|Xs]) ->
    max(X, maxDR(Xs)).


maxTR(Xs) ->
    maxTR(Xs, 0).

maxTR([X], M) ->
    max(X, M);
maxTR([X|Xs], M) ->
    maxTR(Xs, max(X,M)).
