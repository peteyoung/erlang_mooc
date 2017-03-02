-module(xOr).
-export([xOr1/2, xOr2/2, xOr3/2]).

xOr1(X,Y) -> not(X == Y).

xOr2(X,Y) -> X =/= Y.

xOr3(X,Y) -> (X or Y) and not(X and Y).
