-module(second).
-import(first, [square/1]).
-export([hypotenuse/2, area/2, perimeter/2]).

hypotenuse(A, B) ->
    math:sqrt(first:square(A) + first:square(B)).

area(A, B) ->
    (A*B)/2.

perimeter(A, B) ->
    A + B + hypotenuse(A, B).
