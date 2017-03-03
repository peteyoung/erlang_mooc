-module(fib).
-export([fib/1, tcFib/1, fibPm/1]).

fib(N) when N < 1 ->
    0;
fib(1) ->
    1;
fib(N) when N > 1 ->
    fib(N-1) + fib(N-2).


tcFib(0,P,_C) ->
    P;
tcFib(N,P,C) ->
    tcFib(N-1, C, P+C).

tcFib(N) ->
    tcFib(N,0,1).


fibP(0) ->
    {0, 1};
fibP(N) ->
    {P, C} = fibP(N-1),
    {C, P+C}.

fibPm(N) ->
    {P,_} = fibP(N),
    P.
