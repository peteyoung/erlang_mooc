-module(howManyEqual).
-export([trequal/3]).

trequal(A,A,A) -> 3;
trequal(A,A,_) -> 2;
trequal(A,_,A) -> 2;
trequal(_,A,A) -> 2;
trequal(_,_,_) -> 0.
