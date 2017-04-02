-module(hof).
-export([add/1,times/1,compose/2,id/1,iterate/1,
         compose_n/1, twice/1]).

add(X) ->
    fun(Y) -> X+Y end.

times(X) ->
    fun(Y) ->
	     X*Y end.

compose(F,G) ->
    fun(X) -> G(F(X)) end.

id(X) ->
    X.

compose_n(Fs) -> lists:foldr(fun compose/2, fun id/1, Fs).

twice(F) -> compose(F, F).

iterate(0) ->
    fun id/1;
iterate(N) ->
    fun (F) -> compose(F, iterate(N-1)) end.
