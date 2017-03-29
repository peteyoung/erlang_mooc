-module(hof_practice).
-export([doubleAll/1, doubleAll2/1,
         evens/1, evens2/1,
	 product/1, product2/1,
         zip/2, zip2/2, zip_with/3, zip_with2/3,
         test/0]).

doubleAll([]) -> [];
doubleAll([X|Xs]) ->
    [ 2*X | doubleAll(Xs) ].

doubleAll2(L) -> lists:map(fun (X) -> X+X end, L).


evens([]) -> [];
evens([X|Xs]) when X rem 2 == 0 ->
    [X | evens(Xs) ];
evens([_|Xs]) ->
    evens(Xs).

evens2(L) -> lists:filter(fun(X) -> X rem 2 == 0 end, L).


product([]) -> 1;
product([X|Xs]) -> X * product(Xs).

product2(L) -> lists:foldr(fun(X, P) -> X*P end, 1, L).


zip([], [_|_]) -> [];
zip([_|_], []) -> [];
zip([H1|T1], [H2|T2]) -> [{H1,H2} | zip(T1,T2)].

zip2(L1, L2) -> zip_with(fun (X,Y) -> {X,Y} end, L1, L2).
 

zip_with(_, [], [_|_]) -> [];
zip_with(_, [_|_], []) -> [];
zip_with(F, [H1|T1], [H2|T2]) -> [F(H1,H2) | zip_with(F,T1,T2)].

zip_with2(F, L1, L2) ->
    lists:map(fun (T) -> {X,Y} = T, F(X,Y) end, zip(L1,L2)).


test() ->
    [2,4,6,8] = doubleAll([1,2,3,4]),
    [2,4,6,8] = doubleAll2([1,2,3,4]),

    [2,4,6] = evens([1,2,3,4,5,6]),
    [2,4,6] = evens2([1,2,3,4,5,6]),

    120 = product([2,3,4,5]),
    120 = product2([2,3,4,5]),

    [{1,2},{3,4}] = zip([1,3,5,7], [2,4]),
    [{1,2},{3,4}] = zip2([1,3,5,7], [2,4]),

    [3,7] = zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]),
    [3,7] = zip_with2(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]),
    ok.
