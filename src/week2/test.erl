-module(test).
-export([foo/2, bar/2, baz/1, zab/2]).

foo(_,[])              -> [];
foo(Y,[X|_]) when X==Y -> [X];
foo(Y,[X|Xs])          -> [X | foo(Y,Xs) ].
%% test:foo(0,[4,0,1]) == [4,0].


bar (N, [N]) ->
    [];

bar (_N, [Y]) ->
     [Y];

bar (N, [Y|Ys]) when N =/= Y ->
    [Y|bar (N, Ys)];

bar (N, [_Y|Ys]) ->
    bar(N, Ys).

%% test:bar(2, [1,2,3,4,3,2,1,2,2,6,5,4,2]).




baz([])     -> [];
baz([X|Xs]) -> [X | baz(zab(X,Xs))].

zab(_N,[])     -> [];
zab(N,[N|Xs]) -> zab(N,Xs);
zab(N,[X|Xs]) -> [X | zab(N,Xs)].

%% test:baz([1,2,3,4,3,2,1,2,2,6,5,4,2]).
