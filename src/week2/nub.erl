-module(nub).
-export([nub/1, nub2/1, nub3/1]).

nub([]) ->
    [];
nub([H|T]) ->
    case lists:member(H, T) of
        true -> nub(T);
        false -> [H|nub(T)]
    end.


nub2(L) ->
    nub2(L, []).

nub2([], N) ->
    lists:reverse(N);

nub2([H|T], N) ->
    case lists:member(H, N) of
        true -> nub2(T, N);
        false -> nub2(T, [H|N])
    end.


nub3(L) ->
    nub3(L, []).

nub3([], N) ->
    lists:reverse(N);

nub3([H|T], N) ->
    case lists:member(H, T) of
        true -> nub3(T, N);
        false -> nub3(T, [H|N])
    end.

