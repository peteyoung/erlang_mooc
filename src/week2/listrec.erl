-module(listrec).
-export([double/1, evens/1, evens2/1]).

double([N]) ->
    [N*2];
double([N|Ns]) ->
    [N*2|double(Ns)].

%% listrec:double([1,2,3,4,5,6]).


evens([N], Evens) ->
    case N of N when N rem 2 == 0 ->
        [N | Evens];
    _ ->
        Evens
    end;
evens([N|Ns], Evens) ->
    case N of N when N rem 2 == 0 ->
        evens(Ns, [N | Evens]);
    _ ->
        evens(Ns, Evens)
    end.

evens(Ns) ->
    evens(Ns, []).

%% listrec:evens([1,2,3,4,5,6,7,8,9,0]).


evens2([N], Evens) when N rem 2 == 0 ->
    [N | Evens];
evens2([_N], Evens) ->
    Evens;
evens2([N|Ns], Evens) when N rem 2 == 0 ->
    evens2(Ns, [N | Evens]);
evens2([_N|Ns], Evens) ->
    evens2(Ns, Evens).

evens2(Ns) ->
    evens2(Ns, []).

%% listrec:evens2([1,2,3,4,5,6,7,8,9,0]).
