-module(palin).
-export([palin/1,nopunct/1,palindrome/1,server/1]).

% palindrome problem
%
% palindrome("Madam I\'m Adam.") = true

palindrome(Xs) ->
    palin(nocaps(nopunct(Xs))).

nopunct([]) ->
    [];
nopunct([X|Xs]) ->
    case lists:member(X,".,\ ;:\t\n\'\"") of
	true ->
	    nopunct(Xs);
	false ->
	    [ X | nopunct(Xs) ]
    end.

nocaps([]) ->
    [];
nocaps([X|Xs]) ->
    [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
    case $A =< X andalso X =< $Z of
	true ->
	    X+32;
	false ->
	    X
    end.

% literal palindrome

palin(Xs) ->
    Xs == reverse(Xs).

reverse(Xs) ->
    shunt(Xs,[]).

shunt([],Ys) ->
    Ys;
shunt([X|Xs],Ys) ->
    shunt(Xs,[X|Ys]).


% server

server(Pid) ->
    receive
        {check, P} ->
            case palindrome(P) of
                true  ->
                    Pid ! {result, P ++ " is a palindrome", [P]};
                false ->
                    Pid ! {result, P ++ " is not a palindrome", [P]}
            end,
            server(Pid);
        Msg ->
            io:format("stopping on ~s~n", [Msg])
    end.
%% Pid = spawn(palin, server, [self()]).
%% Pid ! {check, "boob"}.
%% Pid ! {check, "cat"}.
%% Pid ! {check, "A man, a plan, a canal, Panama."}.
%% flush().
%% Pid ! stop.


