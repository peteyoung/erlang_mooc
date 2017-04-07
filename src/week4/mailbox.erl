-module(mailbox).
-export([receiver/1, message_ord/1]).

receiver(M) ->
    timer:sleep(M),
    receive
       stop ->
           io:format("message: ~w~n", [stop]);
       Msg ->
           io:format("message: ~w~n", [Msg]),
           receiver(M)
    end.

% Pid = spawn(mailbox, receiver, [2000]).
% Pid = spawn(mailbox, receiver, [0]).
% Pid ! one.
% Pid ! two.
% Pid ! three.
% Pid ! four.
% Pid ! five.


message_ord([]) -> stop;
message_ord([MType|More]) ->
    timer:sleep(2000),
    receive
       {MType, Msg} ->
           io:format("~w: ~s~n", [MType, Msg]),
	   message_ord(More)
    end.

% Pid = spawn(mailbox, message_ord, [[first, second]]).
% Pid ! {first, "FirstString"}.
% Pid ! {second, "SecondString"}.
% f().

% Pid = spawn(mailbox, message_ord, [[first, second]]).
% Pid ! {second, "SecondString"}.
% Pid ! {first, "FirstString"}.
% f().

