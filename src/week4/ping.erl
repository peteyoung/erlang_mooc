-module(ping).
-export([srv/0]).

srv() ->
    receive
        {Pid, ping} ->
            Pid ! pong,
	    srv();
	stop -> ok
    end.

%% Server = spawn(ping, srv, []).
%% Server ! {self(), ping}.
%% flush().
%% Server ! stop.

%% register(ponger, spawn(ping, srv, [])).
%% ponger ! {self(), ping}.
%% flush().
%% ponger ! stop.
