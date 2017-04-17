-module(frequency_tests).
-include_lib("eunit/include/eunit.hrl").
-export([call_allocate/1]).

allocate_series_test_() ->
    {setup,
        fun setup_server/0,
        fun teardown_server/1,
%        fun(Pid) ->
%                ?_assertEqual(10, call_allocate(Pid)),
%                ?_assertEqual(11, call_allocate(Pid)),
%                ?_assertEqual(12, call_allocate(Pid)),
%                ?_assertEqual(13, call_allocate(Pid)),
%                ?_assertEqual(14, call_allocate(Pid)),
%                ?_assertEqual(15, call_allocate(Pid)),
%                ?_assertEqual(no_frequency, call_allocate(Pid))
%        end
        fun assert_allocate/1
    }.

setup_server() -> 
    %eunit:debug({starting, self()}),
    spawn(frequency, init, []).
    
teardown_server(Pid) ->
    %eunit:debug({teardown, self()}),
    Pid ! {request, self(), stop}.

assert_allocate(Pid) -> 
    [?_assertEqual(11, call_allocate(Pid))].

call_allocate(Pid) ->
    Pid ! {request, self(), allocate},
    {_, Freq} = receive
        {reply, F} -> F
    end,
    Freq.

%% Pid = spawn(frequency, init, []).
%% Pid ! {request, self(), allocate}.
%% {_, Freq} = receive
%% {reply, F} -> F
%% end.
%% Freq.
%% frequency_tests:call_allocate(Pid).
%% Pid ! {request, self(), stop}.

