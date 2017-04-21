-module(frequency_tests).
-include_lib("eunit/include/eunit.hrl").
-export([allocate_test_client/0]).

% see http://learnyousomeerlang.com/eunit
-define(setup(TestSet), {setup, fun setup_server/0, fun teardown_server/1, TestSet}).

setup_server() -> 
    frequency:init().
    
teardown_server(Server) ->
    Server ! {request, self(), stop},
    timer:sleep(20).

allocate_series_test_() ->
    ?setup(fun(Server) ->
               [
                   ?_assertEqual(10, allocate_with_new_pid(Server)),
                   ?_assertEqual(11, allocate_with_new_pid(Server)),
                   ?_assertEqual(12, allocate_with_new_pid(Server)),
                   ?_assertEqual(13, allocate_with_new_pid(Server)),
                   ?_assertEqual(14, allocate_with_new_pid(Server)),
                   ?_assertEqual(15, allocate_with_new_pid(Server)),
                   ?_assertEqual(no_frequency, allocate_with_new_pid(Server))
               ]
	   end).

allocate_one_per_pid_test_() ->
    ?setup(fun(Server) -> 
               [
                   ?_assertEqual(10, allocate(Server)),
                   ?_assertEqual(pid_allocated_freq, allocate(Server))
               ]
           end).

allocate_with_new_pid(Server) ->
    Pid = spawn(frequency_tests, allocate_test_client, []),
    Pid ! {allocate, Server, self()},
    receive
        Resp -> Resp
    after 500 ->
        {fail, no_resp_from_allocate_test_client}
    end.

allocate(Server) ->
    Server ! {request, self(), allocate},
    {_, Freq} = receive
        {reply, F} -> F
    after 500 ->
        {fail, no_resp_from_server}
    end,
    Freq.

allocate_test_client() ->
    receive
        {allocate, Server, TestProc} ->
	    Freq = allocate(Server),
	    TestProc ! Freq
    after 500 -> 
        {fail, allocate_test_client_ignored}
    end.
