-module(frequency_tests).
-define(NODEBUG, true).
-define(ASSERT, true).
-include_lib("eunit/include/eunit.hrl").
-export([allocate_test_client/0, deallocate_test_client/0]).

% see http://learnyousomeerlang.com/eunit
-define(setup(TestSet), {setup, fun setup_server/0, fun teardown_server/1, TestSet}).
-define(foreach(TestSet), {foreach, fun setup_server/0, fun teardown_server/1, TestSet}).

setup_server() -> 
    frequency:init().
    
teardown_server(Server) ->
    Server ! {request, self(), stop},
    timer:sleep(20).

allocate_series_test_() ->
    ?setup(fun(Server) ->
               [
                   ?_assertEqual({reply, {ok, 10}}, allocate_with_new_pid(Server)),
                   ?_assertEqual({reply, {ok, 11}}, allocate_with_new_pid(Server)),
                   ?_assertEqual({reply, {ok, 12}}, allocate_with_new_pid(Server)),
                   ?_assertEqual({reply, {ok, 13}}, allocate_with_new_pid(Server)),
                   ?_assertEqual({reply, {ok, 14}}, allocate_with_new_pid(Server)),
                   ?_assertEqual({reply, {ok, 15}}, allocate_with_new_pid(Server)),
                   ?_assertEqual({reply, {error, no_frequency}}, allocate_with_new_pid(Server))
               ]
	   end).

allocate_one_per_pid_test_() ->
    ?setup(fun(Server) -> 
               [
                   ?_assertEqual({reply, {ok, 10}}, allocate(Server)),
                   ?_assertEqual({reply, {error, pid_allocated_freq}}, allocate(Server))
               ]
           end).

deallocation_test_() ->
    ?foreach([
                 fun(Server) -> ?_assertEqual({reply, {error, not_allocated_to_pid}},
                                deallocate_with_different_pid(Server)) end,
                 fun(Server) -> ?_assertEqual({reply, {ok, deallocated}},
                                deallocate_with_same_pid(Server)) end,
                 fun(Server) -> ?_assertEqual({reply, {error, not_allocated_to_pid}},
                                deallocate(Server, 10)) end
             ]).

allocate_with_new_pid(Server) ->
    Pid = spawn(frequency_tests, allocate_test_client, []),
    Pid ! {allocate, Server, self()},
    receive
        Reply -> Reply
    after 500 ->
        {fail, no_resp_from_allocate_test_client}
    end.

allocate(Server) ->
    Server ! {request, self(), allocate},
    ?debugFmt("allocate PID: ~w", [self()]),
    receive
        {reply, F} -> {reply, F}
    after 500 ->
        {fail, no_resp_from_server}
    end.

allocate_test_client() ->
    receive
        {allocate, Server, TestProc} ->
	    Freq = allocate(Server),
	    TestProc ! Freq
    after 500 -> 
        {fail, allocate_test_client_ignored}
    end.

deallocate_with_different_pid(Server) ->
    {reply, {ok, Freq}} = allocate(Server),
    Pid = spawn(frequency_tests, deallocate_test_client, []),
    Pid ! {deallocate, Server, Freq, self()},
    receive
        Reply -> Reply
    after 500 ->
        {fail, no_resp_from_deallocate_test_client}
    end.

deallocate_with_same_pid(Server) ->
    ?debugMsg("deallocate_with_same_pid"),
    {reply, {ok, Freq}} = allocate(Server),
    ?debugFmt("allocated freq ~w to ~w", [Freq, self()]),
    deallocate(Server, Freq).

deallocate(Server, Freq) ->
    Server ! {request, self(), {deallocate, Freq}},
    ?debugFmt("deallocate freq ~w from PID: ~w", [Freq, self()]),
    receive
        Reply -> Reply
    after 500 ->
        {fail, no_resp_from_server}
    end.

deallocate_test_client() ->
    receive
        {deallocate, Server, Freq, TestProc} ->
	    Reply = deallocate(Server, Freq),
	    TestProc ! Reply
    after 500 -> 
        {fail, deallocate_test_client_ignored}
    end.
