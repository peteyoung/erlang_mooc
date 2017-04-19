-module(frequency_tests).
-include_lib("eunit/include/eunit.hrl").

allocate_series_test_() ->
    {setup,
        fun setup_server/0,
        fun teardown_server/1,
        fun assert_allocate/1
    }.

setup_server() -> 
    frequency:init().
    
teardown_server(Server) ->
    Server ! {request, self(), stop}.

assert_allocate(Server) -> 
    [
        ?_assertEqual(10, call_allocate(Server)),
        ?_assertEqual(11, call_allocate(Server)),
        ?_assertEqual(12, call_allocate(Server)),
        ?_assertEqual(13, call_allocate(Server)),
        ?_assertEqual(14, call_allocate(Server)),
        ?_assertEqual(15, call_allocate(Server)),
        ?_assertEqual(no_frequency, call_allocate(Server))
    ].

call_allocate(Server) ->
    Server ! {request, self(), allocate},
    {_, Freq} = receive
        {reply, F} -> F
    after 2000 ->
        {fail, test_timed_out}
    end,
    Freq.
