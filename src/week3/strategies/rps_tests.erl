-module(rps_tests).
-include_lib("eunit/include/eunit.hrl").

count_test() ->
    ?assertEqual(0, rps:count([])),
    ?assertEqual(1, rps:count([one])),
    ?assertEqual(10, rps:count([0,9,8,7,6,5,4,3,2,1])).

no_repeat_test() ->
    ?assertEqual(rock, rps:no_repeat([])),
    ?assertEqual(paper, rps:no_repeat([rock, paper, scissors])),
    ?assertEqual(rock, rps:no_repeat([scissors, rock, paper])),
    ?assertEqual(scissors, rps:no_repeat([paper, scissors, rock])).

cycle_test() ->
    ?assertEqual(rock, rps:cycle([])),
    ?assertEqual(paper, rps:cycle([rock])),
    ?assertEqual(scissors, rps:cycle([rock,rock])),
    ?assertEqual(rock, rps:cycle([rock,rock,rock])),
    ?assertEqual(paper, rps:cycle([rock,rock,rock,rock])),
    ?assertEqual(scissors, rps:cycle([rock,rock,rock,rock,rock])),
    ?assertEqual(rock, rps:cycle([rock,rock,rock,rock,rock,rock])).

frequency_test() ->
    ?assertEqual(0, rps:frequency(rock, [])),
    ?assertEqual(0, rps:frequency(paper, [])),
    ?assertEqual(0, rps:frequency(scissors, [])),
    ?assertEqual(3,
        rps:frequency(rock, [rock,paper,scissors,rock,paper,rock])),
    ?assertEqual(2,
        rps:frequency(paper, [rock,paper,scissors,rock,paper,rock])),
    ?assertEqual(1,
        rps:frequency(scissors, [rock,paper,scissors,rock,paper,rock])).

frequencies_test() ->
    ?assertEqual([{rock, 0}, {paper, 0}, {scissors, 0}],
                  rps:frequencies([])),
    ?assertEqual([{rock, 3}, {paper, 2}, {scissors, 1}],
                  rps:frequencies([rock,paper,scissors,rock,paper,rock])).

least_freq_test() ->
    ?assertEqual(rock, rps:least_freq([])),
    ?assertEqual(scissors,
                  rps:least_freq([rock,paper,scissors,rock,paper,rock])).

most_freq_test() ->
    ?assertEqual(rock, rps:most_freq([])),
    ?assertEqual(rock,
                  rps:most_freq([rock,paper,scissors,rock,paper,rock])).

rand_strategy_test() ->
    Strategies = [fun rps:echo/1, fun rps:rock/1, fun rps:no_repeat/1,
                  fun rps:cycle/1, fun rps:rand/1, fun rps:least_freq/1,
                  fun rps:most_freq/1],

    S = rps:rand_strategy(Strategies),

    true = lists:member(S, Strategies).
