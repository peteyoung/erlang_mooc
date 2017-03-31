-module(rps_tests).
-include_lib("eunit/include/eunit.hrl").

result_test() ->
    ?assertEqual(1, rps:result(rock, scissors)),
    ?assertEqual(1, rps:result(paper, rock)),
    ?assertEqual(1, rps:result(scissors, paper)),
    ?assertEqual(0, rps:result(rock, rock)),
    ?assertEqual(0, rps:result(paper, paper)),
    ?assertEqual(0, rps:result(scissors, scissors)),
    ?assertEqual(-1, rps:result(rock, paper)),
    ?assertEqual(-1, rps:result(paper, scissors)),
    ?assertEqual(-1, rps:result(scissors, rock)).

tournament_test() ->
    ?assertEqual(-1, 
        rps:tournament([rock,rock,paper,paper],
                       [rock,paper,scissors,rock])).
