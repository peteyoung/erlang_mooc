-module(rps).
-export([beat/1, lose/1, result/2, tournament/2]).

beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock.

lose(rock) -> scissors;
lose(paper) -> rock;
lose(scissors) -> paper.

result(L, R) -> 
    case L == lose(R) of
        true  -> -1;
        false -> case L == beat(R) of
            true  -> 1;
            false -> 0
        end
    end.

results([], []) -> [];
results([L|Ls], [R|Rs]) -> [result(L, R)|results(Ls, Rs)].

tournament(L, R) ->
    lists:foldr(fun (X, Acc) -> Acc + X end,
                0,
		results(L, R)).

