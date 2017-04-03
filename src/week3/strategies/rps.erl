-module(rps).
-export([play/1,echo/1,play_two/3,rock/1,no_repeat/1,const/1,
         enum/1,cycle/1,rand/1,val/1,tournament/2,
         count/1, frequencies/1, frequency/2, least_freq/1,
         most_freq/1, rand_strategy/1]).


%
% play one strategy against another, for N moves.
%

play_two(StrategyL,StrategyR,N) ->
    play_two(StrategyL,StrategyR,[],[],N).

% tail recursive loop for play_two/3
% 0 case computes the result of the tournament

% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

play_two(_,_,PlaysL,PlaysR,0) ->
   dummy;

play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
   dummy.

%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[]).

% tail recursive loop for play/1

play(Strategy,Moves) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
	stop ->
	    io:format("Stopped~n");
	_    ->
	    Result = result(Play,Strategy(Moves)),
	    io:format("Result: ~p~n",[Result]),
	    play(Strategy,[Play|Moves])
    end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form
    
expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

% result of a tournament

tournament(PlaysL,PlaysR) ->
    lists:sum(
      lists:map(fun outcome/1,
		lists:zipwith(fun result/2,PlaysL,PlaysR))).

outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) ->
    rock;
enum(1) ->
    paper;
enum(2) ->
    scissors.

val(rock) ->
    0;
val(paper) ->
    1;
val(scissors) ->
    2.

% give the play which the argument beats.

beats(rock) ->
    scissors;
beats(paper) ->
    rock;
beats(scissors) ->
    paper.

% give the play which the argument loses to.

loses(rock) ->
    paper;
loses(paper) ->
    scissors;
loses(scissors) ->
    rock.

count(Xs) -> 
    lists:foldr(fun (_, Acc) ->  Acc+1 end, 0, Xs).

frequencies(Xs) -> 
    R = frequency(rock, Xs),
    P = frequency(paper, Xs),
    S = frequency(scissors, Xs),
    [{rock, R}, {paper, P}, {scissors, S}].

frequency(RPS, Xs) -> 
    count(lists:filter(fun(X) -> X == RPS end, Xs)).

%
% strategies.
%
echo([]) ->
     paper;
echo([Last|_]) ->
    Last.

rock(_) ->
    rock.


% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

no_repeat([]) ->
    rock;
no_repeat([X|_]) ->
    loses(X).

const(Play) ->
    dummy.

cycle(Xs) -> 
    enum(count(Xs) rem 3).

rand(_) ->
    enum(random:uniform(3)-1).

least_freq(Xs) ->
    by_freq(fun lists:min/1, Xs).

most_freq(Xs) ->
    by_freq(fun lists:max/1, Xs).

by_freq(Filter, Xs) ->
    Freqs = frequencies(Xs),
    Bound = Filter(lists:map(fun({_,F}) -> F end, Freqs)),
    [{Play,_}|_] = lists:filter(fun({_,X}) -> X == Bound end, Freqs),
    Play.

%combine_strategies(S1, S2) -> fun(Xs) -> S2([S1(Xs)]).
%combine_strategies(Fs) ->
%    lists:foldr(fun combine_strategies/2, fun echo/1, Fs).

% http://learnyousomeerlang.com/types-or-lack-thereof
rand_strategy(Ss) when is_list(Ss) ->
    lists:nth(random:uniform(count(Ss)), Ss).


