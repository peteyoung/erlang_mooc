-module(week1).
-export([area/1, perimeter/1, enclose/1, sumBitDR/1, sumBitDR/2]).

area({circle, R}) ->
    math:pi()*R*R;
area({rectangle, W, H}) ->
    H*W;
%% using Heron's formula
area({triangle, A, B, C}) ->
    S = (A+B+C)/2,
    math:sqrt(S*(S-A)*(S-B)*(S-C)).
    
perimeter({circle, R}) ->
    math:pi()*R*2;
perimeter({rectangle, W, H}) ->
    (H*2)+(W*2);
perimeter({triangle, A, B, C}) ->
    A+B+C.

%% week1:area({circle, 3}) == 3*3*math:pi().
%% week1:perimeter({circle, 3}) == (3+3)*math:pi().

%% week1:area({rectangle, 3, 5}) == 15.
%% week1:perimeter({rectangle, 3, 5}) == 16.

%% week1:area({triangle, 3, 4, 5}) == 6.0.
%% week1:area({triangle, 5, 12, 13}) == 30.0.

%% week1:perimeter({triangle, 3, 4, 5}) == 12.
%% week1:perimeter({triangle, 5, 12, 13}) == 30.


enclose({circle, R}) ->
    {rectangle, R*2, R*2};
enclose({rectangle, W, H}) ->
    {rectangle, W, H};
enclose({triangle, A, B, C}) ->
    P = area({triangle, A, B, C}),
    %% assume base is B
    H = (P*2)/B,
    %% doesn't work for obtuse triangles
    {rectangle, B, H}.

%% week1:enclose({circle, 3}) == {rectangle, 6, 6}.
%% week1:enclose({rectangle, 3, 5}) == {rectangle, 3, 5}.
%% week1:enclose({triangle, 3, 4, 5}) == {rectangle,4,3.0}.


sumBitDR(M, N) when M < N ->
    %trunc((N band M) / M) + sumBitDR(M*2, N);
    R = trunc((N band M) / M),
    io:fwrite("N = ~p, M = ~p, R = ~p~n", [N,M,R]), 
    R + sumBitDR(M*2, N);
sumBitDR(M, N) ->
    io:fwrite("N = ~p, M = ~p, R = 0~n", [N,M]), 
    0.

sumBitDR(N) ->
    sumBitDR(1, N).

