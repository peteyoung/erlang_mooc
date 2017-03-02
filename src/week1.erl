-module(week1).
-export([area/1, perimeter/1, enclose/1, sumBitDR/1, sumBitTR/1]).

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


sumBitDR(M, N) when M =< N ->
    trunc((N band M) / M) + sumBitDR(M*2, N);
sumBitDR(_M, _N) ->
    0.

sumBitDR(N) ->
    sumBitDR(1, N).

%% week1:sumBitDR(127) == 7.
%% week1:sumBitDR(128) == 1.
%% week1:sumBitDR(255) == 8.
%% week1:sumBitDR(256) == 1.
%% week1:sumBitDR(254) == 7.
%% week1:sumBitDR(173) == 5.
%% week1:sumBitDR(85) == 4.


sumBitTR(M, N, A) when M =< N ->
    sumBitTR(M*2, N, A + trunc((N band M) / M));
sumBitTR(_M, _N, A) ->
    A.

sumBitTR(N) ->
    sumBitTR(1, N, 0).

%% week1:sumBitTR(127) == 7.
%% week1:sumBitTR(128) == 1.
%% week1:sumBitTR(255) == 8.
%% week1:sumBitTR(256) == 1.
%% week1:sumBitTR(254) == 7.
%% week1:sumBitTR(173) == 5.
%% week1:sumBitTR(85) == 4.
