-module(howManyPieces).
-export([howMany/1]).
%% lines  verts  pieces
%%     0      0       1
%%     1      0       2
%%     2      1       4
%%     3      3       7
%%     4      6      11
%%     5     10      16

%%  lines + pieces(lines - 1)

howMany(0) ->
    1;
howMany(Lines) when Lines > 0 ->
    Lines + howMany(Lines - 1).

