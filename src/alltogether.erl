-module(alltogether).
-export([area/1, perimeter/1, enclose/1, bits/1, bitstail/1]).

area({circle, {_X, _Y}, R})
  -> math:pi()*R*R;
area({rectangle, {_X, _Y}, H, W})
  -> H*W.

perimeter({circle, {_X, _Y}, R})
  -> 2*math:pi()*R;
perimeter({rectangle, {_X, _Y}, H, W})
  -> 2*H+2*W.

enclose({circle, {X, Y}, R})
  -> {rectangle, {X-R, Y-R}, 2*R, 2*R};
enclose({rectangle, {X, Y}, H, W})
  -> {rectangle, {X, Y}, H, W}.

bits(0) ->
  0;
bits(N) ->
  (N rem 2) + bits(N div 2).

bitsA(0, D) ->
  D;
bitsA(R, D) ->
  bitsA(R div 2, D + (R rem 2)).

bitstail(N) ->
  bitsA(N, 0).
