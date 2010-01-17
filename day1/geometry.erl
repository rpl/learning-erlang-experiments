-module(geometry).

-export([area/1]).

area({rect, W, H}) ->
    W * H;
area({square, W}) ->
    W * W;
area({circle, R}) ->
    3.14 * R *R.

