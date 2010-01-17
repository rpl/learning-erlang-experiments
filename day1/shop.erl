-module(shop).

-export([cost/1]).

cost([H|T]) ->
    [ cost(H) | cost(T) ];
cost([]) ->
    [];
cost(oranges) ->
    5;
cost(newspaper) ->
    2;
cost(milk) ->
    10.
