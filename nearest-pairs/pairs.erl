%% Return the two closest pairs of a list of coordinates

-module(pairs).
-export([squaredist/1, dist/1, pairs/1, closest/1, pairdist/1]).

%% Return the square of the distance between two points
squaredist([{A1, A2}, {B1, B2}]) ->
    math:pow((B1-A1), 2)+ math:pow((B2-A2), 2).

dist([{A1, A2}, {B1, B2}]) ->
    math:sqrt(squaredist([{A1, A2}, {B1, B2}])).

pairs([_]) ->
    [];
pairs([A, B]) ->
    [A | B];
pairs([H | T]) ->
    [[H | El] || El <- T] ++ pairs(T).

pairdist([_]) ->
    [];
pairdist([A, B]) ->
    [{pair, A, B, squaredist([A, B])}];
pairdist([H | T]) ->
    [{pair, H, El, squaredist([H, El])} || El <- T] ++ pairdist(T).


