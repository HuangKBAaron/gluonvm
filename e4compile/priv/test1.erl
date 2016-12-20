-module(test1).
-compile([export_all]).

map_1(_, nil) -> nil;
map_1(F, {K, V, Smaller, Larger}) ->
    {K, F(K, V), map_1(F, Smaller), map_1(F, Larger)}.

linear_pattern(F, {F, X}) -> F + X.

iterator({_, _, nil, _} = T, As) ->
    [T | As];
iterator({_, _, L, _} = T, As) ->
    iterator(L, [T | As]);
iterator(nil, As) ->
    As.
