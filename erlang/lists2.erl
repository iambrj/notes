-module(lists2).
-export([flat_length/1, flatten/1]).

flat_length(List) ->
    flat_length(List, 0).

% Unlike, say C, same name can be used for functions with different number
% arguments
flat_length([H | T], Acc) when is_list(H) -> % guards may NOT have new vars
    flat_length(H, flat_length(T, Acc));
flat_length([_ | T], Acc) ->
    flat_length(T, Acc + 1);
flat_length([], Acc) -> Acc.

flatten([H | T]) when is_list(H) ->
    flatten(H) ++ flatten(T);
flatten([H | T]) ->
    [H] ++ flatten(T);
flatten([]) -> [].


