-module(life).
-import(lists, [flatmap/2]).
-import(sets, [from_list/1, to_list/1, is_element/2]).
-export([neighbours/1, next_step/1, frequencies/1]).

neighbours({X, Y}) ->
    [{X + DX, Y + DY} || DX <- [-1, 0, 1], DY <- [-1, 0, 1], {DX, DY} =/= {0, 0}].

next_step(Cells) ->
    Nbs = flatmap(fun neighbours/1, to_list(Cells)),
    NewCells = [C || {C, N} <- frequencies(Nbs),
                     (N == 3) orelse ((N == 2) andalso is_element(C, Cells))],
    from_list(NewCells).

frequencies(List) -> frequencies(List, []).
frequencies([], Acc) -> Acc;
frequencies([X|Xs], Acc) ->
    case lists:keyfind(X, 1, Acc) of
        {X, F} -> frequencies(Xs, lists:keyreplace(X, 1, Acc, {X, F+1}));
        false  -> frequencies(Xs, lists:keystore(X, 1, Acc, {X, 1}))
    end.

%
% Unit tests
%

-include_lib("eunit/include/eunit.hrl").

frequencies_test() ->
    ?assertEqual([{1,2}, {2,2}, {3,3}, {4,1}],
        frequencies([1, 2, 3, 2, 3, 4, 1, 3])).

neighbours_test() ->
    ?assertEqual([{0,1}, {0,2}, {0,3}, {1,1}, {1,3}, {2,1}, {2,2}, {2,3}],
        neighbours({1, 2})).

blinker_test() ->
    assert_next_step([{2,3}, {3,3}, {4,3}], [{3,2}, {3,3}, {3,4}]),
    assert_next_step([{3,2}, {3,3}, {3,4}], [{2,3}, {3,3}, {4,3}]).

beehive_test() ->
    assert_next_step([{3,2}, {2,3}, {2,4}, {3,5}, {4,4}, {4,3}],
        [{3,2}, {2,3}, {2,4}, {3,5}, {4,4}, {4,3}]).

assert_next_step(ListAfter, ListBefore) ->
    ?assertEqual(from_list(ListAfter), next_step(from_list(ListBefore))).
