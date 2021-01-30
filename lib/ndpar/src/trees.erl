%%
%% @doc TODO
%%
%% @reference [B1] Chapter 7, pp. 41–49
%%
-module(trees).
-author("Andrey Paramonov <github@ndpar.com>").

-export([trees/1]).
-export([forests/1, prefixes/2, rollup/1]).

-import(lists, [concat/1, foldl/3, map/2]).

-type tree(Value) :: {leaf, Value} | {fork, tree(Value), tree(Value)}.
-type forest(Value) :: [tree(Value)].


-spec trees(Fringe :: [integer()]) -> [tree(integer())].
trees(Fringe) -> map(fun rollup/1, forests(Fringe)).


-spec forests(Fringe :: [integer()]) -> [forest(integer())].
forests(Fringe) ->
  F = fun(P, Ts) -> concat(map(fun(T) -> prefixes(P, T) end, Ts)) end,
  G = fun(X) -> [[{leaf, X}]] end,
  foldrn(F, G, Fringe).


-spec prefixes(X :: integer(), Ts :: forest(integer())) -> [forest(integer())].
prefixes(X, Ts) ->
  [[{leaf, X} | split_and_rollup(K, Ts)] || K <- lists:seq(1, length(Ts))].

split_and_rollup(K, Ts) ->
  {L, R} = lists:split(K, Ts),
  [rollup(L) | R].

%%
%% @doc Left fold forest (lists of trees) into a single tree.
%%
-spec rollup(Forest :: forest(T)) -> tree(T).
rollup(Forest) ->
  F = fun(L, R) -> {fork, L, R} end,
  core:foldl1(F, Forest).

cost({leaf, X}) -> X;
cost({fork, U, V}) -> 1 + max(cost(U), cost(V)).

mincost(Fringe) -> core:min_by(fun cost/1, trees2(Fringe)).

%%
%% @doc General right fold on nonempty lists.
%%
-spec foldrn(F, G, [A]) -> B when
  F :: fun((A, B) -> B),
  G :: fun((A) -> B),
  A :: term(),
  B :: term().

foldrn(_, G, [X]) -> G(X);
foldrn(F, G, [X | Xs]) -> F(X, foldrn(F, G, Xs)).

trees2(Fringe) ->
  F = fun(P, Ts) -> concat(map(fun(T) -> prefixes2(P, T) end, Ts)) end,
  G = fun(X) -> [{leaf, X}] end,
  foldrn(F, G, Fringe).

prefixes2(X, {leaf, _} = T) -> [{fork, {leaf, X}, T}];
prefixes2(X, {fork, U, V} = T) ->
  Us = [{fork, Un, V} || Un <- prefixes2(X, U)],
  [{fork, {leaf, X}, T} | Us].

%% =============================================================================
%% Unit tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

trees_test_() ->
  [
    ?_assertEqual(14, length(trees([1, 2, 3, 4, 5])))
  ].

foldrn_test_() ->
  F = fun(X, Y) -> X + Y end,
  G = fun(X) -> X + 1 end,
  [
    ?_assertEqual(16, foldrn(F, G, [1, 2, 3, 4, 5]))
  ].

trees2_test_() ->
  [
    ?_assertEqual(14, length(trees2([1, 2, 3, 4, 5])))
  ].

mincost_test_() ->
  [
    ?_assertEqual(
      {fork,
        {fork,
          {fork,
            {fork,
              {leaf, 1},
              {leaf, 2}
            },
            {leaf, 3}
          },
          {leaf, 4}
        },
        {leaf, 5}
      },
      mincost([1, 2, 3, 4, 5])
    )
  ].

-endif.
