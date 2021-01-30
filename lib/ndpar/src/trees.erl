%%
%% @doc TODO
%%
%% @reference [B1] Chapter 7, pp. 41–49
%%
-module(trees).
-author("Andrey Paramonov <github@ndpar.com>").

-export([cost/1, mincost_tree/1, trees/1]).

-import(lists, [concat/1, foldl/3, map/2]).

-type tree(Value) :: {leaf, Value} | {fork, tree(Value), tree(Value)}.
-type forest(Value) :: [tree(Value)].


trees2(Fringe) ->
  F = fun(P, Ts) -> concat(map(fun(T) -> prefixes2(P, T) end, Ts)) end,
  G = fun(X) -> [{leaf, X}] end,
  foldrn(F, G, Fringe).

prefixes2(X, {leaf, _} = T) -> [{fork, {leaf, X}, T}];
prefixes2(X, {fork, U, V} = T) ->
  Us = [{fork, Un, V} || Un <- prefixes2(X, U)],
  [{fork, {leaf, X}, T} | Us].


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

-spec cost(tree(integer())) -> integer().
cost({leaf, X}) -> X;
cost({fork, U, V}) -> 1 + max(cost(U), cost(V)).

mincost_tree_2(Fringe) -> core:min_by(fun cost/1, trees2(Fringe)).

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

%% =============================================================================
%% Final algorithm
%% =============================================================================

-spec mincost_tree(Fringe :: [integer()]) -> tree(integer()).
mincost_tree(Fringe) ->
  G = fun(X) -> [leaf(X)] end,
  TCs = foldrn(fun insert/2, G, Fringe),
  F = fun(L, R) -> {fork, L, R} end,
  core:foldl1(F, [T || {_, T} <- TCs]).

insert(X, Ts) -> [leaf(X) | split(X, Ts)].

split(_, [U]) -> [U];
split(X, [{Cu, _} = U, {Cv, _} = V | Ts] = Spine) ->
  Cxu = max(X, Cu),
  if
    Cxu < Cv -> Spine;
    true -> split(X, fork(U, [V | Ts]))
  end.

%% Smart constructors to pair tree node with its cost.

leaf(X) -> {X, {leaf, X}}.
fork({A, U}, {B, V}) -> {1 + max(A, B), {fork, U, V}}.

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

mincost_tree_test_() ->
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
      mincost_tree_2([1, 2, 3, 4, 5])
    ),
    ?_assertEqual(
      mincost_tree_2([1, 2, 3, 4, 5]),
      mincost_tree([1, 2, 3, 4, 5])
    )
  ].

-endif.
