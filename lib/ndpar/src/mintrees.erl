%%
%% @doc Problem: given a sequence `Xs = [X1, X2,..., Xn]' of natural numbers,
%% find a linear-time algorithm to build a tree with fringe `Xs' that
%% minimises {@link cost/1. cost} function.
%%
%% The fringe of a tree is the list of labels at the leaves in left-to-right order.
%%
%% The presented {@link mincost_tree/1. mincost_tree} algorithm can be used
%% to solve the following problem:
%%
%% Given an arbitrary list of trees `Ts = [T1, T2,..., Tn]' together with their
%% heights `Hs = [H1, H2,..., Hn]', find a linear time algorithm to combine
%% them into a single tree `T' of minimum height.
%% Trees `Ts' should appear as subtrees of the tree `T' in the same order
%% as they appear in the list `Ts'.
%%
%% The solution is: run {@link mincost_tree/1. mincost_tree} algorithm for
%% fringe `Hs'. In the resulting tree replace all leaves `Hi' with the
%% corresponding tree `Ti'.
%%
%% @reference [B1] Chapter 7, pp. 41–49
%%
-module(mintrees).
-author("Andrey Paramonov <github@ndpar.com>").

-export([cost/1]).
-export([mincost_tree/1, trees/1]).
-export([mincost_tree2/1, trees2/1]).

-import(core, [foldl1/2]).
-import(lists, [concat/1, foldl/3, map/2]).

%% =============================================================================
%% Types
%% =============================================================================

-type tree(Type) :: {leaf, Type} | {fork, tree(Type), tree(Type)}.
-type forest(Type) :: [tree(Type)].

%%
%% @doc Defines cost of the integer tree.
%%
-spec cost(Tree :: tree(integer())) -> integer().

cost({leaf, X}) -> X;
cost({fork, U, V}) -> 1 + max(cost(U), cost(V)).


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
%% Exponential algorithms
%% =============================================================================

%%
%% @doc Returns a minimal cost tree for the given fringe.
%% @see trees2/1
%%
-spec mincost_tree2(Fringe :: [integer()]) -> tree(integer()).

mincost_tree2(Fringe) -> core:min_by(fun cost/1, trees2(Fringe)).


%%
%% @doc Returns list of trees for the given fringe.
%% Intuitive algorithm to build trees by prefixing
%% previously built right sub-trees.
%%
-spec trees2(Fringe :: [integer()]) -> [tree(integer())].

trees2(Fringe) ->
  F = fun(P, Ts) -> concat(map(fun(T) -> prefixes2(P, T) end, Ts)) end,
  G = fun(X) -> [{leaf, X}] end,
  foldrn(F, G, Fringe).

prefixes2(X, {leaf, _} = T) -> [{fork, {leaf, X}, T}];
prefixes2(X, {fork, U, V} = T) ->
  Us = [{fork, Un, V} || Un <- prefixes2(X, U)],
  [{fork, {leaf, X}, T} | Us].


%%
%% @doc Returns list of trees for the given fringe.
%% Another algorithm to build trees by rolling up
%% left spines
%%
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
  foldl1(F, Forest).

%% =============================================================================
%% Linear algorithm
%% =============================================================================

%%
%% @doc Returns a minimal cost tree for the given fringe.
%%
-spec mincost_tree(Fringe :: [integer()]) -> tree(integer()).

mincost_tree(Fringe) ->
  G = fun(X) -> [leaf(X)] end,
  TCs = foldrn(fun insert/2, G, Fringe),
  F = fun(L, R) -> {fork, L, R} end,
  foldl1(F, [T || {_, T} <- TCs]).

insert(X, Ts) -> [leaf(X) | split(X, Ts)].

split(_, [U]) -> [U];
split(X, [{Cu, _} = U, {Cv, _} = V | Ts] = Spine) ->
  Cxu = max(X, Cu),
  if
    Cxu < Cv -> Spine;
    true -> split(X, [fork(U, V) | Ts])
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
      mincost_tree2([1, 2, 3, 4, 5])
    ),
    ?_assertEqual(
      mincost_tree2([1, 2, 3, 4, 5]),
      mincost_tree([1, 2, 3, 4, 5])
    )
  ].

-endif.
