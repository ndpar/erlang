%%
%% @doc TODO
%%
%% @reference [B1] Chapter 7, pp. 41–49
%%
-module(trees).
-author("Andrey Paramonov <github@ndpar.com>").

-export([]).

-import(lists, [concat/1, map/2]).

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
