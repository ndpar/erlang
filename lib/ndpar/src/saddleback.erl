%%
%% @doc Saddleback search.
%%
%% Design a function `invert' that takes two arguments:
%% a function `f: N × N ⟶ N', and
%% a number `z ∈ N'.
%% The value `invert(f, z)' is a list of all pairs `(x,y)'
%% satisfying `f(x,y) = z'.
%% `f' is strictly increasing in each argument.
%%
%% The solution should make as few evaluations of `f' as possible.
%%
%% See performance test results to choose the best algorithm
%% for a given function.
%%
%% @reference [B1] Chapter 3, pp. 12–20
%%
-module(saddleback).
-author("Andrey Paramonov <github@ndpar.com>").

-export([invert/2, invert1/2, invert2/2, invert3/2, invert4/2]).

-import(lists, [seq/2]).

-type natural() :: non_neg_integer().

%%
%% @doc An alias to {@link invert4/2}.
%%
-spec invert(F, Z) -> [{X, Y}] when
  F :: fun((X, Y) -> Z),
  X :: natural(),
  Y :: natural(),
  Z :: natural().

invert(F, Z) -> invert4(F, Z).

%%
%% @doc An execution of the definition of the inverse function.
%% The algorithm traverses the entire search square.
%% ```
%% (0,Z)           (Z,Z)
%%   ┌───────────────┐
%%   ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑
%%   │ │ │ │ │ │ │ │ │
%%   │ │ │ │ │ │ │ │ │
%%   │ │ │ │ │ │ │ │ │
%%   │ │ │ │ │ │ │ │ │
%%   │ │ │ │ │ │ │ │ │
%%   │ │ │ │ │ │ │ │ │
%%   └─┴─┴─┴─┴─┴─┴─┴─┘
%% (0,0)           (Z,0)
%% '''
%% Inefficient (quadratic) but easy to understand.
%%
%% See Jack's algorithm, [B1] p. 12.
%%
-spec invert1(F, Z) -> [{X, Y}] when
  F :: fun((X, Y) -> Z),
  X :: natural(),
  Y :: natural(),
  Z :: natural().

invert1(F, Z) -> [{X, Y} || X <- seq(0, Z), Y <- seq(0, Z), F(X, Y) =:= Z].

%%
%% @doc Basic algorithm.
%% Traverses the diagonal of the search square, from `(0,Z)' to `(Z,0)'.
%% ```
%% (0,Z)           (Z,Z)
%%   ┌───────────────┐
%%   ├─┐             │
%%   │ │             │
%%   │ └──┐          │
%%   │    └─┐        │
%%   │      └─┐      │
%%   │        └─┐    │
%%   │          └─┐  │
%%   └────────────┴──┘
%% (0,0)           (Z,0)
%% '''
%% Runs in linear time in `Z'.
%%
%% See Anne's algorithm, [B1] p. 13.
%% @end
%%
%% Not tail optimized.
%%
-spec invert2(F, Z) -> [{X, Y}] when
  F :: fun((X, Y) -> Z),
  X :: natural(),
  Y :: natural(),
  Z :: natural().

invert2(F, Z) -> find2({0, Z}, F, Z).

find2({U, V}, _F, Z) when U > Z orelse V < 0 -> [];
find2({U, V}, F, Z) ->
  Zf = F(U, V),
  if
    Zf < Z -> find2({U + 1, V}, F, Z);
    Zf =:= Z -> [{U, V} | find2({U + 1, V - 1}, F, Z)];
    Zf > Z -> find2({U, V - 1}, F, Z)
  end.


%%
%% @doc Improvement to the basic algorithm by
%% Gries, Dijkstra, and Backhouse.
%% Replaces the search square with the search rectangle.
%% ```
%% (0,Z)           (Z,Z)
%%   ┌───────────────┐
%%   │               │
%%   │               │
%% M ├┬───────────┐  │
%%   │└─┐         │  │
%%   │  └─┐       │  │
%%   │    └─┐     │  │
%%   │      └─┐   │  │
%%   └────────┴───┴──┘
%% (0,0)         N (Z,0)
%% '''
%% For some functions it can yield logarithmic performance.
%%
%% See Theo's algorithm, [B1] p. 14.
%%
-spec invert3(F, Z) -> [{X, Y}] when
  F :: fun((X, Y) -> Z),
  X :: natural(),
  Y :: natural(),
  Z :: natural().

invert3(F, Z) ->
  {N, M} = range(ext(F), Z),
  find3({0, M}, F, Z, N).

find3({U, V}, _F, _Z, N) when U > N orelse V < 0 -> [];
find3({U, V}, F, Z, _N) -> find2({U, V}, F, Z).

%%
%% @doc Finds better initial search range for function `F'.
%% Narrows `(0,0) (0,Z) (Z,Z) (Z,0)' to `(0,0) (0,M) (N,M) (N,0)'.
%% `F' must be extended to points (0,-1) and (-1,0).
%%
range(F, Z) ->
  {
    bsearch(fun(X) -> F(X, 0) end, {-1, Z + 1}, Z),
    bsearch(fun(Y) -> F(0, Y) end, {-1, Z + 1}, Z)
  }.

%%
%% @doc Binary search to determine `M' such that
%%```
%% M = bsearch(G, {A, B}, Z), A ≤ M < B, G(M) ≤ Z < G(M + 1)'''
%%
bsearch(_, {A, B}, _) when A + 1 =:= B -> A;
bsearch(G, {A, B}, Z) ->
  M = (A + B) div 2,
  bsearch(G, {A, B}, Z, M, G(M)).

bsearch(G, {_, B}, Z, M, Gm) when Gm =< Z -> bsearch(G, {M, B}, Z);
bsearch(G, {A, _}, Z, M, _) -> bsearch(G, {A, M}, Z).

%%
%% @doc Extends function F to points (0,-1) and (-1,0).
%%
ext(F) ->
  fun(X, Y) ->
    case {X, Y} of
      {0, -1} -> 0;
      {-1, 0} -> 0;
      _ -> F(X, Y)
    end
  end.


%%
%% @doc Divide and conquer version of {@link invert3/2}.
%% ```
%% (0,Z)           (Z,Z)
%%   ┌───────────────┐
%%   │               │
%%   ├───┐           │
%%   ├───┘.          │
%%   │     ┌─┐       │
%%   │     └─┘ .     │
%%   │           ┌──┐│
%%   └───────────┴──┴┘
%% (0,0)           (Z,0)
%% '''
%% Asymptotically optimal saddleback search algorithm.
%%
%% See Mary's algorithm, [B1] p. 18.
%% @end
%%
%% Not tail optimized though and with list concatenations.
%%
-spec invert4(F, Z) -> [{X, Y}] when
  F :: fun((X, Y) -> Z),
  X :: natural(),
  Y :: natural(),
  Z :: natural().

invert4(F, Z) ->
  {N, M} = range(ext(F), Z),
  find4({0, M}, {N, 0}, F, Z).

find4({U, V}, {R, S}, _F, _Z) when U > R orelse V < S -> [];
find4({U, V}, {R, S}, F, Z) ->
  P = (U + R) div 2,
  Q = (V + S) div 2,
  RFind = fun(X) ->
    case F(X, Q) of
      Z -> [{X, Q} | find4({U, V}, {X - 1, Q + 1}, F, Z)];
      _ -> find4({U, V}, {X, Q + 1}, F, Z)
    end ++
    find4({X + 1, Q - 1}, {R, S}, F, Z)
          end,
  CFind = fun(Y) ->
    find4({U, V}, {P - 1, Y + 1}, F, Z) ++
    case F(P, Y) of
      Z -> [{P, Y} | find4({P + 1, Y - 1}, {R, S}, F, Z)];
      _ -> find4({P + 1, Y}, {R, S}, F, Z)
    end
          end,
  if
    V - S =< R - U -> RFind(bsearch(fun(X) -> F(X, Q) end, {U - 1, R + 1}, Z));
    true -> CFind(bsearch(fun(Y) -> F(P, Y) end, {S - 1, V + 1}, Z))
  end.

%% =============================================================================
%% Unit tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

invert_test_() ->
  F = fun(X, Y) -> X + Y end,
  Result = [{0, 5}, {1, 4}, {2, 3}, {3, 2}, {4, 1}, {5, 0}],
  [
    ?_assertEqual(Result, invert1(F, 5)),
    ?_assertEqual(Result, invert2(F, 5)),
    ?_assertEqual(Result, invert3(F, 5)),
    ?_assertEqual([{3, 2}, {1, 4}, {0, 5}, {2, 3}, {5, 0}, {4, 1}], invert4(F, 5))
  ].

ext_test_() ->
  F = fun(X, Y) -> X + Y end,
  G = ext(F),
  [
    ?_assertEqual(0, G(0, -1)),
    ?_assertEqual(0, G(-1, 0)),
    ?_assertEqual(F(3, 4), G(3, 4))
  ].

-endif.
