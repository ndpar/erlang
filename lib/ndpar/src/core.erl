%%
%% @doc A collection of frequently used functions.
%% Inspired by Clojure and Haskell.
%%
-module(core).
-export([cross/2]).
-export([frequencies/1, group_by/2, inc/1, minfree/1, msc/1, msc2/1]).
-export([zipfold/4, zipfold/5]).

-import(lists, [filter/2, map/2, max/1]).

%%
%% @doc Applies pair of functions to a pair of arguments.
%%
-spec cross(Funs :: {F, G}, Args :: {X, Y}) -> {A, B} when
  F :: fun((X) -> A),
  G :: fun((Y) -> B),
  X :: term(),
  Y :: term(),
  A :: term(),
  B :: term().

cross({F, G}, {X, Y}) -> {F(X), G(Y)}.

%%
%% @doc Returns a map from distinct items in List to the number of times they appear.
%%
%% See [https://clojuredocs.org/clojure.core/frequencies]
%%
-spec frequencies([A]) -> #{A => pos_integer()}.

frequencies(List) ->
  lists:foldl(fun update_count/2, #{}, List).

update_count(X, Map) ->
  maps:update_with(X, fun(C) -> C + 1 end, 1, Map).

%%
%% @doc Returns a map of the elements of List keyed by the result of
%% Fun on each element. The value at each key will be a list of the
%% corresponding elements, in the order they appeared in List.
%%
%% See [https://clojuredocs.org/clojure.core/group-by]
%%
group_by(Fun, List) ->
  F = fun(E, Map) ->
    K = Fun(E),
    maps:update_with(K, fun(L) -> [E | L] end, [E], Map)
  end,
  maps:map(fun(_, V) -> lists:reverse(V) end, lists:foldl(F, #{}, List)).

%%
%% @doc Returns a number one greater than X.
%%
-spec inc(X :: number()) -> number().

inc(X) -> X + 1.

%%
%% @doc Zips the elements of the given lists
%% left folding them with the accumulator.
%%
-spec zipfold(F, Acc, [A], [B]) -> Acc when
  F :: fun((Acc, A, B) -> Acc),
  Acc :: term(),
  A :: term(),
  B :: term().

zipfold(F, Acc, [], []) when is_function(F) -> Acc;
zipfold(F, Acc, [A | As], [B | Bs]) ->
  zipfold(F, F(Acc, A, B), As, Bs).

%%
%% @doc Zips the elements of the given lists
%% left folding them with the accumulator.
%%
-spec zipfold(F, Acc, [A], [B], [C]) -> Acc when
  F :: fun((Acc, A, B, C) -> Acc),
  Acc :: term(),
  A :: term(),
  B :: term(),
  C :: term().

zipfold(F, Acc, [], [], []) when is_function(F) -> Acc;
zipfold(F, Acc, [A | As], [B | Bs], [C | Cs]) ->
  zipfold(F, F(Acc, A, B, C), As, Bs, Cs).


%%
%% @doc The smallest free number.
%%
%% Computes the smallest natural number not in a given finite list
%% of <em>unique</em> natural numbers.
%%
%% This algorithm takes linear time and space (tail-optimized).
%% In comparison, the straightforward algorithm is quadratic:
%% ```
%% hd(lists:seq(0, N) -- List)'''
%%
%% See [B1] Chapter 1, pp. 1–6.
%%
-spec minfree([non_neg_integer()]) -> non_neg_integer().

minfree(List) -> minfrom(0, {length(List), List}).

minfrom(A, {0, []}) -> A;
minfrom(A, {N, List}) ->
  B = A + 1 + N div 2,
  {Us, Vs} = lists:partition(fun(X) -> X < B end, List), % Θ(N)
  M = length(Us),
  case B - A of
    M -> minfrom(B, {N - M, Vs});
    _ -> minfrom(A, {M, Us})
  end.


%%
%% @doc Maximum surpasser count.
%%
%% `x[j]' is a surpasser of `x[i]' if `i < j' and `x[i] < x[j]'.
%% The <em>surpasser count</em> of an element is the number of
%% its surpassers.
%%
%% The complexity of the divide and conquer version of the MSC algorithm is `O(n log n)'.
%%
%% See [B1] Chapter 2, pp. 7–11.
%%
%% @see msc2/1
%%
-spec msc(list()) -> non_neg_integer().

msc(List) -> max([C || {_, C} <- table(List)]).


-spec table([A]) -> [{A, Count :: non_neg_integer()}].

table([X]) -> [{X, 0}];
table(List) ->
  M = length(List),
  N = M div 2,
  {Ys, Zs} = lists:split(N, List),
  join(M - N, table(Ys), table(Zs)).

-spec join(non_neg_integer(), [A], [A]) -> [A].

join(0, Txs, []) -> Txs;
join(_N, [], Tys) -> Tys;
join(N, [{X, C} | Txst] = Txs, [{Y, D} | Tyst] = Tys) ->
  case X < Y of
    true -> [{X, C + N} | join(N, Txst, Tys)];
    false -> [{Y, D} | join(N - 1, Txs, Tyst)]
  end.


%%
%% @doc MSC algorithm running in quadratic time.
%%
-spec msc2(list()) -> non_neg_integer().

msc2(List) -> max([scount(X, Xs) || [X | Xs] <- tails(List)]).

scount(X, Xs) -> length(filter(fun(Y) -> X < Y end, Xs)).

tails([]) -> [];
tails([_ | Xs] = List) -> [List | tails(Xs)].


%% =============================================================================
%% Unit tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

cross_test_() ->
  F = fun(X) -> 2 * X end,
  G = fun(Y) -> 3 * Y end,
  [
    ?_assertEqual({2, 6}, cross({F, G}, {1, 2}))
  ].

zipfold_test_() -> [
  ?_assertEqual(2 + 6 + 12,
    zipfold(fun(Acc, A, B) -> Acc + A * B end, 0, [1, 2, 3], [2, 3, 4])),
  ?_assertEqual(6 + 24 + 60,
    zipfold(fun(Acc, A, B, C) -> Acc + A * B * C end, 0, [1, 2, 3], [2, 3, 4], [3, 4, 5]))].

frequencies_test() ->
  ?assertEqual(#{1 => 2, 2 => 2, 3 => 3, 4 => 1},
    frequencies([1, 2, 3, 2, 3, 4, 1, 3])).

group_by_test() ->
  ?assertEqual(#{1 => ["a"], 2 => ["as", "aa"], 3 => ["asd"], 4 => ["asdf", "qwer"]},
    group_by(fun erlang:length/1, ["a", "as", "asd", "aa", "asdf", "qwer"])).

minfree_test_() ->
  List = [4, 0, 5, 7, 3, 10, 2, 1],
  [
    ?_assertEqual(0, minfree(lists:seq(1, 10))),
    ?_assertEqual(0, minfree(lists:reverse(lists:seq(1, 10)))),
    ?_assertEqual(9, minfree(lists:seq(0, 8))),
    ?_assertEqual(9, minfree(lists:reverse(lists:seq(0, 8)))),
    ?_assertEqual(6, minfree(List)),
    ?_assertEqual(hd(lists:seq(0, 8) -- List), minfree(List))
  ].

scount([X | Xs]) -> scount(X, Xs).

msc_test_() ->
  Word = "GENERATING",
  [
    ?_assertEqual(5, scount(hd(Word), tl(Word))),
    ?_assertEqual([5, 6, 2, 5, 1, 4, 0, 1, 0, 0], map(fun scount/1, tails(Word))),
    ?_assertEqual(6, msc2(Word)),
    ?_assertEqual(6, msc(Word)),
    ?_assertEqual([6, 6, 5, 5, 4, 4, 1, 1, 0, 0], map(fun msc/1, tails(Word)))
  ].

tails_test_() -> [
  ?_assertEqual([[1, 2, 3], [2, 3], [3]], tails([1, 2, 3]))
].

-endif.
