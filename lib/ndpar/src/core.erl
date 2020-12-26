%%
%% @doc A collection of frequently used functions.
%% Inspired by Clojure and Haskell.
%%
-module(core).
-export([frequencies/1, group_by/2, inc/1, minfree/1]).
-export([zipfold/4, zipfold/5]).

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

%% =============================================================================
%% Unit tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

zipfold_test_() -> [
  ?_assertEqual(2 + 6 + 12,
    zipfold(fun(Acc, A, B) -> Acc + A * B end, 0, [1, 2, 3], [2, 3, 4])),
  ?_assertEqual(6 + 24 + 60,
    zipfold(fun(Acc, A, B, C) -> Acc + A * B * C end, 0, [1, 2, 3], [2, 3, 4], [3, 4, 5]))].

frequencies_test() ->
  ?assertEqual(#{1=>2, 2=>2, 3=>3, 4=>1},
    frequencies([1, 2, 3, 2, 3, 4, 1, 3])).

group_by_test() ->
  ?assertEqual(#{1=>["a"], 2=>["as", "aa"], 3=>["asd"], 4=>["asdf", "qwer"]},
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

-endif.
