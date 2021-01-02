%%
%% @doc One of the implementations of
%% [https://groups.google.com/g/erlang-programming/c/ZUHZpH0wsOA
%% coinductive data types].
%%
%% @see lazy2
%%
-module(lazy).
-author("Andrey Paramonov <github@ndpar.com>").

-export([gen/2, filter/2, foldl/3, map/2, take/2]).
-export([natural_numbers/0]).

-dialyzer(no_improper_lists).

-type lazy_seq() :: fun(() -> [term() | lazy_seq()]).
-type integers() :: fun(() -> [pos_integer() | integers()]).

%%
%% @doc Generates lazy (infinite) sequence of elements `E'
%% using generating function `F'.
%%
-spec gen(E0, F) -> lazy_seq() when
  F :: fun((ECurrent) -> ENext),
  E0 :: term(),
  ECurrent :: term(),
  ENext :: term().

gen(E, F) -> fun() -> [E | gen(F(E), F)] end.

%%
%% @doc Generates sequence of integers.
%%
-spec integers_from(pos_integer()) -> integers().
integers_from(K) -> gen(K, fun(N) -> N + 1 end).

%%
%% @doc Generates sequence of natural numbers.
%%
-spec natural_numbers() -> integers().
natural_numbers() -> integers_from(1).

%%
%% @doc Filters the lazy sequence `CE' using predicate `P'.
%%
-spec filter(P :: Predicate, CE :: lazy_seq()) -> fun(() -> lazy_seq()) when
  Predicate :: fun((term()) -> boolean()).

filter(P, CE) ->
  fun() ->
    case CE() of
      [] -> [];
      [X | Xs] ->
        case P(X) of
          true -> [X | filter(P, Xs)];
          false -> (filter(P, Xs))()
        end
    end
  end.

%%
%% @doc Left folds the lazy sequence `CE' using function `F' and accumulator `A'.
%%
-spec foldl(F, Acc0 :: Acc, lazy_seq()) -> Acc1 :: Acc when
  F :: fun((term(), AccIn :: Acc) -> AccOut :: Acc),
  Acc :: term().

foldl(F, A, CE) ->
  case CE() of
    [] -> A;
    [X | Xs] -> foldl(F, F(A, X), Xs)
  end.

%%
%% @doc Maps the lazy sequence `CE' using function `F'.
%%
-spec map(F, lazy_seq()) -> lazy_seq() when
  F :: fun((term()) -> term()).

map(F, CE) ->
  fun() ->
    case CE() of
      [] -> [];
      [X | Xs] -> [F(X) | map(F, Xs)]
    end
  end.

%%
%% @doc Returns first `N' elements of the given lazy sequence.
%%
-spec take(non_neg_integer(), lazy_seq()) -> [term()].

take(N, LazySeq) -> take([], N, LazySeq).

take(A, 0, _) -> lists:reverse(A);
take(A, N, CE) ->
  [X | Xs] = CE(),
  take([X | A], N - 1, Xs).

%% =============================================================================
%% Unit tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

filter_first(N, P, CE) -> take(N, filter(P, CE)).

foldl_first(0, _, A, _) -> A;
foldl_first(N, F, A, CE) ->
  case CE() of
    [] -> A;
    [X | Xs] -> foldl_first(N - 1, F, F(A, X), Xs)
  end.

map_first(N, F, CE) -> take(N, map(F, CE)).

first_natural_numbers(N) -> take(N, natural_numbers()).

first_even_numbers(N) -> filter_first(N, fun(X) -> X rem 2 =:= 0 end, natural_numbers()).

first_squares(N) -> map_first(N, fun(X) -> X * X end, natural_numbers()).

first_sum(N) -> foldl_first(N, fun(X, Sum) -> X + Sum end, 0, natural_numbers()).

filter_test() ->
  F = filter(fun(X) -> 10 < X end, natural_numbers()),
  [X | _] = F(),
  ?assertEqual(11, X).

map_test() ->
  [X | _] = (map(fun(X) -> X * 2 end, natural_numbers()))(),
  ?assertEqual(2, X).

first_natural_numbers_test() ->
  ?assertEqual([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], first_natural_numbers(10)).

first_even_numbers_test_() -> [
  ?_assertEqual([2, 4, 6, 8, 10, 12, 14, 16, 18, 20], first_even_numbers(10)),
  ?_assertEqual([2, 4, 6, 8, 10, 12, 14, 16, 18, 20], take(10, gen(2, fun(N) -> N + 2 end)))
].

first_squares_test() ->
  ?assertEqual([1, 4, 9, 16, 25, 36, 49, 64, 81, 100], first_squares(10)).

powers_of_two_test() ->
  ?assertEqual([1, 2, 4, 8, 16, 32, 64, 128, 256, 512], take(10, gen(1, fun(N) -> 2 * N end))).

first_sum_test() ->
  ?assertEqual(55, first_sum(10)).

-endif.
