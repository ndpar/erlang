%%
%% @doc Another implementations of
%% [https://groups.google.com/g/erlang-programming/c/ZUHZpH0wsOA
%% coinductive data types].
%%
%% @see lazy
%%
-module(lazy2).
-author("Andrey Paramonov <github@ndpar.com>").

-export([gen/2, filter/2, foldl/3, map/2, take/2]).
-export([natural_numbers/0]).

-dialyzer(no_improper_lists).

-type lazy_seq() :: [term() | fun(() -> lazy_seq())].
-type integers() :: [pos_integer() | fun(() -> integers())].

%%
%% @doc Generates lazy (infinite) sequence of elements `E'
%% using generating function `F'.
%%
gen(E, F) -> [E | fun() -> gen(F(E), F) end].

%%
%% @doc Generates sequence of integers.
%%
integers_from(K) -> gen(K, fun(N) -> N + 1 end).

%%
%% @doc Generates sequence of natural numbers.
%%
natural_numbers() -> integers_from(1).

%%
%% @doc Filters the given lazy sequence using predicate `Pred'.
%%
filter(_, []) -> [];
filter(Pred, [X | Gen]) ->
  case Pred(X) of
    true -> [X | fun() -> filter(Pred, Gen()) end];
    false -> filter(Pred, Gen())
  end.

%%
%% @doc Left folds the given lazy sequence using function `Fun' and accumulator `Acc'.
%%
foldl(_, Acc, []) -> {Acc, []};
foldl(Fun, Acc, [X | Gen]) -> {Fun(X, Acc), Gen()}.

%%
%% @doc Maps the given lazy sequence using function `Fun'.
%%
map(_, []) -> [];
map(Fun, [X | Gen]) -> [Fun(X) | fun() -> map(Fun, Gen()) end].

%%
%% @doc Returns first `N' elements of the given lazy sequence.
%%
take(N, LazySeq) -> take([], N, LazySeq).

take(Acc, 0, _) -> Acc;
take(Acc, N, [X | Gen]) -> take(Acc ++ [X], N - 1, Gen()).

%% =============================================================================
%% Unit tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

filter_first(N, Pred, LazyList) -> take(N, filter(Pred, LazyList)).

foldl_first(0, _, Acc, _) -> Acc;
foldl_first(N, Fun, Acc, LazyList) ->
  {NewAcc, LazyTail} = foldl(Fun, Acc, LazyList),
  foldl_first(N - 1, Fun, NewAcc, LazyTail).

map_first(N, Fun, LazyList) -> take(N, map(Fun, LazyList)).

first_natural_numbers(N) -> take(N, natural_numbers()).

first_even_numbers(N) -> filter_first(N, fun(X) -> X rem 2 == 0 end, natural_numbers()).

first_squares(N) -> map_first(N, fun(X) -> X * X end, natural_numbers()).

first_sum(N) -> foldl_first(N, fun(X, Sum) -> X + Sum end, 0, natural_numbers()).

filter_test() ->
  [X | _] = filter(fun(X) -> 10 < X end, natural_numbers()),
  ?assertEqual(11, X).

foldl_test() ->
  {P, _} = foldl(fun(X, Prod) -> X * Prod end, 1, natural_numbers()),
  ?assertEqual(1, P).

map_test() ->
  [X | _] = map(fun(X) -> X * 2 end, natural_numbers()),
  ?assertEqual(2, X).

first_natural_numbers_test() ->
  ?assertEqual([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], first_natural_numbers(10)).

first_even_numbers_test() ->
  ?assertEqual([2, 4, 6, 8, 10, 12, 14, 16, 18, 20], first_even_numbers(10)).

first_squares_test() ->
  ?assertEqual([1, 4, 9, 16, 25, 36, 49, 64, 81, 100], first_squares(10)).

first_sum_test() ->
  ?assertEqual(55, first_sum(10)).

-endif.
