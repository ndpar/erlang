%%
%% LazyList = [Elem1, ListGenerator]
%% ListGenerator = fun(Elem2) -> LazyList
%% Elem1 = Elem2 = term()
%%
%% See http://www.erlang.org/cgi-bin/ezmlm-cgi/4/177
%%
-module(lazy1).
-export([filter/2, foldl/3, map/2]).
-export([first/2, filter_first/3, foldl_first/4, map_first/3]).
-export([natural_numbers/0]).

%
% Lazy list samples
%

integers_from(K) -> [K | fun() -> integers_from(K + 1) end].
natural_numbers() -> integers_from(1).

%
% Fundamental operations on lazy lists
%

filter(_, []) -> [];
filter(Pred, [X|Gen]) ->
    case Pred(X) of
        true -> [X | fun() -> filter(Pred, Gen()) end];
        false -> filter(Pred, Gen())
    end.

foldl(_, Acc, []) -> {Acc, []};
foldl(Fun, Acc, [X|Gen]) -> {Fun(X, Acc), Gen()}.

map(_, []) -> [];
map(Fun, [X|Gen]) -> [Fun(X) | fun() -> map(Fun, Gen()) end].

%
% Operations on first elements of lazy lists
%

first(N, LazyList) -> take([], N, LazyList).

take(Acc, 0, _) -> Acc;
take(Acc, N, [X|Gen]) -> take(Acc ++ [X], N - 1, Gen()).

filter_first(N, Pred, LazyList) -> first(N, filter(Pred, LazyList)).

foldl_first(0, _, Acc, _) -> Acc;
foldl_first(N, Fun, Acc, LazyList) ->
    {NewAcc, LazyTail} = foldl(Fun, Acc, LazyList),
    foldl_first(N-1, Fun, NewAcc, LazyTail).

map_first(N, Fun, LazyList) -> first(N, map(Fun, LazyList)).

%
% Operations on natural numbers
%

first_natural_numbers(N) -> first(N, natural_numbers()).

first_even_numbers(N) -> filter_first(N, fun(X) -> X rem 2 == 0 end, natural_numbers()).

first_squares(N) -> map_first(N, fun(X) -> X * X end, natural_numbers()).

first_sum(N) -> foldl_first(N, fun(X,Sum) -> X + Sum end, 0, natural_numbers()).

%
% Unit Tests
%

-include_lib("eunit/include/eunit.hrl").

filter_test() ->
    [X|_] = filter(fun(X) -> 10 < X end, natural_numbers()),
    ?assertEqual(11, X).

foldl_test() ->
    {P,_} = foldl(fun(X,Prod) -> X * Prod end, 1, natural_numbers()),
    ?assertEqual(1, P).

map_test() ->
    [X|_] = map(fun(X) -> X*2 end, natural_numbers()),
    ?assertEqual(2, X).

first_natural_numbers_test() ->
    ?assertEqual([1,2,3,4,5,6,7,8,9,10], first_natural_numbers(10)).

first_even_numbers_test() ->
    ?assertEqual([2,4,6,8,10,12,14,16,18,20], first_even_numbers(10)).

first_squares_test() ->
    ?assertEqual([1,4,9,16,25,36,49,64,81,100], first_squares(10)).

first_sum_test() ->
    ?assertEqual(55, first_sum(10)).
