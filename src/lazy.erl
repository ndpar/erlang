%
% See http://www.erlang.org/cgi-bin/ezmlm-cgi/4/177
%
% 1> lazy:first_natural_numbers(10).
% [1,2,3,4,5,6,7,8,9,10]
% 2> lazy:first_even_numbers(10).
% [2,4,6,8,10,12,14,16,18,20]
% 3> lazy:first_squares(10).
% [1,4,9,16,25,36,49,64,81,100]
% 4> lazy:first_sum(10).
% 55
%
-module(lazy).
-export([first_natural_numbers/1, first_even_numbers/1, first_squares/1, first_sum/1]).

integers(K) -> [K | fun() -> integers(K + 1) end].
natural_numbers() -> integers(1).

first(N, LazyList) -> take([], N, LazyList).

take(Acc, 0, _) -> Acc;
take(Acc, N, [X | Fun]) -> take(Acc ++ [X], N-1, Fun()).

first_natural_numbers(N) -> first(N, natural_numbers()).

filter(_, []) -> [];
filter(Pred, [X | Fun]) ->
    case Pred(X) of
        true -> [X | fun() -> filter(Pred, Fun()) end];
        false -> filter(Pred, Fun())
    end.

filter_first(N, Pred, LazyList) -> first(N, filter(Pred, LazyList)).

first_even_numbers(N) -> filter_first(N, fun(X) -> X rem 2 == 0 end, natural_numbers()).

map(_, []) -> [];
map(MapFun, [X | ListFun]) -> [MapFun(X) | fun() -> map(MapFun, ListFun()) end].

map_first(N, Fun, LazyList) -> first(N, map(Fun, LazyList)).

first_squares(N) -> map_first(N, fun(X) -> X*X end, natural_numbers()).

%foldl(_, Acc, []) -> Acc;
%foldl(Fun, Acc, [X | ListFun]) -> foldl(Fun, Fun(X, Acc), ListFun()).

foldl_first(0, _, Acc, _) -> Acc;
foldl_first(N, Fun, Acc, [X | ListFun]) -> foldl_first(N-1, Fun, Fun(X, Acc), ListFun()).

first_sum(N) -> foldl_first(N, fun(X, Acc) -> X+Acc end, 0, natural_numbers()).
