%%
%% http://groups.google.com/group/erlang-programming/browse_thread/thread/6541d9a47d30b0e0#
%%
-module(lazy).
-export([filter/2, foldl/3, map/2]).
-export([first/2, filter_first/3, foldl_first/4, map_first/3]).
-export([natural_numbers/0]).

%
% Lazy list samples
%

integers_from(K) -> fun() -> [K | integers_from(K + 1)] end.
natural_numbers() -> integers_from(1).

%
% Fundamental operations on lazy lists
%

filter(P, CE) ->
    fun() ->
        case CE() of
            []     -> [];
            [X|Xs] -> case P(X) of
                          true  -> [X | filter(P, Xs)];
                          false -> (filter(P, Xs))()
                      end
        end
    end.

foldl(F, A, CE) ->
    case CE() of
        []     -> A;
        [X|Xs] -> foldl(F, F(A,X), Xs)
    end.

map(F, CE) ->
    fun() ->
        case CE() of
            []     -> [];
            [X|Xs] -> [F(X) | map(F, Xs)]
        end
    end.

%
% Operations on first elements of lazy lists
%

first(N, LazyList) -> take([], N, LazyList).

take(A, 0, _) -> A;
take(A, N, CE) -> [X|Xs] = CE(), take(A ++ [X], N-1, Xs).

filter_first(N, P, CE) -> first(N, filter(P, CE)).

foldl_first(0, _, A, _) -> A;
foldl_first(N, F, A, CE) ->
    case CE() of
        []     -> A;
        [X|Xs] -> foldl_first(N-1, F, F(A,X), Xs)
    end.

map_first(N, F, CE) -> first(N, map(F, CE)).

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
    [X|_] = (filter(fun(X) -> 10 < X end, natural_numbers()))(),
    ?assertEqual(11, X).

map_test() ->
    [X|_] = (map(fun(X) -> X*2 end, natural_numbers()))(),
    ?assertEqual(2, X).

first_natural_numbers_test() ->
    ?assertEqual([1,2,3,4,5,6,7,8,9,10], first_natural_numbers(10)).

first_even_numbers_test() ->
    ?assertEqual([2,4,6,8,10,12,14,16,18,20], first_even_numbers(10)).

first_squares_test() ->
    ?assertEqual([1,4,9,16,25,36,49,64,81,100], first_squares(10)).

first_sum_test() ->
    ?assertEqual(55, first_sum(10)).
