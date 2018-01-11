-module(maths).
-export([egcd/2, inv_mod/2, pow/2, primes_upto/1]).

%%
%% Extended Euclidean Algorithm to compute GCD.
%%
-spec egcd(pos_integer(), pos_integer()) -> {pos_integer(), pos_integer(), pos_integer()}.

egcd(A, B) when 0 < A, 0 < B -> egcd(A, B, 1, 0, 0, 1).

egcd(0, D, _, _, Ud, Vd) -> {D, Ud, Vd};
egcd(C, D, Uc, Vc, Ud, Vd) ->
  Q = D div C,
  egcd(D - Q * C, C, Ud - Q * Uc, Vd - Q * Vc, Uc, Vc).

%%
%% Inverse of B modulo prime P.
%%
-spec inv_mod(B :: pos_integer(), P :: pos_integer()) -> pos_integer().

inv_mod(B, P) when 0 < B, 0 < P ->
  {1, U, _} = egcd(B, P),
  U.

%%
%% Integer power of another integer
%%
-spec pow(N :: integer(), E :: non_neg_integer()) -> integer().

pow(_, 0) -> 1;
pow(N, E) -> N * pow(N, E - 1).

%%
%% Find all prime numbers up to specified value.
%% Works relatively fast for N < 5,000,000.
%%
-spec primes_upto(N :: 2..5000000) -> [integer()].

primes_upto(N) when 2 =< N, N =< 5000000 -> eratosthenes(math:sqrt(N), lists:seq(2, N)).

%%
%% Recursion implementation of Eratosthenes sieve algorithm
%% Author: Zac Brown
%%
%% See also: https://github.com/ndpar/algorithms/blob/master/mymath.erl
%%
eratosthenes(Max, [H | T]) when H =< Max -> [H | eratosthenes(Max, sieve([H | T], H))];
eratosthenes(_Max, L) -> L.

sieve([H | T], N) when H rem N =/= 0 -> [H | sieve(T, N)];
sieve([_ | T], N) -> sieve(T, N);
sieve([], _N) -> [].

%%
%% Unit tests
%%
-include_lib("eunit/include/eunit.hrl").

egcd_test() ->
  ?assertEqual({263, 168, -131}, egcd(91261, 117035)).

inv_mod_test() ->
  ?assertEqual(79, inv_mod(74, 167)).

pow_test() ->
  ?assertEqual(1048576, pow(2, 20)).
