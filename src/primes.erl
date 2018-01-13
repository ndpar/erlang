-module(primes).
-author("Andrey Paramonov").

-export([is_prime/1, primes_upto/1, random_prime/2]).

%%
%% @doc Returns true if the given N is a prime number.
%%
-spec is_prime(N :: pos_integer()) -> boolean().

is_prime(N) -> miller_rabin(N).

miller_rabin(2) -> true;
miller_rabin(N) when N rem 2 =:= 0 -> false;
miller_rabin(N) when 3 =< N, N rem 2 =:= 1 ->
  {S, T} = factor_out(N - 1, 0),
  miller_rabin(N, S, T, 0).

%%
%% Compute {s, t} such that s is odd and s * 2^t = n - 1.
%%
factor_out(S, T) when S rem 2 =/= 0 -> {S, T};
factor_out(S, T) -> factor_out(S div 2, T + 1).

%%
%% Keep track of the probability of a false result in K.
%% The probability is at most 2^-K.
%% Loop until the probability of a false result is small enough.
%%
miller_rabin(N, S, T, K) when K < 128 ->
  A = maths:random(2, N - 1),
  case maths:modexp(A, S, N) of
    1 -> miller_rabin(N, S, T, K + 2);
    V -> case mr_squaring(0, V, N, T) of
           composite -> false;
           candidate -> miller_rabin(N, S, T, K + 2)
         end
  end;
miller_rabin(_, _, _, _) -> true.

%%
%% The sequence v, v^2,..., v^2^t must finish on the value 1,
%% and the last value not equal to 1 must be n-1 if n is a prime.
%%
mr_squaring(_I, V, N, _T) when V =:= N - 1 -> candidate;
mr_squaring(I, _V, _N, T) when I =:= T - 1 -> composite;
mr_squaring(I, V, N, T) -> mr_squaring(I + 1, maths:modexp(V, 2, N), N, T).


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
%% @doc Returns a random prime in the interval [L, U].
%%
-spec random_prime(L :: pos_integer(), U :: pos_integer()) -> pos_integer().

random_prime(L, U) when 2 < L, L =< U ->
  random_prime(L, U, 100 * (math:log2(U) + 1) - 1).

random_prime(L, U, R) when 0 < R ->
  N = maths:random(L, U),
  case is_prime(N) of
    true -> N;
    false -> random_prime(L, U, R - 1)
  end.


%% =============================================================================
%% Unit tests
%% =============================================================================

-include_lib("eunit/include/eunit.hrl").

is_prime_test() ->
  ?assert(is_prime(17)),
  ?assert(is_prime(283)).

composite_test() ->
  ?assertNot(is_prime(100)),
  ?assertNot(is_prime(105)).

primes_upto_test() ->
  ?assertEqual([2, 3, 5, 7, 11, 13], primes_upto(15)).

random_prime_test() ->
  ?assertEqual(103, random_prime(102, 105)).
