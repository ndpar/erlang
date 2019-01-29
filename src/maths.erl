%%
%% @doc Math functions missing in the standard math module.
%%
-module(maths).
-author("Andrey Paramonov").

-export([log/2]).
-export([egcd/2, gcd/2, lcm/2, ilog2/1, isqrt/1]).
-export([crt_garner/2, crt_solver/2]).
-export([mod/2, mod_exp/3, mod_inv/2, mod_linear_equation_solver/3]).
-export([dot_product/2, hadamard_prod/2, pairwise_primes/1, prod/1]).
-export([pow/2]).
-export([factor2/1, jacobi/2, pollard_rho/1]).

%%
%% @doc Log base B of X.
%%
-spec log(B :: number(), X :: number()) -> float().

log(B, X) -> math:log(X) / math:log(B).


%%
%% @doc Hadamard product (a.k.a. Schur product) of two given vectors.
%%
-spec hadamard_prod([number()], [number()]) -> [number()].

hadamard_prod(A, B) -> lists:zipwith(fun erlang:'*'/2, A, B).

%%
%% @doc Dot product (a.k.a. scalar product) of two given vectors.
%%
-spec dot_product([number()], [number()]) -> number().

dot_product(A, B) -> lists:sum(hadamard_prod(A, B)).

%%
%% @doc Extended Euclidean Algorithm to compute GCD.
%% This identity holds true: GCD(A, B) = A * U + B * V.
%%
-spec egcd(A :: pos_integer(), B :: pos_integer()) ->
  {GCD :: pos_integer(), U :: integer(), V :: integer()}.

egcd(A, B) when 0 < A, 0 < B -> egcd(A, B, 1, 0, 0, 1).

egcd(0, D, _, _, Ud, Vd) -> {D, Ud, Vd};
egcd(C, D, Uc, Vc, Ud, Vd) ->
  Q = D div C,
  egcd(D - Q * C, C, Ud - Q * Uc, Vd - Q * Vc, Uc, Vc).

%%
%% @doc Returns the GCD of two positive integers.
%%
-spec gcd(pos_integer(), pos_integer()) -> pos_integer().

gcd(A, B) when A < 0 -> gcd(-A, B);
gcd(A, B) when B < 0 -> gcd(A, -B);
gcd(A, B) -> {GCD, _, _} = egcd(A, B), GCD.

%%
%% @doc Returns the least common multiple (LCM) of two positive integers.
%%
-spec lcm(pos_integer(), pos_integer()) -> pos_integer().

lcm(A, B) -> A * B div gcd(A, B).

%%
%% @doc Floor of the logarithm base 2 of the given integer N.
%%
-spec ilog2(pos_integer()) -> pos_integer().

ilog2(N) when 0 < N ->
  B = bin:integer_to_bitstring(N),
  bit_size(B) - 1.

%%
%% @doc Integer square root.
%% Implemented using bitwise algorithm
%% (https://en.wikipedia.org/wiki/Integer_square_root#Using_bitwise_operations)
%%
-spec isqrt(non_neg_integer()) -> non_neg_integer().

isqrt(N) when 0 =< N -> isqrt_shift(N, 2, N bsr 2).

isqrt_shift(N, Shift, 0) -> isqrt_root(N, Shift - 2, 0);
isqrt_shift(N, Shift, N) -> isqrt_root(N, Shift - 2, 0);
isqrt_shift(N, Shift, _) ->
  S = Shift + 2,
  isqrt_shift(N, S, N bsr S).

isqrt_root(_, Shift, Root) when Shift < 0 -> Root;
isqrt_root(N, Shift, Root) ->
  R = Root bsl 1,
  Candidate = R + 1,
  if
    Candidate * Candidate =< N bsr Shift -> isqrt_root(N, Shift - 2, Candidate);
    true -> isqrt_root(N, Shift - 2, R)
  end.


%%
%% @doc Garner's formula to solve particular case of CRT:
%% `x = a (mod p), x = b (mod q)' where `p' and `q' are primes.
%%
%% It gives the same result as `crt_solver([A, B], [P, Q]).'
%%
-spec crt_garner({non_neg_integer(), pos_integer()}, {non_neg_integer(), pos_integer()}) -> pos_integer().

crt_garner({A, P}, {B, Q}) ->
  mod((A - B) * mod_inv(Q, P), P) * Q + B.

%%
%% @doc Chinese Remainder Theorem solver.
%% For given vectors A and N, solves the equation `x = A (mod N)'
%% or returns `error' if the solution does not exist.
%%
%% The solution exists when N is a vector of relatively prime numbers.
%%
%% See CLRS, Theorem 31.27.
%%
-spec crt_solver([non_neg_integer()], [pos_integer()]) -> pos_integer().

crt_solver(A, N) when length(A) =:= length(N) ->
  try core:zipfold(crtFun(prod(N)), 0, A, N) of
    Fold -> Fold
  catch
    error:badarith -> error
  end.

crtFun(Prod) ->
  fun(Acc, A, N) ->
    M = Prod div N,
    MInv = maths:mod_inv(M, N),
    mod(Acc + A * M * MInv, Prod)
  end.

%%
%% @doc Modulo operation that works properly on negative integers.
%%
-spec mod(integer(), pos_integer()) -> non_neg_integer().

mod(A, M) -> (A rem M + M) rem M.

%%
%% @doc Fast modular exponentiation by repeated squaring.
%%
%% CLRS, chapter 31.6.
%%
%% If Base, Exp, and Mod are b-bit numbers, then the total
%% number of arithmetic operations required is O(b) and
%% the total number of bit operations required is O(b^3).
%%
%% @see crypto:mod_pow/3
%% @see crypto:bytes_to_integer/1
%%
-spec mod_exp(Base :: non_neg_integer(), Exp :: non_neg_integer(), Mod :: pos_integer()) -> non_neg_integer().

mod_exp(Base, Exp, Mod) ->
  mod_exp(Base, bin:integer_to_bitstring(Exp), 0, 1, Mod).

mod_exp(_A, <<>>, _C, D, _N) -> D;
mod_exp(A, <<0:1, B/bitstring>>, C, D, N) ->
  mod_exp(A, B, 2 * C, D * D rem N, N);
mod_exp(A, <<1:1, B/bitstring>>, C, D, N) ->
  mod_exp(A, B, 2 * C + 1, D * D * A rem N, N).


%%
%% @doc Inverse of B modulo N.
%%
-spec mod_inv(B :: pos_integer(), N :: pos_integer()) -> pos_integer().

mod_inv(B, N) when 0 < B, 0 < N ->
  case mod_linear_equation_solver(B, 1, N) of
    error -> error;
    [A] -> A
  end.

%%
%% @doc Solves equation ax = b (mod n) or returns `error'
%% if the solution does not exist.
%%
%% CLRS, chapter 31.4.
%%
-spec mod_linear_equation_solver(integer(), integer(), pos_integer()) -> [non_neg_integer()] | error.

mod_linear_equation_solver(A, B, N) ->
  {D, U, _} = egcd(A, N),
  mod_linear_equation_solver(B, N, D, U).

mod_linear_equation_solver(B, _, D, _) when B rem D =/= 0 -> error;
mod_linear_equation_solver(B, N, D, U) ->
  mod_linear_equation_solver_list(N, D, mod(U * B div D, N)).

mod_linear_equation_solver_list(N, D, X0) ->
  [mod(X0 + I * N div D, N) || I <- lists:seq(0, D - 1)].

%%
%% @doc Returns `true' if the numbers in the given list
%% are pairwise relatively prime, otherwise returns `false'.
%%
-spec pairwise_primes([pos_integer()]) -> boolean().

pairwise_primes(List) ->
  Ps = [1 < X andalso 1 < Y andalso gcd(X, Y) =:= 1 || X <- List, Y <- List, X < Y],
  lists:all(fun(Bool) -> Bool end, Ps).

%%
%% @doc Integer power of another integer
%%
-spec pow(N :: integer(), Exp :: non_neg_integer()) -> integer().

pow(_, 0) -> 1;
pow(N, E) when E rem 2 =:= 0 ->
  M = pow(N, E div 2),
  M * M;
pow(N, E) when 0 < E -> N * pow(N, E - 1).

%%
%% @doc Returns the product of the numbers in the given list.
%%
-spec prod([number()]) -> number().

prod(Numbers) -> lists:foldl(fun erlang:'*'/2, 1, Numbers).

%%
%% @doc Compute {s, t} such that s is odd and n = s * 2^t.
%%
-spec factor2(pos_integer()) -> {pos_integer(), non_neg_integer()}.

factor2(N) -> factor2(N, 0).

factor2(S, T) when S rem 2 =/= 0 -> {S, T};
factor2(S, T) -> factor2(S div 2, T + 1).

%%
%% @doc Compute Jacobi symbol (and Legendre symbol).
%%
%% See https://en.wikipedia.org/wiki/Jacobi_symbol
%%
%% @reference A.J.Menezes, P.C.van Oorschot, S.A.Vanstone.
%% <em>Handbook of Applied Cryptography</em>. Chapter 2.4.5. Algorithm 2.149
%%
%% @reference CLRS, Problem 31-4.b
%%
jacobi(A, N) when 2 < N, N rem 2 =/= 0 -> jacobi(A, N, 1).

jacobi(0, _, _) -> 0;
jacobi(A, N, Acc) ->
  {A1, E} = factor2(A),
  S1 = if
         E rem 2 =:= 0 -> 1;
         N rem 8 =:= 1 orelse N rem 8 =:= 7 -> 1;
         N rem 8 =:= 3 orelse N rem 8 =:= 5 -> -1
       end,
  S = if
        N rem 4 =:= 3 andalso A1 rem 4 =:= 3 -> -S1;
        true -> S1
      end,
  if
    A1 =:= 1 -> S * Acc;
    true -> jacobi(N rem A1, A1, S * Acc)
  end.

%%
%% @doc Pollard's rho heuristic.
%%
%% @reference CLRS, Chapter 31.9, p.976
%%
-spec pollard_rho(pos_integer()) -> pos_integer().

pollard_rho(N) ->
  X = rand:uniform(N - 1),
  pollard_rho(N, X, X, 1, 2).

pollard_rho(N, X, _, J, J) ->
  pollard_rho(N, X, X, J, 2 * J);
pollard_rho(N, X, Y, I, K) ->
  Xi = mod(X * X - 1, N),
  case gcd(Y - Xi, N) of
    1 -> pollard_rho(N, Xi, Y, I + 1, K);
    N -> pollard_rho(N, Xi, Y, I + 1, K);
    D -> D
  end.


%% =============================================================================
%% Unit tests
%% =============================================================================

-include_lib("eunit/include/eunit.hrl").

crt_garner_test() ->
  ?_assertEqual(crt_solver([4, 5], [7, 11]), crt_garner({4, 7}, {5, 11})).

crt_solver_test_() -> [
  ?_assertEqual(21, crt_solver([21], [23])),
  ?_assertEqual(49, crt_solver([4, 5], [5, 11])),
  ?_assertEqual(10, crt_solver([1, 2, 3], [9, 8, 7])),
  ?_assertEqual(8458, crt_solver([3, 5], [89, 107])),
  ?_assertEqual(error, crt_solver([1, 3], [4, 6])) % it can be 9 or 21
].

dot_product_test() ->
  ?assertEqual(3, dot_product([1, 3, -5], [4, -2, -1])).

egcd_test() ->
  ?assertEqual({263, 168, -131}, egcd(91261, 117035)).

lcm_test_() -> [
  ?_assertEqual(4100, lcm(82, 100)),
  ?_assertEqual(120, lcm(8, 30))].

ilog2_test_() -> [
  ?_assertEqual(0, ilog2(1)),
  ?_assertEqual(1, ilog2(3)),
  ?_assertEqual(2, ilog2(4)),
  ?_assertEqual(2, ilog2(5)),
  ?_assertEqual(20, ilog2(1048576))].

isqrt_test_() -> [
  ?_assertEqual(0, isqrt(0)),
  ?_assertEqual(1, isqrt(1)),
  ?_assertEqual(1, isqrt(2)),
  ?_assertEqual(1, isqrt(3)),
  ?_assertEqual(2, isqrt(4)),
  ?_assertEqual(2, isqrt(5)),
  ?_assertEqual(10, isqrt(100)),
  ?_assertEqual(79, isqrt(6241)),
  ?_assertEqual(79, isqrt(6250)),
  ?_assertEqual(111111110651, isqrt(12345678910111213141516))].

mod_linear_equation_solver_test_() -> [
  ?_assertEqual(error, mod_linear_equation_solver(2, 3, 4)),
  ?_assertEqual([6, 16, 26, 36, 46], mod_linear_equation_solver(35, 10, 50)),
  ?_assertEqual([95, 45], mod_linear_equation_solver(14, 30, 100))].

mod_test_() -> [
  ?_assertEqual(1, mod(15, 7)),
  ?_assertEqual(2, mod(-5, 7)),
  ?_assertEqual(0, mod(0, 17)),
  ?_assertEqual(-5, -5 rem 7)].

mod_exp_test_() -> [
  ?_assertEqual(1, mod_exp(0, 0, 5)),
  ?_assertEqual(1, mod_exp(1, 1, 5)),
  ?_assertEqual(1, mod_exp(7, 560, 561)),
  ?_assertEqual(16, mod_exp(12, 34, 56)),
  ?_assertEqual(199, mod_exp(27, 35, 569)),
  ?_assertEqual(81, mod_exp(12345, 67890, 103)),
  ?_assertEqual(81, crypto:bytes_to_integer(crypto:mod_pow(12345, 67890, 103)))].

mod_inv_test_() -> [
  ?_assertEqual(error, mod_inv(3, 60)),
  ?_assertEqual(1367, mod_inv(3, 4100)),
  ?_assertEqual(27, crypto:bytes_to_integer(crypto:mod_pow(3, -1, 60))), %% sic!
  ?_assertEqual(43, mod_inv(7, 60)),
  ?_assertEqual(43, crypto:bytes_to_integer(crypto:mod_pow(7, -1, 60))),
  ?_assertEqual(79, mod_inv(74, 167))].

pow_test() ->
  ?assertEqual(1048576, pow(2, 20)).

factor2_test_() -> [
  ?_assertEqual({3, 4}, factor2(48)),
  ?_assertEqual({79, 1}, factor2(158))].

jacobi_test_() ->
  Z21 = [1, 2, 4, 5, 8, 10, 11, 13, 16, 17, 19, 20], [
    ?_assertEqual(-1, jacobi(158, 235)),
    ?_assertEqual([1, -1, 1, -1, -1, 1, -1, 1, 1, -1, 1, -1], [jacobi(A, 3) || A <- Z21]),
    ?_assertEqual([1, 1, 1, -1, 1, -1, 1, -1, 1, -1, -1, -1], [jacobi(A, 7) || A <- Z21]),
    ?_assertEqual([1, -1, 1, 1, -1, -1, -1, -1, 1, 1, -1, 1], [jacobi(A, 21) || A <- Z21]),
    ?_assertEqual([1, 1, 0, 1, 1, 0, 1, 1], [jacobi(A, 9) || A <- lists:seq(1, 8)]),
    ?_assertEqual([1, 1, 0, 1, 0, 0, -1, 1, 0, 0, -1, 0, -1, -1], [jacobi(A, 15) || A <- lists:seq(1, 14)])].

pollard_rho_test_() -> [
  ?_assert(lists:member(pollard_rho(1387), [19, 73])),
  ?_assert(lists:member(pollard_rho(455459), [613, 743]))].
