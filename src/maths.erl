%%
%% Math functions missing in the standard math module.
%%
-module(maths).
-author("Andrey Paramonov").

-export([egcd/2, ilog2/1, isqrt/1]).
-export([mod/2, mod_exp/3, mod_inv/2, mod_linear_equation_solver/3]).
-export([pow/2, random/2]).

%%
%% @doc Extended Euclidean Algorithm to compute GCD.
%%
-spec egcd(pos_integer(), pos_integer()) -> {pos_integer(), pos_integer(), pos_integer()}.

egcd(A, B) when 0 < A, 0 < B -> egcd(A, B, 1, 0, 0, 1).

egcd(0, D, _, _, Ud, Vd) -> {D, Ud, Vd};
egcd(C, D, Uc, Vc, Ud, Vd) ->
  Q = D div C,
  egcd(D - Q * C, C, Ud - Q * Uc, Vd - Q * Vc, Uc, Vc).

%%
%% @doc Floor of the logarithm base 2 of the given integer N.
%%
-spec ilog2(N :: pos_integer()) -> pos_integer().

ilog2(N) when 0 < N ->
  B = bin:integer_to_bitstring(N),
  bit_size(B) - 1.

%%
%% @doc Integer square root.
%% https://en.wikipedia.org/wiki/Integer_square_root#Using_bitwise_operations
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
%% @doc mod that works properly on negative integers.
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
%% See also: crypto:mod_pow/3 and crypto:bytes_to_integer/1
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
%% @doc Solves equation Ax = B (mod N) or returns error
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
%% @doc Integer power of another integer
%%
-spec pow(N :: integer(), E :: non_neg_integer()) -> integer().

pow(_, 0) -> 1;
pow(N, E) -> N * pow(N, E - 1).

%%
%% @doc Returns a random integer uniformly distributed in the interval [L, U].
%%
-spec random(L :: integer(), U :: integer()) -> integer().

random(L, U) -> L + rand:uniform(U - L + 1) - 1.


%% =============================================================================
%% Unit tests
%% =============================================================================

-include_lib("eunit/include/eunit.hrl").

egcd_test() ->
  ?assertEqual({263, 168, -131}, egcd(91261, 117035)).

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
  ?_assertEqual(27, crypto:bytes_to_integer(crypto:mod_pow(3, -1, 60))), %% sic!
  ?_assertEqual(43, mod_inv(7, 60)),
  ?_assertEqual(43, crypto:bytes_to_integer(crypto:mod_pow(7, -1, 60))),
  ?_assertEqual(79, mod_inv(74, 167))].

pow_test() ->
  ?assertEqual(1048576, pow(2, 20)).
