%%
%% Math functions missing in the standard math module.
%%
-module(maths).
-export([egcd/2, mod/2, mod_exp/3, mod_inv/2, pow/2, random/2]).

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
%% @doc Fast modular exponentiation by repeated squaring.
%%
%% CLRS, chapter 31.6.
%%
%% If Base, Exp, and Mod are b-bit numbers, then the total
%% number of arithmetic operations required is O(b) and
%% the total number of bit operations required is O(b^3).
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
%% @doc Inverse of B modulo prime P.
%%
-spec mod_inv(B :: pos_integer(), P :: pos_integer()) -> pos_integer().

mod_inv(B, P) when 0 < B, 0 < P ->
  {1, U, _} = egcd(B, P),
  U.

%%
%% @doc mod that works properly on negative integers.
%%
-spec mod(integer(), pos_integer()) -> non_neg_integer().

mod(A, M) -> (A rem M + M) rem M.

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

mod_test() ->
  ?assertEqual(1, mod(15, 7)),
  ?assertEqual(2, mod(-5, 7)),
  ?assertEqual(0, mod(0, 17)),
  ?assertEqual(-5, -5 rem 7).

mod_exp_test() ->
  ?assertEqual(1, mod_exp(0, 0, 5)),
  ?assertEqual(1, mod_exp(1, 1, 5)),
  ?assertEqual(1, mod_exp(7, 560, 561)),
  ?assertEqual(16, mod_exp(12, 34, 56)),
  ?assertEqual(199, mod_exp(27, 35, 569)),
  ?assertEqual(81, mod_exp(12345, 67890, 103)).

mod_inv_test() ->
  ?assertEqual(79, mod_inv(74, 167)).

pow_test() ->
  ?assertEqual(1048576, pow(2, 20)).
