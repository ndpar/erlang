%%
%% Math functions missing in the standard math module.
%%
-module(maths).
-export([egcd/2, mod/2, modexp/3, modinv/2, pow/2, random/2]).

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
-spec modexp(Base :: pos_integer(), Exp :: non_neg_integer(), Mod :: pos_integer()) -> non_neg_integer().

modexp(_Base, 0, _Mod) -> 1;
modexp(Base, Exp, Mod) when Exp rem 2 =:= 0 ->
  square(modexp(Base, Exp div 2, Mod)) rem Mod;
modexp(Base, Exp, Mod) ->
  Base * modexp(Base, Exp - 1, Mod) rem Mod.

square(A) -> A * A.

%%
%% @doc Inverse of B modulo prime P.
%%
-spec modinv(B :: pos_integer(), P :: pos_integer()) -> pos_integer().

modinv(B, P) when 0 < B, 0 < P ->
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

modexp_test() ->
  ?assertEqual(16, modexp(12, 34, 56)).

modinv_test() ->
  ?assertEqual(79, modinv(74, 167)).

pow_test() ->
  ?assertEqual(1048576, pow(2, 20)).
