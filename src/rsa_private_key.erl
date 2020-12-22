%%
%% @doc Restoring RSA private key from its parts.
%%
%% RSA private key consists of four integer values {p, q, t, d}.
%% The knowledge of any one of these values is sufficient to
%% compute all the other three.
%%
%% Easy case: Attacker knows p (or q). Then q = n / p.
%% t = (p - 1)(q - 1) / gcd(p - 1, q - 1).
%% d = e^-1 (mod t).
%%
%% To play with the functions in this module, the following commands
%% may be useful if you want to generate the real private keys:
%% ```
%% $ openssl genrsa -out private.pem 2048
%% $ openssl asn1parse -i -in private.pem
%% '''
%% @reference [FSK1] Chapter 12.4.3. The Private Key.
%%
-module(rsa_private_key).
-author("Andrey Paramonov").

-export([factorize_from_d/3, factorize_from_t/2]).

%%
%% @doc If attacker knows d, she can find {p, q} using this method.
%%
-spec factorize_from_d(N :: pos_integer(), E :: pos_integer(), D :: pos_integer()) ->
  {P :: pos_integer(), Q :: pos_integer()} | error.

factorize_from_d(N, E, D) ->
  factorize_from_d(N, E, D, 1).

factorize_from_d(N, E, D, Factor) ->
  case factorize_from_t(N, (E * D - 1) div Factor) of
    error -> factorize_from_d(N, E, D, Factor + 1);
    Result -> Result
  end.

%%
%% @doc If attacker knows t, she can find {p, q} using this method.
%%
-spec factorize_from_t(N :: pos_integer(), T :: pos_integer()) ->
  {P :: pos_integer(), Q :: pos_integer()} | error.

factorize_from_t(N, T) ->
  case N div T of
    0 -> error;
    GCD -> factorize_from_t(N, T, GCD)
  end.

factorize_from_t(N, T, GCD) ->
  Phi = T * GCD,
  S = N - Phi + 1,
  factorize_from_s(N, S).

factorize_from_s(_N, S) when S rem 2 =:= 1 -> error;
factorize_from_s(N, S) ->
  S2 = S div 2,
  Desc = maths:isqrt(S2 * S2 - N),
  {P, Q} = {S2 - Desc, S2 + Desc},
  case P * Q of
    N -> {P, Q};
    _ -> error
  end.

%% =============================================================================
%% Unit tests
%% =============================================================================

-include_lib("eunit/include/eunit.hrl").

factorize_from_d_test() ->
  ?assertEqual({11, 13}, factorize_from_d(143, 7, 43)).

factorize_from_t_test_() -> [
  ?_assertEqual(error, factorize_from_t(143, 300)),
  ?_assertEqual(error, factorize_from_t(143, 150)),
  ?_assertEqual(error, factorize_from_t(143, 100)),
  ?_assertEqual(error, factorize_from_t(143, 75)),
  ?_assertEqual({11, 13}, factorize_from_t(143, 60))].
