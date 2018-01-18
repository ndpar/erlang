-module(rsa_private_key).
-author("Andrey Paramonov").

-export([factorize_from_d/3, factorize_from_t/2]).

%% =============================================================================
%% Restoring full private key from partially known private key.
%% =============================================================================
%%
%% RSA public key consists of two integers {n, e}.
%% RSA private key consists of four integers {p, q, t, d}.
%% If an attacker knows one of the private integers,
%% she can restore the other three.
%%
%% Simplest case: Attacker knows p. Then q = n / p.
%% t = (p - 1)(q - 1) / gcd(p - 1, q - 1).
%% d = e^-1 (mod t).
%%
%% Similar case when attacker knows q.
%%
%% To play with the functions, use the following commands to generate keys
%% $ openssl genrsa -out private.pem 2048
%% $ openssl asn1parse -i -in private.pem
%%

%%
%% If attacker knows d, she can find {p, q}
%% using this method.
%%
-spec factorize_from_d(N :: pos_integer(), E :: pos_integer(), D :: pos_integer()) ->
  {pos_integer(), pos_integer()} | error.

factorize_from_d(N, E, D) ->
  factorize_from_d(N, E, D, 1).

factorize_from_d(N, E, D, Factor) ->
  case factorize_from_t(N, (E * D - 1) div Factor) of
    error -> factorize_from_d(N, E, D, Factor + 1);
    Result -> Result
  end.

%%
%% If attacker knows t, she can find {p, q}
%% using this method.
%%
-spec factorize_from_t(N :: pos_integer(), T :: pos_integer()) ->
  {pos_integer(), pos_integer()} | error.

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
