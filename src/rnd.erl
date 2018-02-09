%%
%% @doc PRNG functions.
%%
-module(rnd).
-author("Andrey Paramonov").

-export([rand_bytes/1, rand_seed/1]).
-export([random/2]).

%%
%% @doc Generates N bytes randomly uniform 0..255,
%% and returns the result as an integer.
%%
%% Uses {@link rand:uniform/1} so that the result can
%% be reproduced after reseeding PRNG with the same value.
%%
%% If reproducibility is not a requirement, use standard
%% {@link crypto:strong_rand_bytes/1} instead.
%%
%% @see rand_seed/1
%%
-spec rand_bytes(N :: pos_integer()) -> pos_integer().

rand_bytes(N) ->
  Bytes = <<<<(rand:uniform(256) - 1):8>> || _ <- lists:seq(1, N)>>,
  binary:decode_unsigned(Bytes).

%%
%% @doc Seeds PRNG with `exsplus' algorithm and given seed and returns the state.
%%
%% @param Seed binary of size at least 30 bytes.
%%
%% @see rand:seed/2
%%
-spec rand_seed(Seed :: binary()) -> term().

rand_seed(Seed) when 30 =< size(Seed) ->
  L = bit_size(Seed),
  S = L div 24,
  R = L - 2 * S,
  <<A1:S, A2:S, A3:R>> = Seed,
  rand:seed(exsplus, {A1, A2, A3}).

%%
%% @doc Returns a random integer uniformly distributed in the interval [L, U].
%%
-spec random(L :: integer(), U :: integer()) -> integer().

random(L, U) -> L + rand:uniform(U - L + 1) - 1.
