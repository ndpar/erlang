%
% Galois Fields and
% Galois Counter Mode of Operation (GCM)
%
% http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.694.695&rep=rep1&type=pdf
%
-module(galois).
-export([mult/2]).
-import(crypto, [exor/2]).

-define(R, <<2#11100001:8, 0:120>>).

%
% Algorithm 1. Multiplication in GF(2^128).
%
mult(X, Y) -> mult(<<0:128>>, X, Y).

mult(Z, _, <<>>) -> Z;
mult(Z, V, <<0:1, _/bitstring>> = Y) -> mult_shift(Z, V, Y);
mult(Z, V, <<1:1, _/bitstring>> = Y) -> mult_shift(exor(Z, V), V, Y).

mult_shift(Z, <<V:127, 0:1>>, <<_:1, Y/bitstring>>) -> mult(Z, <<0:1, V:127>>, Y);
mult_shift(Z, <<V:127, 1:1>>, <<_:1, Y/bitstring>>) -> mult(Z, exor(<<0:1, V:127>>, ?R), Y).


-include_lib("eunit/include/eunit.hrl").

mult_test() ->
  ?assertEqual(<<16#A3B928F1CECEBA0F612BFEEBE5AEA0E1:128>>,
    mult(<<16#b96baa8c1c75a671bfb2d08d06be5f36:128>>, <<16#1e0889016f67601c8ebea4943bc23ad6:128>>)).

enc(K, P) -> crypto:block_encrypt(aes_ecb, K, P).

manual_gcm_one_block_test() ->
  K = <<16#92e11dcdaa866f5ce790fd24501f92509aacf4cb8b1339d50c9c1240935dd08b:256>>,
  A = <<16#1e0889016f67601c8ebea4943bc23ad6:128>>,
  P = <<16#2d71bcfa914e4ac045b2aa60955fad24:128>>,
  IV = <<16#ac93a1a6145299bde902f21a:96>>,
  H = enc(K, <<0:128>>),
  Y0 = <<IV/binary, 1:32>>,
  Y1 = <<IV/binary, 2:32>>,
  C1 = exor(P, enc(K, Y1)),
  H1 = exor(C1, mult(A, H)),
  H2 = exor(mult(H1, H), <<128:64, 128:64>>),
  T = exor(enc(K, Y0), mult(H2, H)),
  ?assertEqual({C1, T}, crypto:block_encrypt(aes_gcm, K, IV, {A, P, 16})).

aes_gcm_test() ->
  K = <<16#92e11dcdaa866f5ce790fd24501f92509aacf4cb8b1339d50c9c1240935dd08b:256>>,
  A = <<16#1e0889016f67601c8ebea4943bc23ad6:128>>,
  P = <<16#2d71bcfa914e4ac045b2aa60955fad24:128>>,
  IV = <<16#ac93a1a6145299bde902f21a:96>>,
  {CipherText, CipherTag} = crypto:block_encrypt(aes_gcm, K, IV, {A, P, 16}),
  ?assertEqual(<<16#8995AE2E6DF3DBF96FAC7B7137BAE67F:128>>, CipherText),
  ?assertEqual(<<16#ECA5AA77D51D4A0A14D9C51E1DA474AB:128>>, CipherTag).

exor_test() ->
  ?assertEqual(<<2#0100000000011000:16>>, exor(<<2#1110101010110010:16>>, <<2#1010101010101010:16>>)).
