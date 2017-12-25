%
% Galois Fields and
% Galois Counter Mode of Operation (GCM)
%
% http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.694.695&rep=rep1&type=pdf
%
-module(galois).
-export([mult/2, ghash/3]).
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


ghash(H, A, C) -> ghash(H, A, C, <<0:128>>, size(A), size(C)).

ghash(H, <<Ai:16/binary, A/binary>>, C, X, ALength, CLength) ->
  ghash(H, A, C, mult(exor(X, Ai), H), ALength, CLength);
ghash(H, <<A/binary>>, C, X, ALength, CLength) when size(A) > 0 ->
  S = 8 * (16 - size(A)),
  ghash(H, <<>>, C, mult(exor(X, <<A/binary, 0:S>>), H), ALength, CLength);
ghash(H, <<>>, <<Ci:16/binary, C/binary>>, X, ALength, CLength) ->
  ghash(H, <<>>, C, mult(exor(X, Ci), H), ALength, CLength);
ghash(H, <<>>, <<C/binary>>, X, ALength, CLength) when size(C) > 0 ->
  S = 8 * (16 - size(C)),
  ghash(H, <<>>, <<>>, mult(exor(X, <<C/binary, 0:S>>), H), ALength, CLength);
ghash(H, <<>>, <<>>, X, ALength, CLength) ->
  mult(exor(X, <<(8 * ALength):64, (8 * CLength):64>>), H).


-include_lib("eunit/include/eunit.hrl").

mult_test() ->
  ?assertEqual(<<16#A3B928F1CECEBA0F612BFEEBE5AEA0E1:128>>,
    mult(<<16#b96baa8c1c75a671bfb2d08d06be5f36:128>>, <<16#1e0889016f67601c8ebea4943bc23ad6:128>>)).

%
% Test vectors from
% http://www.ieee802.org/1/files/public/docs2011/bn-randall-test-vectors-0511-v1.pdf
%
ghash_1_test() ->
  ?assertEqual(<<16#A4C350FB66B8C960E83363381BA90F50:128>>,
    ghash(<<16#73A23D80121DE2D5A850253FCF43120E:128>>,
      <<16#D609B1F056637A0D46DF998D88E52E00:128,
        16#B2C2846512153524C0895E81:96>>,
      <<16#701AFA1CC039C0D765128A665DAB6924:128,
        16#3899BF7318CCDC81C9931DA17FBE8EDD:128,
        16#7D17CB8B4C26FC81E3284F2B7FBA713D:128>>)).

ghash_2_test() ->
  ?assertEqual(<<16#F02428563BB7E67C378044C874498FF8:128>>,
    ghash(<<16#E4E01725D724C1215C7309AD34539257:128>>,
      <<16#E20106D7CD0DF0761E8DCD3D88E54000:128,
        16#76D457ED08000F101112131415161718:128,
        16#191A1B1C1D1E1F202122232425262728:128,
        16#292A2B2C2D2E2F303132333435363738:128,
        16#393A0003:32>>,
      <<>>)).


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
