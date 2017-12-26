%
% Galois Fields and
% Galois Counter Mode of Operation (GCM)
%
% http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.694.695&rep=rep1&type=pdf
%
-module(galois).
-export([gcm/5, ghash/3, gmac/3, mult/2, lxor/2]).
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

%
% GHASH function as defined by (2).
%
ghash(H, A, C) ->
  AL = bit_size(A),
  CL = bit_size(C),
  Data = <<A/binary, 0:(mod(-AL, 128)), C/binary, 0:(mod(-CL, 128)), AL:64, CL:64>>,
  Blocks = [<<X:128>> || <<X:128>> <= Data],
  lists:foldl(fun(B, X) -> mult(exor(X, B), H) end, <<0:128>>, Blocks).

%
% AES-256-GCM implementation as specified by (1).
%
gcm(K, IV, A, P, TLength) ->
  EK = fun(X) -> enc(K, X) end,
  H = EK(<<0:128>>),
  Y0 = init_counter(IV, H),
  {_, CipherText} = lists:foldl(
    fun(B, {Y, <<C/binary>>}) ->
      Yi = incr(Y),
      {Yi, <<C/binary, (lxor(B, EK(Yi)))/binary>>}
    end,
    {Y0, <<>>},
    [<<X:128>> || <<X:128>> <= P]),
  CipherTag = lxor(ghash(H, A, CipherText), EK(Y0)),
  {CipherText, <<CipherTag:TLength/binary>>}.

%
% GMAC
%
gmac(K, Nonce, A) ->
  {<<>>, Tag} = gcm(K, Nonce, A, <<>>, 16),
  Tag.

%
% Utility functions
%

% mod that works properly on negative integers
mod(X, Y) -> (X rem Y + Y) rem Y.

init_counter(IV, _) when bit_size(IV) =:= 96 -> <<IV/binary, 1:32>>;
init_counter(IV, H) -> ghash(H, <<>>, IV).

incr(<<Nonce:12/binary, Counter:32>>) -> <<Nonce/binary, (Counter + 1):32>>.

% Left XOR
lxor(<<X/binary>>, <<Y/binary>>) ->
  Length = min(size(X), size(Y)),
  crypto:exor(binary:part(X, 0, Length), binary:part(Y, 0, Length)).

% Single block AES encryption for key K
enc(K, P) -> crypto:block_encrypt(aes_ecb, K, P).


-include_lib("eunit/include/eunit.hrl").

my_aes_gcm_is_equivalent_to_erlang_aes_gcm_test() ->
  K = <<16#92e11dcdaa866f5ce790fd24501f92509aacf4cb8b1339d50c9c1240935dd08b:256>>,
  A = <<16#1e0889016f67601c8ebea4943bc23ad6:128>>,
  P = <<16#2d71bcfa914e4ac045b2aa60955fad24:128>>,
  IV = <<16#ac93a1a6145299bde902f21a:96>>,
  ?assertEqual(crypto:block_encrypt(aes_gcm, K, IV, {A, P, 16}), gcm(K, IV, A, P, 16)).

gmac_test() ->
  K = <<16#8000000000000000000000000000000000000000000000000000000000000001:256>>,
  A = <<16#4d4143732061726520766572792075736566756c20696e2063727970746f67726170687921:296>>,
  Nonce = <<16#000000000000000000000001:96>>,
  ?assertEqual(<<16#34B025A57D99315120912DEFBFE329C3:128>>, gmac(K, Nonce, A)).

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

%
% Test vectors from
% http://csrc.nist.gov/groups/STM/cavp/documents/mac/gcmtestvectors.zip
%
nist_256_96_0_0_128_test() ->
  K = <<16#b52c505a37d78eda5dd34f20c22540ea1b58963cf8e5bf8ffa85f9f2492505b4:256>>,
  A = <<>>,
  P = <<>>,
  IV = <<16#516c33929df5a3284ff463d7:96>>,
  {CipherText, CipherTag} = gcm(K, IV, A, P, 16),
  ?assertEqual(<<>>, CipherText),
  ?assertEqual(<<16#BDC1AC884D332457A1D2664F168C76F0:128>>, CipherTag).

nist_256_96_0_128_128_test() ->
  K = <<16#78dc4e0aaf52d935c3c01eea57428f00ca1fd475f5da86a49c8dd73d68c8e223:256>>,
  A = <<16#b96baa8c1c75a671bfb2d08d06be5f36:128>>,
  P = <<>>,
  IV = <<16#d79cf22d504cc793c3fb6c8a:96>>,
  {CipherText, CipherTag} = gcm(K, IV, A, P, 16),
  ?assertEqual(<<>>, CipherText),
  ?assertEqual(<<16#3E5D486AA2E30B22E040B85723A06E76:128>>, CipherTag).

nist_256_96_128_0_128_test() ->
  K = <<16#31bdadd96698c204aa9ce1448ea94ae1fb4a9a0b3c9d773b51bb1822666b8f22:256>>,
  A = <<>>,
  P = <<16#2db5168e932556f8089a0622981d017d:128>>,
  IV = <<16#0d18e06c7c725ac9e362e1ce:96>>,
  {CipherText, CipherTag} = gcm(K, IV, A, P, 16),
  ?assertEqual(<<16#FA4362189661D163FCD6A56D8BF0405A:128>>, CipherText),
  ?assertEqual(<<16#D636AC1BBEDD5CC3EE727DC2AB4A9489:128>>, CipherTag).

nist_256_96_128_128_128_test() ->
  K = <<16#92e11dcdaa866f5ce790fd24501f92509aacf4cb8b1339d50c9c1240935dd08b:256>>,
  A = <<16#1e0889016f67601c8ebea4943bc23ad6:128>>,
  P = <<16#2d71bcfa914e4ac045b2aa60955fad24:128>>,
  IV = <<16#ac93a1a6145299bde902f21a:96>>,
  {CipherText, CipherTag} = gcm(K, IV, A, P, 16),
  ?assertEqual(<<16#8995AE2E6DF3DBF96FAC7B7137BAE67F:128>>, CipherText),
  ?assertEqual(<<16#ECA5AA77D51D4A0A14D9C51E1DA474AB:128>>, CipherTag).

%
% Auxiliary tests
%
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

mult_test() ->
  ?assertEqual(<<16#A3B928F1CECEBA0F612BFEEBE5AEA0E1:128>>,
    mult(<<16#b96baa8c1c75a671bfb2d08d06be5f36:128>>, <<16#1e0889016f67601c8ebea4943bc23ad6:128>>)).

exor_test() ->
  ?assertEqual(<<2#0100000000011000:16>>, exor(<<2#1110101010110010:16>>, <<2#1010101010101010:16>>)).