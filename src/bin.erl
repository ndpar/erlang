%%
%% @doc Various functions to work on binaries.
%%
-module(bin).
-export([integer_to_bitstring/1]).
-export([lxor/1, lxor/2]).
-export([bin_to_hexstr/1, hexstr_to_bin/1]).
-export([reverse_bytes/1, reverse_bits/1]).
-export([term_to_packet/1, packet_to_term/1]).


%%
%% @doc Returns a bitstring representation of the
%% given integer with leading zeroes removed.
%%
%% E.g. `<<2#1100:4>> = bin:integer_to_bitstring(12).'
%%
%% @see binary:encode_unsigned/1.
%%
-spec integer_to_bitstring(non_neg_integer()) -> binary().

integer_to_bitstring(Int) -> trim_bitstring(binary:encode_unsigned(Int)).

trim_bitstring(<<>>) -> <<>>;
trim_bitstring(<<1:1, _/bitstring>> = B) -> B;
trim_bitstring(<<0:1, B/bitstring>>) -> trim_bitstring(B).

%%
%% @doc Left XOR.
%%
%% E.g. `<<16#B9F9:16>> = bin:lxor(<<16#ABCDEF:24>>, <<16#1234:16>>).'
%%
-spec lxor(binary(), binary()) -> binary().

lxor(X, Y) ->
  Length = min(size(X), size(Y)),
  crypto:exor(binary:part(X, 0, Length), binary:part(Y, 0, Length)).

%%
%% @doc Left XOR.
%% Can be used from escript or shell.
%%
-spec lxor([string()]) -> string().

lxor([H | T]) ->
  F = fun(X, Acc) -> lxor(hexstr_to_bin(X), Acc) end,
  bin_to_hexstr(lists:foldl(F, hexstr_to_bin(H), T)).

%%
%% @doc Converts hexadecimal string to binary.
%% The string length is expected to be even.
%%
-spec hexstr_to_bin(string()) -> binary().

hexstr_to_bin(String) ->
  0 = length(String) rem 2,
  <<1:8, Bin/binary>> = binary:encode_unsigned(list_to_integer([$1 | String], 16)),
  Bin.

%%
%% @doc Converts binary to hexadecimal string.
%%
-spec bin_to_hexstr(binary()) -> string().

bin_to_hexstr(Bin) ->
  binary_to_list(<<<<Y>> || <<X:4>> <= Bin, Y <- integer_to_list(X, 16)>>).

%
% [A2] Chapter 7
%

reverse_bytes(B) ->
  list_to_binary(lists:reverse(binary_to_list(B))).

reverse_bits(B) -> reverse_bits(B, <<>>).

reverse_bits(<<>>, Acc) -> Acc;
reverse_bits(<<X:1, Rest/bitstring>>, Acc) ->
  reverse_bits(Rest, <<X:1, Acc/bitstring>>).


term_to_packet(Term) ->
  <<Header:32, Data/binary>> = term_to_binary(Term),
  Length = size(Data),
  <<Header:32, Length:32, Data/binary>>.

packet_to_term(<<Header:32, Length:32, Data/binary>>) ->
  binary_to_term(<<Header:32, Data:Length/binary>>).


%% =============================================================================
%% Unit tests
%% =============================================================================

-include_lib("eunit/include/eunit.hrl").

integer_to_bitstring_test() ->
  ?assertEqual(<<>>, integer_to_bitstring(0)),
  ?assertEqual(<<2#10001100:8, 0:2>>, integer_to_bitstring(560)).

hexstr_to_bin_test() ->
  ?assertEqual(<<0, 18, 52, 86, 120, 144, 171, 205, 239>>, hexstr_to_bin("001234567890ABCDEF")).

bin_to_hexstr_test() ->
  ?assertEqual("001234567890ABCDEF", bin_to_hexstr(<<0, 18, 52, 86, 120, 144, 171, 205, 239>>)).

lxor_test() ->
  ?assertEqual("94D5513AC50DFF660F9FDE299DF35718",
    lxor(["E20106D7CD0DF0761E8DCD3D88E54000", "76D457ED08000F101112131415161718"])),
  ?assertEqual("94D5513AC50D",
    lxor(["E20106D7CD0DF0761E8DCD3D88E54000", "76D457ED0800"])).


reverse_bytes_test() ->
  ?assertEqual(<<12, 34, 56, 78>>, reverse_bytes(<<78, 56, 34, 12>>)).

reverse_bits_test() ->
  ?assertEqual(<<2#1001100100010111:16>>, reverse_bits(<<2#1110100010011001:16>>)).

term_to_packet_test() ->
  ?assertEqual(<<131, 104, 3, 100, 0, 0, 0, 17, 0, 4, 97, 116, 111, 109, 97, 5, 107, 0, 6, 115, 116, 114, 105, 110, 103>>,
    term_to_packet({atom, 5, "string"})).

packet_to_term_test() ->
  ?assertEqual({atom, 5, "string"},
    packet_to_term(<<131, 104, 3, 100, 0, 0, 0, 17, 0, 4, 97, 116, 111, 109, 97, 5, 107, 0, 6, 115, 116, 114, 105, 110, 103, "garbage">>)).
