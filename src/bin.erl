%
% J. Armstrong, Programming Erlang, Chapter 7
%
-module(bin).
-export([reverse_bytes/1, reverse_bits/1]).
-export([term_to_packet/1, packet_to_term/1]).


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


-include_lib("eunit/include/eunit.hrl").

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
