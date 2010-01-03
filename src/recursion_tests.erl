%
% F.Cesarini & S.Thomson, Erlang Programming, p.420.
% Exercise 19-1: Testing sequential functions
%
-module(recursion_tests).
-include_lib("eunit/include/eunit.hrl").

bump_test() ->
    ?assertEqual([2,3,4], recursion:bump([1,2,3])).

average_test_() -> [
    ?_assertEqual(2.5, recursion:average([1,2,3,4])),
    ?_assertEqual(2.0, recursion:average([1,2,3]))
].

sum1_test() ->
    ?assertEqual(10, recursion:sum(4)).

sum2_test() ->
    ?assertEqual(12, recursion:sum(3,5)).

create1_test() ->
    ?assertEqual([1,2,3,4], recursion:create(4)).

create2_test() ->
    ?assertEqual([3,4,5], recursion:create(3,5)).

filter_test() ->
    ?assertEqual([1,2,3], recursion:filter([1,2,3,4,5], 3)).

reverse_test() ->
    ?assertEqual([5,4,3,2,1], recursion:reverse([1,2,3,4,5])).

concatenate_test() ->
    ?assertEqual([1,2,3,4,5,6], recursion:concatenate([[1,2,3], [4], [5,6]])).

flatten_test() ->
    ?assertEqual([1,2,3,4,5,6], recursion:flatten([[1,[2,[3],[]]], [[[4]]], [5,6]])).

split_test_() -> [
    ?_assertEqual({[1,3], [2,4]}, recursion:split([1,2,3,4])),
    ?_assertEqual({[1,3], [2]}, recursion:split([1,2,3]))
].

merge_test_() -> [
    ?_assertEqual([1,2,3,4], recursion:merge([1,3], [2,4])),
    ?_assertEqual([1,2,3], recursion:merge([1,3], [2]))
].

qsort_test() ->
    ?assertEqual([1,2,3,4,5,6], recursion:qsort([1,6,2,5,3,4])).

msort_test_() -> [
    ?_assertEqual([], recursion:msort([])),
    ?_assertEqual([1], recursion:msort([1])),
    ?_assertEqual([3,9,10,27,38,43,82], recursion:msort([38,27,43,3,9,82,10]))
].
