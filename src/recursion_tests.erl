%
% F.Cesarini & S.Thomson, Erlang Programming, p.84.
% Exercise 3-5: Manipulating lists
%
-module(recursion_tests).
-include_lib("eunit/include/eunit.hrl").

flatten_test() ->
    ?assertEqual([1,2,3,4,5,6], recursion:flatten([[1,[2,[3],[]]], [[[4]]], [5,6]])).
