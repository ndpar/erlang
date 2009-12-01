%
% F.Cesarini & S.Thomson, Erlang Programming, p.211.
% Exercise 9-1: Higher Order Functions
%
-module(hof).
-export([print_integers/2]).

print_integers(From, To) ->
    lists:foreach(fun(X) -> io:format("~p~n", [X]) end, lists:seq(From, To)).

squares(List) -> [X*X || X <- List, is_integer(X)].

intersection(L1, L2) -> [X || X <- L1, Y <- L2, X =:= Y].

diff(L1, L2) -> [X || X <- L1, not lists:member(X, L2)].
sym_diff(L1, L2) -> diff(L1, L2) ++ diff(L2, L1).

zip(L1, L2) -> zip([], L1, L2).
zip(Acc, _, []) -> Acc;
zip(Acc, [], _) -> Acc;
zip(Acc, [X|Xs], [Y|Ys]) -> zip(Acc ++ [{X,Y}], Xs, Ys).

zipwith(Fun, L1, L2) -> lists:map(Fun, zip(L1, L2)).


-include_lib("eunit/include/eunit.hrl").

squares_test() ->
    ?assertEqual([1,10000,81], squares([1,hello,100,boo,"boo",9])).

intersection_test() ->
    ?assertEqual([4,5], intersection([1,2,3,4,5], [4,5,6,7,8])).

sym_diff_test() ->
    ?assertEqual([1,2,3,6,7,8], sym_diff([1,2,3,4,5], [4,5,6,7,8])).

zip_fails_on_lists_with_different_length_test() ->
    ?assertError(function_clause, lists:zip([1,2],[3,4,5])).

zip_left_list_test() ->
    ?assertEqual([{1,3},{2,4}], zip([1,2],[3,4,5])).

zip_right_list_test() ->
    ?assertEqual([{1,4},{2,5}], zip([1,2,3],[4,5])).

zipwith_test() ->
    Add = fun({X,Y}) -> X + Y end,
    ?assertEqual([4,6], zipwith(Add, [1,2],[3,4,5])).
