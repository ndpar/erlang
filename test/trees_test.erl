%
% F.Cesarini & S.Thomson, Erlang Programming, p.169.
% Exercise 7-5: Binary Trees
%
-module(trees_test).
-include("Trees.hrl").
-include_lib("eunit/include/eunit.hrl").

sum_undefined_tree_test() ->
    ?assertEqual(0, trees:sum(undefined)).

sum_balanced_tree_test() ->
    Tree = #bintree{value = 5, left = #bintree{value = 6}, right = #bintree{value = 9}},
    ?assertEqual(20, trees:sum(Tree)).

sum_lined_tree_test() ->
    Tree = #bintree{value = 5, left = #bintree{value = 6, left = #bintree{value = 9}}},
    ?assertEqual(20, trees:sum(Tree)).

max_undefined_tree_test() ->
    ?assertEqual(0, trees:max(undefined)).

max_balanced_tree_test() ->
    Tree = #bintree{value = 5, left = #bintree{value = 6}, right = #bintree{value = 9}},
    ?assertEqual(9, trees:max(Tree)).

max_lined_tree_test() ->
    Tree = #bintree{value = 5, left = #bintree{value = 6, left = #bintree{value = 9}}},
    ?assertEqual(9, trees:max(Tree)).

is_ordered_undefined_tree_test() ->
    ?assert(trees:is_ordered(undefined)).

is_ordered_false_test() ->
    Tree = #bintree{value = 5, left = #bintree{value = 6}, right = #bintree{value = 9}},
    ?assertNot(trees:is_ordered(Tree)).

is_ordered_true_test() ->
    Tree = #bintree{value = 5, left = #bintree{value = 4}, right = #bintree{value = 6}},
    ?assert(trees:is_ordered(Tree)).

insert_into_undefined_test() ->
    ?assertEqual(#bintree{value = 5}, trees:insert(5, undefined)).

insert_into_left_tree_test() ->
    Tree = #bintree{value = 5, left = #bintree{value = 4}, right = #bintree{value = 6}},
    ?assertEqual(#bintree{value = 5, left = #bintree{value = 4, left = #bintree{value = 3}}, right = #bintree{value = 6}},
        trees:insert(3, Tree)).

insert_equal_into_right_tree_test() ->
    Tree = #bintree{value = 5, left = #bintree{value = 4}, right = #bintree{value = 6}},
    ?assertEqual(#bintree{value = 5, left = #bintree{value = 4}, right = #bintree{value = 6, left = #bintree{value = 5}}},
        trees:insert(5, Tree)).

insert_greater_into_right_tree_test() ->
    Tree = #bintree{value = 5, left = #bintree{value = 4}, right = #bintree{value = 6}},
    ?assertEqual(#bintree{value = 5, left = #bintree{value = 4}, right = #bintree{value = 6, right = #bintree{value = 7}}},
        trees:insert(7, Tree)).
