%
% F.Cesarini & S.Thomson, Erlang Programming, p.169.
% Exercise 7-5: Binary Trees
%
-module(trees_test).
-export([test/0]).
-include("Trees.hrl").

test() ->
    sum_undefined_tree(),
    sum_balanced_tree(),
    sum_lined_tree(),
    max_undefined_tree(),
    max_balanced_tree(),
    max_lined_tree(),
    is_ordered_undefined_tree(),
    is_ordered_false(),
    is_ordered_true(),
    insert_into_undefined(),
    insert_into_left_tree(),
    insert_equal_into_right_tree(),
    insert_greater_into_right_tree(),
    ok.

sum_undefined_tree() ->
    0 = trees:sum(undefined).

sum_balanced_tree() ->
    Tree = #bintree{value = 5, left = #bintree{value = 6}, right = #bintree{value = 9}},
    20 = trees:sum(Tree).

sum_lined_tree() ->
    Tree = #bintree{value = 5, left = #bintree{value = 6, left = #bintree{value = 9}}},
    20 = trees:sum(Tree).

max_undefined_tree() ->
    0 = trees:max(undefined).

max_balanced_tree() ->
    Tree = #bintree{value = 5, left = #bintree{value = 6}, right = #bintree{value = 9}},
    9 = trees:max(Tree).

max_lined_tree() ->
    Tree = #bintree{value = 5, left = #bintree{value = 6, left = #bintree{value = 9}}},
    9 = trees:max(Tree).

is_ordered_undefined_tree() ->
    true = trees:is_ordered(undefined).

is_ordered_false() ->
    Tree = #bintree{value = 5, left = #bintree{value = 6}, right = #bintree{value = 9}},
    false = trees:is_ordered(Tree).

is_ordered_true() ->
    Tree = #bintree{value = 5, left = #bintree{value = 4}, right = #bintree{value = 6}},
    true = trees:is_ordered(Tree).

insert_into_undefined() ->
    #bintree{value = 5} = trees:insert(5, undefined).

insert_into_left_tree() ->
    Tree = #bintree{value = 5, left = #bintree{value = 4}, right = #bintree{value = 6}},
    #bintree{value = 5, left = #bintree{value = 4, left = #bintree{value = 3}}, right = #bintree{value = 6}} =
        trees:insert(3, Tree).

insert_equal_into_right_tree() ->
    Tree = #bintree{value = 5, left = #bintree{value = 4}, right = #bintree{value = 6}},
    #bintree{value = 5, left = #bintree{value = 4}, right = #bintree{value = 6, left = #bintree{value = 5}}} =
        trees:insert(5, Tree).

insert_greater_into_right_tree() ->
    Tree = #bintree{value = 5, left = #bintree{value = 4}, right = #bintree{value = 6}},
    #bintree{value = 5, left = #bintree{value = 4}, right = #bintree{value = 6, right = #bintree{value = 7}}} =
        trees:insert(7, Tree).
