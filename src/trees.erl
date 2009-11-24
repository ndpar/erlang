%
% F.Cesarini & S.Thomson, Erlang Programming, p.169.
% Exercise 7-5: Binary Trees
%
-module(trees).
-export([sum/1, max/1, is_ordered/1, insert/2]).
-include("Trees.hrl").

sum(Tree) ->
    case Tree of
        undefined ->
            0;
        #bintree{value = Val, left = L, right = R} ->
            Val + sum(L) + sum(R)
    end.

max(Tree) -> max_help(0, Tree).

max_help(Result, Tree) ->
    case Tree of
        undefined ->
            Result;
        #bintree{value = Val, left = L, right = R} ->
            NewResult = erlang:max(Result, Val),
            erlang:max(max_help(NewResult, L), max_help(NewResult, R))
    end.

is_ordered(Tree) ->
    case Tree of
        undefined ->
            true;
        #bintree{value = Val, left = L, right = R} ->
            (max(L) < Val) and (Val =< max(R))
    end.

insert(Value, Tree) ->
    case Tree of
        undefined ->
            #bintree{value = Value};
        #bintree{value = Val, left = L, right = R} when Value < Val ->
            #bintree{value = Val, left = insert(Value, L), right = R};
        #bintree{value = Val, left = L, right = R} ->
            #bintree{value = Val, left = L, right = insert(Value, R)}
    end.
