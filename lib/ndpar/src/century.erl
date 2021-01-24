%%
%% @doc The problem of making a century is to list all the ways
%% the operations + and * can be inserted into the list of
%% digits `[1..9]' so as to make a total of 100.
%% Two such ways are:
%% ```
%% 100 = 12 + 34 + 5 * 6 + 7 + 8 + 9
%% 100 = 1 + 2 * 3 + 4 + 5 + 67 + 8 + 9
%% '''
%% Usage:
%% ```
%% 1> lists:map(fun(S) -> century:format(S) end, century:solutions(100, [1,2,3,4,5,6,7,8,9])).
%% 2> lists:map(fun(S) -> century:format(S) end, century:solutions(1000, [3,1,4,1,5,9,2,6,5,3,5,8,9,7])).
%% '''
%% @reference [B1] Chapter 6, pp. 33–40
%%
-module(century).
-author("Andrey Paramonov <github@ndpar.com>").

-export([solutions/2]).
-export([format/1]).

-import(lists, [concat/1, filter/2, foldr/3, map/2]).

-spec solutions(pos_integer(), [1..9]) -> [Solution] when
  Solution :: [Term],
  Term :: [Factor],
  Factor :: [1..9].

solutions(C, Digits) ->
  Fd = fun(X, ACC) -> expand(C, X, ACC) end,
  Evs = foldr(Fd, [], Digits),
  Ft = fun({_, B}) -> good(C, B) end,
  [X || {X, _} <- filter(Ft, Evs)].

expand(_, X, []) -> [{[[[X]]], {10, X, 1, 0}}];
expand(C, X, Evs) ->
  Filter = fun({_, B}) -> ok(C, B) end,
  F = fun(E) -> filter(Filter, glue(X, E)) end,
  concat(map(F, Evs)).

good(C, {_, F, T, E}) -> F * T + E =:= C.
ok(C, {_, F, T, E}) -> F * T + E =< C.

glue(X, {[[Xs | Xss] | Xsss], {K, F, T, E}}) -> [
  {[[[X | Xs] | Xss] | Xsss], {10 * K, K * X + F, T, E}},
  {[[[X], Xs | Xss] | Xsss], {10, X, F * T, E}},
  {[[[X]], [Xs | Xss] | Xsss], {10, X, 1, F * T + E}}
].

format(Solution) ->
  Terms = map(fun formatt/1, Solution),
  string:join(Terms, "+").

formatt(Term) ->
  Factors = map(fun formatf/1, Term),
  string:join(Factors, "*").

formatf(Factor) -> [X + 48 || X <- Factor].

%% =============================================================================
%% Unit tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

glue_test_() ->
  [
    ?_assertEqual(
      [
        {[[[2, 1]]], {100, 21, 1, 0}},
        {[[[2], [1]]], {10, 2, 1, 0}},
        {[[[2]], [[1]]], {10, 2, 1, 1}}
      ],
      glue(2, {[[[1]]], {10, 1, 1, 0}}))
  ].

expand_test_() ->
  E0 = [],
  E1 = expand(100, 1, E0),
  E2 = expand(100, 2, E1),
  [
    ?_assertEqual([{[[[1]]], {10, 1, 1, 0}}], E1),
    ?_assertEqual(
      [
        {[[[2, 1]]], {100, 21, 1, 0}},
        {[[[2], [1]]], {10, 2, 1, 0}},
        {[[[2]], [[1]]], {10, 2, 1, 1}}
      ],
      E2
    )
  ].

solutions_test_() ->
  [
    ?_assertEqual(
      [
        [[[1], [2], [3]]],     % 1 * 2 * 3
        [[[1]], [[2]], [[3]]]  % 1 + 2 + 3
      ],
      solutions(6, [1, 2, 3])
    ),
    ?_assertEqual(
      [
        [[[1], [2], [3]], [[4]], [[5]], [[6]], [[7]], [[8], [9]]],
        [[[1]], [[2]], [[3]], [[4]], [[5]], [[6]], [[7]], [[8], [9]]],
        [[[1], [2], [3], [4]], [[5]], [[6]], [[7], [8]], [[9]]],
        [[[1, 2]], [[3], [4]], [[5]], [[6]], [[7], [8]], [[9]]],
        [[[1]], [[2], [3]], [[4]], [[5]], [[6, 7]], [[8]], [[9]]],
        [[[1], [2]], [[3, 4]], [[5]], [[6], [7]], [[8]], [[9]]],
        [[[1, 2]], [[3, 4]], [[5], [6]], [[7]], [[8]], [[9]]]
      ],
      solutions(100, [1, 2, 3, 4, 5, 6, 7, 8, 9])
    ),
    ?_assertEqual(202, length(solutions(1000, [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7])))
  ].

format_test_() ->
  [
    ?_assertEqual(
      "1*2*3+4+5+6+7+8*9",
      format([[[1], [2], [3]], [[4]], [[5]], [[6]], [[7]], [[8], [9]]])
    )
  ].

-endif.
