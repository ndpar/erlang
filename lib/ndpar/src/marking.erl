%%
%% @doc Maximum marking problem.
%%
%% Algorithms for solving maximum marking problem in which
%% the marking criterion can be formulated as a regular expression.
%%
%% @reference [B1] Chapter 11, pp. 73–78
%%
-module(marking).
-author("Andrey Paramonov <github@ndpar.com>").

-import(lists, [foldl/3, split/2]).

-export([mnss/1]).

%%
%% @doc Computes maximum non-segment sum in linear time.
%%
%% A segment of a list is a contiguous subsequence,
%% while a non-segment is a subsequence that is not a segment.
%% There are no non-segments of a list with two or fewer elements.
%%
%% For example,
%% ```
%% [-4, -3, -7, 2, 1, -2, -1, -4]
%% '''
%% has maximum segment sum 3 (from the segment [2, 1]) and
%% maximum non-segment sum 2 (from the non-segment [2, 1, -1]).
%%
-spec mnss([integer()]) -> integer().

mnss(Ints) ->
  {L, R} = split(3, Ints),
  {_, _, _, Sum} = foldl(fun h/2, start(L), R),
  Sum.

start([X, Y, Z]) ->
  {0, lists:max([X + Y + Z, Y + Z, Z]), lists:max([X, X + Y, Y]), X + Z}.

h(X, {E, S, M, N}) ->
  {E, max(S, E) + X, max(M, S), max(N, max(N, M) + X)}.

%% =============================================================================
%% Unit tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

mnss_test_() ->
  [
    ?_assertEqual(2, mnss([-4, -3, -7, 2, 1, -2, -1, -4]))
  ].

-endif.
